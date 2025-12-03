package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim.{SimBitVectorPimper, SimBoolPimper, SimClockDomainHandlePimper, SimTimeout}
import spinal.lib._
import spinal.lib.bus.simple._
import spinalextras.lib.blackbox.lattice.lifcl.{DPSC512K_Mem, GSR, OSCD, OSCDConfig, PDPSC512K_Mem}
import spinalextras.lib.bus.PipelinedMemoryBusCmdExt
import spinalextras.lib.logging.{FlowLogger, GlobalLogger, SignalLogger}
import spinalextras.lib.memory._
import spinalextras.lib.misc.{AutoInterconnect, ClockSpecification}
import spinalextras.lib._

import scala.language.{existentials, postfixOps}


class AddressMaskHash(width : Int = 12, dataWidth : Int = 32) extends Component {
  val io = new Bundle {
    val input = in(UInt(width bits))
    val output = out(Bits((dataWidth/8) bits))
  }
  val that = io.input
  //io.output := that.subdivideIn(4 bits, strict = false).fold(U(0, 4 bit))((a,b) => a ^ b.resize(4 bits)).asBits
  io.output.setAll()
}

class AddressHash(width : Int = 12, dataWidth : Int = 32) extends Component {
  val io = new Bundle {
    val input = in(UInt(width bits))
    val output = out(Bits(dataWidth bits))
  }
  val that = io.input
  val startMask = that.subdivideIn(8 bits, strict = false).fold(U(0, 8 bit))((a,b) => a ^ b.resize(8 bits))
  io.output := Vec(Seq(0, 0x12, 0x55, 0xab, 0x34, 0xcd, 0xef, 0xff).map(v => U(v, 8 bits) ^ startMask)).asBits.resized
}
object AddressMaskHash {
  def apply(that : UInt, dw : Int): Bits = {
    if(dw % 8 != 0) {
      return ~B(0, (that.getWidth / 8) bits)
    }
    val addressHash = new AddressMaskHash(that.getWidth, dw)
    addressHash.io.input := that
    addressHash.io.output
  }
}
object ByteMaskToBitMask {
  def apply(that : Bits): Bits = {
    if(that == null) {
      return null
    }
    that.subdivideIn(1 bits).map(x => S(0, 8 bits) - x.resize(8 bits).asSInt).asBits()
  }
}
object AddressHash {
  def apply(that : UInt, dw : Int) : Bits = {
    val addressHash = new AddressHash(that.getWidth, dw)
    addressHash.io.input := that
    addressHash.io.output
  }
}



class MemoryTestBench(cfg : PipelinedMemoryBusConfig, unique_name : Boolean = false, latency : Int = -1,
                           address_mask : BigInt = 0xffffffffL,
                           writesPerIteration : BigInt,
                           readsPerIteration : BigInt,
                           address_to_data : (UInt, Int) => Bits,
                           address_to_data_mask : Option[(UInt, Int) => Bits] = None
                          ) extends Component {
  val io = new Bundle {
    val bus = master(PipelinedMemoryBus(cfg))

    val valid = out(Bool())
    val valid_count = out(UInt(32 bits))
  }
  noIoPrefix()

  val access = io.bus

  val cmd_stream = cloneOf(access.cmd)
  access.cmd <> cmd_stream.stage()

  val primed = Reg(Bool()) init (False) setWhen (cmd_stream.fire)
  val timeout_counter = Counter(32)

  when(~timeout_counter.willOverflowIfInc && ~cmd_stream.fire) {
    when(primed) {
      timeout_counter.increment()
    }
  } elsewhen (cmd_stream.fire) {
    timeout_counter.clear()
  }

  require(writesPerIteration >= readsPerIteration)
  val writeCnt = Counter(writesPerIteration) init (0)
  val read_cnt = Counter(readsPerIteration) init (0)
  val readMode = Reg(Bool()) init (False)

  val readAddress, writeAddress, responseAddress, lastGoodResponse = Reg(UInt(cfg.addressWidth bits)) init (0)

  val dataWidth = cfg.dataWidth
  //require(dataWidth % 8 == 0)
  val incPerOp = 1 << log2Up((dataWidth / 8.0).ceil.toInt)
  //cmd_stream.address := (0x000 + incPerOp * writeCnt.value).resized
  cmd_stream.data := address_to_data(cmd_stream.address, dataWidth).resized
  val cmdHash = address_to_data_mask.map(_(cmd_stream.address, dataWidth)).orNull
  if(cmdHash != null) {
    cmd_stream.mask := cmdHash.resized
  } else {
    cmd_stream.mask.setAll()
  }

  cmd_stream.valid := True
  cmd_stream.write := ~readMode
  when(cmd_stream.write) {
    cmd_stream.payload.assignByteAddress((writeAddress.resized & address_mask).resized)
  } otherwise {
    cmd_stream.payload.assignByteAddress((readAddress.resized & address_mask).resized)
  }
  when(cmd_stream.fire) {
    when(cmd_stream.write) {
      writeAddress := ((writeAddress + incPerOp).resized & address_mask).resized
    } otherwise {
      readAddress := ((readAddress + incPerOp).resized & address_mask).resized
    }
    writeCnt.increment()
    read_cnt.increment()
    when((readMode && read_cnt.willOverflow) || (~readMode && writeCnt.willOverflow)) {
      readMode := ~readMode
      writeCnt.clear()
      read_cnt.clear()
    }
  }

  val hasExpectedLatency = RegInit(True).allowUnsetRegToAvoidLatch()
  if(latency >= 0) {
    var expected_read_stream = cmd_stream.toFlowFire.map(_.address).throwWhen(cmd_stream.write)
    for(i <- 0 until (latency + 1)) {
      expected_read_stream = expected_read_stream.stage()
    }
    expected_read_stream.setName(s"expected_read_stream_${latency}")

    hasExpectedLatency := expected_read_stream.valid === access.rsp.valid
    assert(expected_read_stream.valid === access.rsp.valid, "Memory latency isn't what was expected")
  }

  io.valid_count.setAsReg() init (0)

  io.valid.setAsReg() init (True)

  val bitMask, expected_value = Bits(dataWidth bits)
  val byteMask = address_to_data_mask.map(_(responseAddress, dataWidth)).getOrElse(~B(0, (dataWidth / 8) bits))
  val bitMaskVal = if (byteMask != null && byteMask.getBitsWidth > 0) ByteMaskToBitMask(byteMask) else null
  if(bitMaskVal != null) {
    bitMask := bitMaskVal.resized
  } else {
    bitMask.setAll()
  }

  expected_value := address_to_data(responseAddress, dataWidth).resized

  val outstanding_count = CounterUpDown((1L << 32), incWhen = io.bus.cmd.fire && ~io.bus.cmd.write, decWhen = io.bus.rsp.fire)
  assert(~outstanding_count.msb, "~outstanding_count.msb")

  val count_vals = new Bundle {
    val total_valid, total_invalid = UInt(32 bits)
    val timeout_counts = UInt(32 bits)
  }.setAsReg()

  val read_timeout = Timeout(writesPerIteration * 16)

  val eval_flow = access.rsp
    .map(x => TupleBundle(x.data & bitMask, expected_value.asBits & bitMask, responseAddress, byteMask))
    .stage()
    .map(x => TupleBundle(x._1 === x._2, x._1, x._2, x._3, x._4))
    .stage()

  when(access.rsp.fire) {
    responseAddress := ((responseAddress + incPerOp).resized & address_mask).resized
  }

  //val mem_config = HardwareMemoryReadWriteConfig(cmd_stream.config.copy(dataWidth = 32))
  val invalid_data = Flow(PipelinedMemoryBusCmd(cmd_stream.config.copy(dataWidth = 32)))
  invalid_data.valid := False
  invalid_data.assignDontCareToUnasigned()

  when(eval_flow.fire) {
    read_timeout.clear()
    val (valid_value, given_data, expected_value, responseAddress, byteMask) =
      (eval_flow.payload._1, eval_flow.payload._2, eval_flow.payload._3, eval_flow.payload._4, eval_flow.payload._5)

    invalid_data.payload.data := (given_data ^ expected_value).resized
    invalid_data.payload.address := responseAddress
    invalid_data.payload.mask := byteMask.resized

    io.valid := (io.valid && valid_value && hasExpectedLatency)// && ~timeout_counter.willOverflowIfInc)
    when(~valid_value) {
      io.valid_count := 0
      count_vals.total_invalid := count_vals.total_invalid + 1
    }


    when(!valid_value) {
      report(Seq("Invalid value found given ", given_data, " vs expected ", expected_value, " at ", responseAddress, " for ", ClockDomain.current.frequency.getValue.decomposeString))
      assert(False, "Invalid value found")
      invalid_data.valid := True
    } otherwise {
      lastGoodResponse := responseAddress
      io.valid_count := io.valid_count + 1
      count_vals.total_valid := count_vals.total_valid + 1
    }
  }

  when(read_timeout) {
    read_timeout.clear()
    count_vals.timeout_counts := count_vals.timeout_counts + 1
    //report(Seq("timeout found for", ClockDomain.current.frequency.getValue.decomposeString))
    //assert(False, "read timeout")
  }

  val timer = Timeout(1000 ms)
  timer.clearWhen(timer.state)

  GlobalLogger(
    tags = Set("meta"),
    SignalLogger.concat(100 ms, "counts", count_vals.total_invalid, count_vals.total_valid),
    SignalLogger.concat(100 ms, "error_counts", lastGoodResponse, count_vals.timeout_counts),
    FlowLogger.flows(invalid_data)
  )

  if(unique_name) {
    io.valid_count.setName(s"valid_count${dataWidth}_${(ClockDomain.current.frequency.getValue / 1e6).toDouble.round.toInt}")
    io.valid.setName(s"valid${dataWidth}_${(ClockDomain.current.frequency.getValue / 1e6).toDouble.round.toInt}")
  }
}

object MemoryTestBench {
  def apply(
             cfg : PipelinedMemoryBusConfig, unique_name : Boolean = false, latency : Int = -1, test_masking : Boolean = true,
             address_mask : BigInt = 0xffffffffL): MemoryTestBench = {
    val iterationCount = 1 << 8
    new MemoryTestBench(cfg, unique_name, latency, address_mask,
      iterationCount - 1, iterationCount - 2 ,
      address_to_data = AddressHash.apply,
      address_to_data_mask = if (test_masking) Some(AddressMaskHash.apply) else None)
  }

  def linear(
             cfg : PipelinedMemoryBusConfig, unique_name : Boolean = false, latency : Int = -1,
             address_mask : BigInt = 0xffffffffL): MemoryTestBench = {
    new MemoryTestBench(cfg, unique_name, latency, address_mask,
      512, 512,
      address_to_data = (address : UInt, dw : Int) => address.asBits)
  }

  def constant(
              cfg : PipelinedMemoryBusConfig, constant : BigInt, unique_name : Boolean = false, latency : Int = -1,
              address_mask : BigInt = 0xffffffffL): MemoryTestBench = {
    new MemoryTestBench(cfg, unique_name, latency, address_mask,
      512, 512,
      address_to_data = (address : UInt, dw : Int) => B(constant, dw bits))
  }
}

class MemMemoryTest extends AnyFunSuite {
  def doTest[T <: Data](reqs : MemoryRequirement[T], technologyKind : MemTechnologyKind = auto, factory : (MemoryRequirement[T], MemTechnologyKind) => HardwareMemory[T] = Memories.apply[T] _): Unit = {
    Config.sim.doSim(
      AutoInterconnect(
        s"MemMemoryTest_${reqs.toString}",
        () => {
          val mem = PipelinedMemoryBusMemory(reqs = reqs, technologyKind = technologyKind, factory = factory)
          Seq(
            MemoryTestBench(mem.io.bus.config, latency = mem.latency),
            mem
          ).toIterator
        }
      )
    ) { dut =>
      SimTimeout(5000 us)
      dut.clockDomain.forkStimulus(100 MHz)
      dut.clockDomain.waitSampling(10)

      val valid_count = dut.getAllIo.find(_.name == "valid_count").get.asInstanceOf[UInt]
      val valid = dut.getAllIo.find(_.name == "valid").get.asInstanceOf[Bool]

      while(valid_count.toBigInt < 2 * reqs.num_elements) {
        println(s"Valids: ${valid_count.toBigInt} ${valid.toBoolean}")
        dut.clockDomain.waitSampling(100)
        assert(valid.toBoolean)
      }

    }
  }

  for(reqs <- Seq(
    new MemoryRequirementBits(19, 1024, 1, 0, 0),
    new MemoryRequirementBits(19, 1024, 0, 1, 1),

    new MemoryRequirementBits(8, 1024, 1, 0, 0),
    new MemoryRequirementBits(32, 1024, 1, 0, 0),
    new MemoryRequirementBits(32, 1024, 0, 1, 1),

    new MemoryRequirementBits(95, 1024, 0, 1, 1),
    new MemoryRequirementBits(96, 1024, 0, 1, 1)
  )) {
    test(s"MemBased_${reqs.toString}") {
      doTest(reqs)
    }
  }

  for(reqs <- Seq(
    //(new MemoryRequirementBits(19, 1000, 1, 0, 0), 19, 10000),
    (new MemoryRequirementBits(32, 1024, 1, 0, 0), 32),
    (new MemoryRequirementBits(64, 1024, 1, 0, 0), 32),

    (new MemoryRequirementBits(64, 1024, 0, 1, 1), 32),

    (new MemoryRequirementBits(95, 1024, 1, 0, 0), 32),
    (new MemoryRequirementBits(95, 1024, 0, 1, 1), 32),
  )) {
    test(s"WidenTest_${reqs._1.toString}_${reqs._2}") {
      doTest(reqs._1, factory = (freqs, tech) => new WideHardwareMemory[Bits](freqs, () => Memories(reqs._1.copy(dataType = Bits(reqs._2 bits)))))
    }
  }

  for(reqs <- Seq(
    (new MemoryRequirementBits(95, 1 << 14, 0, 1, 1), 32, 1 << 14),
    (new MemoryRequirementBits(95, 1 << 14, 1, 0, 0), 32, 1 << 14),
    (new MemoryRequirementBits(32, 1<<9, 1, 0, 0), 32, 1 << 9),

    (new MemoryRequirementBits(95, 1<<8, 1, 0, 0), 32, 1 << 8),

    (new MemoryRequirementBits(32, 1<<9, 1, 0, 0), 16, 1 << 9),
    (new MemoryRequirementBits(32, 1<<9, 0, 1, 1), 16, 1 << 9)
  )) {
    test(s"StackedTest_${reqs._1.toString}_${reqs._2}_${reqs._3}") {
      doTest(reqs._1, factory = (freqs, tech) => new StackedHardwareMemory[Bits](freqs, () => Memories(reqs._1.copy(dataType = Bits(32 bits), num_elements = reqs._3))))
    }
  }

}


case class LatticeMemoryTest() extends Component {
  val GSR_INST = GSR.no_op()
  var osc = new ResetArea(False, false) {
    val osc = new OSCD(OSCDConfig.create(ClockSpecification(45 MHz, tolerance = .1)))
  }.osc

  val rst_counter = new ClockingArea(new ClockDomain(clock = osc.io.HFCLKOUT, config = ClockDomain.current.config.copy(resetKind = BOOT))) {
    val rst_counter = new Timeout(128)
  }.rst_counter

  new ClockingArea(osc.hf_clk().get.copy(reset = !rst_counter)) {
    for(latency <- Seq(1, 2)) {
      val mem = new PDPSC512K_Mem(latency)
      val tb = MemoryTestBench(mem.config, latency = mem.latency)
      val pmbs = mem.pmbs()
      tb.io.bus <> pmbs.head
    }
  }

}

case class LatticeFifoTest() extends Component {
  val GSR_INST = GSR.no_op()
  var osc = new ClockingArea(new ClockDomain(False, null, config = ClockDomain.current.config.copy(resetKind = BOOT))) {
    val osc = new OSCD(OSCDConfig.create(ClockSpecification(45 MHz, tolerance = .1)))
  }.osc

  val rst_counter = new ClockingArea(new ClockDomain(clock = osc.io.HFCLKOUT, config = ClockDomain.current.config.copy(resetKind = BOOT))) {
    val rst_counter = new Timeout(128)
  }.rst_counter

  val reset = CombInit(!rst_counter.state)
  new ClockingArea(osc.hf_clk().get.copy(reset = reset)) {
    val mem = new MemoryBackedFifo(Bits(33 bits), 1 << 12, latencyRange = (2, 2))//, mem_factory = (r : MemoryRequirement[Bits]) => new PDPSC512K_Mem())
    val tb = FifoTestBench(mem.dataType)
    mem.io.push <> tb.io.push
    mem.io.pop <> tb.io.pop
    mem.io.flush := False
  }
}

object LatticeMemoryTest extends App {
  Config.spinalConfig.copy(device = Device("lattice", "lifcl")).generateVerilog(
    new LatticeMemoryTest()
  )
}
object LatticeFifoTest extends App {
  Config.spinalConfig.copy(device = Device("lattice", "lifcl")).generateVerilog(
    new LatticeFifoTest()
  )
}