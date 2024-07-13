package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim.{SimBitVectorPimper, SimClockDomainHandlePimper, SimTimeout}
import spinal.lib._
import spinal.lib.bus.simple._
import spinalextras.lib.misc.AutoInterconnect
import spinalextras.lib.{Config, HardwareMemory, Memories, MemoryRequirement, MemoryRequirementBits, PipelinedMemoryBusMemory, StackedHardwareMemory, WideHardwareMemory}


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
      return null
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

case class MemoryTestBench(cfg : PipelinedMemoryBusConfig, unique_name : Boolean = false) extends Component {
  val io = new Bundle {
    val bus = master(PipelinedMemoryBus(cfg))

    val valid = out(Bool())
    val valid_count = out(UInt(32 bits))
  }

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

  val iterationCount = 1 << 8
  val writeCnt = Counter(iterationCount - 1) init (0)
  val read_cnt = Counter(iterationCount - 2) init (0)
  val readMode = Reg(Bool()) init (False)

  val readAddress, writeAddress, responseAddress = Reg(UInt(cfg.addressWidth bits)) init (0)

  val dataWidth = cfg.dataWidth
  val incPerOp = dataWidth / 8
  //cmd_stream.address := (0x000 + incPerOp * writeCnt.value).resized
  cmd_stream.data := AddressHash(cmd_stream.address, dataWidth).resized
  val cmdHash = AddressMaskHash(cmd_stream.address, dataWidth)
  if(cmdHash != null) {
    cmd_stream.mask := cmdHash.resized
  } else {
    cmd_stream.mask.setAll()
  }

  cmd_stream.valid := True
  cmd_stream.write := ~readMode
  when(cmd_stream.write) {
    cmd_stream.address := writeAddress.resized
  } otherwise {
    cmd_stream.address := readAddress.resized
  }
  when(cmd_stream.fire) {
    when(cmd_stream.write) {
      writeAddress := writeAddress + incPerOp
    } otherwise {
      readAddress := readAddress + incPerOp
    }
    writeCnt.increment()
    read_cnt.increment()
    when((readMode && read_cnt.willOverflow) || (~readMode && writeCnt.willOverflow)) {
      readMode := ~readMode
      writeCnt.clear()
      read_cnt.clear()
    }
  }

  io.valid_count.setAsReg() init (0)

  io.valid.setAsReg() init (True)

  val byteMask, expected_value = Bits(dataWidth bits)
  val byteMaskVal = ByteMaskToBitMask(AddressMaskHash(responseAddress, dataWidth))
  if(byteMaskVal != null) {
    byteMask := byteMaskVal
  } else {
    byteMask.setAll()
  }

  expected_value := AddressHash(responseAddress, dataWidth).resized
  val outstanding_count = CounterUpDown((1L << 32), incWhen = io.bus.cmd.fire && ~io.bus.cmd.write, decWhen = io.bus.rsp.fire)
  assert(~outstanding_count.msb)

  when(access.rsp.valid) {
    responseAddress := responseAddress + incPerOp
    val valid_value = ((expected_value).asBits & byteMask) === (access.rsp.data & byteMask)
    io.valid := (io.valid && valid_value)// && ~timeout_counter.willOverflowIfInc)
    when(~valid_value) {
      io.valid_count := 0
    }

    when(!valid_value) {
      report(Seq("Invalid value found given ", access.rsp.data, " vs expected ", expected_value, " at ", responseAddress, " for ", ClockDomain.current.frequency.getValue.decomposeString))
      assert(False)
    } otherwise {
      io.valid_count := io.valid_count + 1
    }
  }


  if(unique_name) {
    io.valid_count.setName(s"valid_count${dataWidth}_${(ClockDomain.current.frequency.getValue / 1e6).toDouble.round.toInt}")
    io.valid.setName(s"valid${dataWidth}_${(ClockDomain.current.frequency.getValue / 1e6).toDouble.round.toInt}")
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
            MemoryTestBench(mem.io.bus.config),
            mem
          ).toIterator
        }
      )
    ) { dut =>
      SimTimeout(5000 us)
      dut.clockDomain.forkStimulus(100 MHz)
      val valid_count = dut.getAllIo.find(_.name == "io_valid_count").get.asInstanceOf[UInt]

      while(valid_count.toBigInt < 2 * reqs.num_elements) {
        dut.clockDomain.waitSampling(100)
      }

    }
  }

  for(reqs <- Seq(
    new MemoryRequirementBits(19, 1000, 1, 0, 0),
    new MemoryRequirementBits(19, 1000, 0, 1, 1),

    new MemoryRequirementBits(8, 1000, 1, 0, 0),
    new MemoryRequirementBits(32, 1000, 1, 0, 0),
    new MemoryRequirementBits(32, 1000, 0, 1, 1),

    new MemoryRequirementBits(95, 1000, 0, 1, 1),
    new MemoryRequirementBits(96, 1000, 0, 1, 1)
  )) {
    test(s"MemBased_${reqs.toString}") {
      doTest(reqs)
    }
  }

  for(reqs <- Seq(
    //(new MemoryRequirementBits(19, 1000, 1, 0, 0), 19, 10000),
    (new MemoryRequirementBits(32, 1000, 1, 0, 0), 32),
    (new MemoryRequirementBits(64, 1000, 1, 0, 0), 32),

    (new MemoryRequirementBits(64, 1000, 0, 1, 1), 32),

    (new MemoryRequirementBits(95, 1000, 1, 0, 0), 32),
    (new MemoryRequirementBits(95, 1000, 0, 1, 1), 32),
  )) {
    test(s"WidenTest_${reqs._1.toString}_${reqs._2}") {
      doTest(reqs._1, factory = (freqs, tech) => new WideHardwareMemory[Bits](freqs, () => Memories(reqs._1.copy(dataType = Bits(reqs._2 bits)))))
    }
  }

  for(reqs <- Seq(
    (new MemoryRequirementBits(32, 1000, 1, 0, 0), 32, 1 << 9),

    (new MemoryRequirementBits(95, 1000, 1, 0, 0), 32, 1 << 8),

    (new MemoryRequirementBits(32, 1000, 1, 0, 0), 16, 1 << 9),
    (new MemoryRequirementBits(32, 1000, 0, 1, 1), 16, 1 << 9)
  )) {
    test(s"StackedTest_${reqs._1.toString}_${reqs._2}_${reqs._3}") {
      doTest(reqs._1, factory = (freqs, tech) => new StackedHardwareMemory[Bits](freqs, () => Memories(reqs._1.copy(dataType = Bits(reqs._2 bits), num_elements = reqs._3))))
    }
  }



}
