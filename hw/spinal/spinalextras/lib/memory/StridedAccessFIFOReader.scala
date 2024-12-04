package spinalextras.lib.memory

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim.{SimBaseTypePimper, SimBoolPimper, SimClockDomainHandlePimper, SimTimeout, simRandom}
import spinal.core._
import spinal.lib.bus.regif.BusIf
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusConfig}
import spinal.lib.sim.StreamMonitor
import spinal.lib.{Counter, CounterUpDown, Flow, Fragment, StreamFifo, StreamJoin, master}
import spinalextras.lib.Config
import spinalextras.lib.bus.PipelineMemoryGlobalBus
import spinalextras.lib.bus.simple.SimpleMemoryProvider
import spinalextras.lib.logging.PipelinedMemoryBusLogger
import spinalextras.lib.misc.{CounterTools, CounterUpDownUneven, PipelinedMemoryBusBuffered, RegisterTools, StreamTools}
import spinalextras.lib.testing.test_funcs
import spinal.lib.Stream

import scala.collection.mutable
import scala.language.postfixOps

case class StridedAccessFIFOReader[T <: Data](
    dataType: HardType[T],
    /** Depth in units of dataType */
    depth: Int,
    baseAddress: BigInt,
    outCnt: Int,
    busConfig: PipelinedMemoryBusConfig = PipelinedMemoryBusConfig(32, 32),
    rsp_latency : Int = 0
) extends Component {
  val io = new Bundle {
    val pop = master Stream (Fragment(Vec(dataType, outCnt)))

    val bus = master(PipelinedMemoryBus(busConfig))
  }
  require(depth % outCnt == 0)

  def div_assert_even(num : Int, den : Int): Int = {
    require((num % den) == 0)
    num / den
  }

  test_funcs.assertStreamContract(io.pop)
  test_funcs.assertPMBContract(io.bus)
  val midDatatype = Bits(StreamTools.lcm(dataType.getBitsWidth, busConfig.dataWidth) bits)

  val bufferSize = rsp_latency + 1
  val minBufferSizeInBits = bufferSize * busConfig.dataWidth
  val bufferSizeInMidWords = ((minBufferSizeInBits + midDatatype.getBitsWidth - 1) / midDatatype.getBitsWidth)
  val bufferSizeInBits = bufferSizeInMidWords * midDatatype.getBitsWidth
  val armRead = RegInit(True)

  var fifos = (0 until outCnt).map(i => StreamFifo(midDatatype, bufferSizeInMidWords))
  fifos.foreach(_.io.push.setIdle())
  fifos.foreach(_.logic.ram.addAttribute("syn_ramstyle", "distributed"))

  val pushes = Vec(fifos.map(_.io.push))
  var roundrobin_idx_cmd, roundrobin_idx_rsp = Counter(outCnt)

  val dt_words_per_out = div_assert_even(depth, outCnt)

  // Bus -> Mid -> DT

  val busWordsPerOut = div_assert_even(dt_words_per_out * dataType.getBitsWidth, busConfig.dataWidth)
  val bufferSizeInBusWords = div_assert_even(bufferSizeInBits, busConfig.dataWidth)
  val chunks_per_frame = div_assert_even(busWordsPerOut * busConfig.dataWidth, bufferSizeInBits)

  //val read_port = PipelinedMemoryBusBuffered(io.bus, busConfig.dataWidth / dataType.getBitsWidth - 1)
  val read_port = cloneOf(io.bus)
  read_port >> io.bus

  read_port.cmd.valid := False
  read_port.cmd.write.assignDontCare()
  read_port.cmd.payload.assignDontCare()
  read_port.cmd.mask.assignDontCare()

  val usage = new CounterUpDownUneven(bufferSizeInBits, busConfig.dataWidth, midDatatype.getBitsWidth)
  when( fifos.head.io.pop.fire) { usage.decrement() }
  val usageIsMax = usage.isMax

  val chunk_cmd = new Area {
    val read_address_words = Reg(UInt(read_port.config.addressWidth bits))

    read_port.cmd.address := read_address_words
    val cnt = Counter(bufferSizeInBusWords)
    val chunk_idx = Counter(chunks_per_frame)

    val roundrobin_idx_offset = CounterTools.multiply_by(roundrobin_idx_cmd, busWordsPerOut).valueNext
    val chunkidx_offset = CounterTools.multiply_by(chunk_idx, bufferSizeInBusWords).valueNext

    val read_address_words_logical =
      baseAddress +^ ((roundrobin_idx_cmd.value * busWordsPerOut +^ chunk_idx.value * bufferSizeInBusWords +^ cnt.value))

    read_address_words := (baseAddress +^ ((roundrobin_idx_offset +^ chunkidx_offset +^ cnt.valueNext))).resized

    assert(read_address_words_logical === read_address_words, "Read address is wrong")

    val stall = roundrobin_idx_cmd === 0 && usageIsMax
    when(!stall && armRead) {
      read_port.cmd.valid := True
      read_port.cmd.write := False

      when(read_port.cmd.fire) {
        cnt.increment()

        when(cnt.willOverflow) {
          roundrobin_idx_cmd.increment()
        }

        when(roundrobin_idx_cmd === 0) {
          usage.incrementIt := True
        }

        when(roundrobin_idx_cmd.willOverflow && chunk_idx.willOverflow) {
          armRead := False
        }

        when(roundrobin_idx_cmd.willOverflow) {
          chunk_idx.increment()
        }
      }
    }
  }


  val read_port_unpack = Flow(Bits(pushes(0).payload.getBitsWidth bits))
  StreamTools.AdaptWidth(read_port.rsp.map(_.data.asBits), read_port_unpack)

  val counter = Counter(640)
  when(io.pop.fire) {
    counter.increment()
  }
  when(io.pop.lastFire) {
    counter.clear()
  }
  val chunk_rsp = new Area {
    val cnt = Counter(bufferSizeInMidWords)

    when(read_port_unpack.fire) {
      pushes(roundrobin_idx_rsp).payload.assignFromBits(read_port_unpack.payload.asBits)
      pushes(roundrobin_idx_rsp).valid := True
      assert(pushes(roundrobin_idx_rsp).ready, "Ready should be true")

      cnt.increment()
      when(cnt.willOverflow) {
        roundrobin_idx_rsp.increment()
      }
    }
  }

  val popCounter = Counter(dt_words_per_out, inc = io.pop.fire)
  val adapedFifos = fifos.map(f => StreamTools.AdaptWidth(f.io.pop, Bits(dataType.getBitsWidth bits)).map(_.as(dataType)))
  io.pop <> StreamJoin.vec(adapedFifos).addFragmentLast(popCounter)
  when(io.pop.lastFire) {
    armRead := True
  }

  def attach_bus(busSlaveFactory: BusIf): Unit = {
    PipelinedMemoryBusLogger.attach_debug_registers(busSlaveFactory, io.bus.setName("strided_reader_bus"))

    RegisterTools.newSection(busSlaveFactory)
    RegisterTools.Counter(busSlaveFactory, "pop_fire", io.pop.fire)
    RegisterTools.Counter(busSlaveFactory, "pop_lastFire", io.pop.lastFire)
    RegisterTools.ReadOnly(busSlaveFactory, "chunk_cmd_cnt", chunk_cmd.cnt.value)
    RegisterTools.ReadOnly(busSlaveFactory, "chunk_rsp_cnt", chunk_rsp.cnt.value)
    RegisterTools.ReadOnly(busSlaveFactory, "roundrobin_idx_rsp", roundrobin_idx_rsp.value)
    RegisterTools.ReadOnly(busSlaveFactory, "readAddress", chunk_cmd.read_address_words)

    RegisterTools.ReadOnly(busSlaveFactory, "chunkidx_offset", chunk_cmd.chunkidx_offset)
    RegisterTools.ReadOnly(busSlaveFactory, "cnt", chunk_cmd.cnt.value)
    RegisterTools.ReadOnly(busSlaveFactory, "roundrobin_idx_cmd", roundrobin_idx_cmd.value)
    RegisterTools.ReadOnly(busSlaveFactory, "fifo_reader_status", armRead ## chunk_cmd.stall ## usageIsMax)
    RegisterTools.ReadOnly(busSlaveFactory, "popCounter", popCounter.value)

  }

}

class StridedAccessFIFOReaderTest extends AnyFunSuite {
  def doTest() {
    Config.sim.doSim(
      new Component {
        val reader = StridedAccessFIFOReader(Bits(32 bits), 2304, 0x00000000L, 9, rsp_latency = 32)
        val io = new Bundle {
          val pop = master(cloneOf(reader.io.pop))
        }
        val memory = SimpleMemoryProvider(
          init = (0 until 9).flatMap(idx => Array.fill(2304 / 9)(idx)).map(BigInt(_))
        )
        memory.io.bus <> reader.io.bus
        val start = RegInit(True) clearWhen (io.pop.valid) setWhen(io.pop.lastFire)

        reader.io.pop <> io.pop
      }.setDefinitionName("StridedAccessFIFOReaderTest")
    ) { dut =>
      SimTimeout(5000 us)
      dut.clockDomain.forkStimulus(100 MHz)
      dut.io.pop.ready #= true
      dut.clockDomain.waitSampling(10)
      val q = new mutable.ArrayBuffer[Seq[BigInt]]()

      StreamMonitor(dut.io.pop, dut.clockDomain) {
        data => {
          q.append(data.fragment.map(_.toBigInt))
        }
      }

      // Test random stall
      while (!(dut.io.pop.valid.toBoolean && dut.io.pop.ready.toBoolean && dut.io.pop.last.toBoolean)) {
        dut.io.pop.ready #= simRandom.nextBoolean()
        dut.clockDomain.waitSampling(1)
      }

      // Test no stall
      dut.clockDomain.waitSampling(1)
      while (!(dut.io.pop.valid.toBoolean && dut.io.pop.ready.toBoolean && dut.io.pop.last.toBoolean)) {
        dut.io.pop.ready #= true
        dut.clockDomain.waitSampling(1)
      }

      // Test heavy stall
      dut.clockDomain.waitSampling(1)
      while (!(dut.io.pop.valid.toBoolean && dut.io.pop.ready.toBoolean && dut.io.pop.last.toBoolean)) {
        dut.io.pop.ready #= false
        dut.clockDomain.waitSampling(64)

        dut.io.pop.ready #= true
        dut.clockDomain.waitSampling(1)
      }

      assert(q.size == 256 * 3)
    }
  }

  test("Basic") {
    doTest();
  }
}