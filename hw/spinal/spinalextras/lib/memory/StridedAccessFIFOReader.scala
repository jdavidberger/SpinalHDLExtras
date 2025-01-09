package spinalextras.lib.memory

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim.{SimBaseTypePimper, SimBoolPimper, SimClockDomainHandlePimper, SimTimeout, simRandom}
import spinal.core._
import spinal.core.formal.HasFormalAsserts
import spinal.lib.bus.regif.BusIf
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusConfig}
import spinal.lib.sim.StreamMonitor
import spinal.lib.{Counter, CounterUpDown, Flow, Fragment, Stream, StreamDemux, StreamFifo, StreamJoin, master}
import spinalextras.lib.Config
import spinalextras.lib.bus.PipelineMemoryGlobalBus
import spinalextras.lib.bus.simple.SimpleMemoryProvider
import spinalextras.lib.logging.PipelinedMemoryBusLogger
import spinalextras.lib.misc.{CounterTools, CounterUpDownUneven, PipelinedMemoryBusBuffered, RegisterTools, StreamTools}
import spinalextras.lib.testing.test_funcs
import spinalextras.lib.misc._

import scala.collection.mutable
import scala.language.postfixOps


case class StridedAccessFIFOReaderAsync[T <: Data](
                                                    dataType: HardType[T],
                                               /** Depth in units of dataType */
                                                    depth: Int,
                                                    baseAddress: BigInt,
                                                    outCnt: Int,
                                                    busConfig: PipelinedMemoryBusConfig = PipelinedMemoryBusConfig(32, 32),
                                                    rsp_latency : Int = 0
                                                  ) extends Component with HasFormalAsserts {
  var outSize = (busConfig.dataWidth / dataType.getBitsWidth.floatValue()).ceil.toInt
  while(depth % outSize != 0) {
    outSize += 1
  }

  val outDatatype = Vec(dataType, outSize )

  val io = new Bundle {
    val pop = master (AsyncStream (TupleBundle(outDatatype, UInt(log2Up(outCnt) bits))))
    val lastFire = out Bool()
    val bus = master(PipelinedMemoryBus(busConfig))
  }
  io.lastFire := False

  require(depth % outCnt == 0)

  def div_assert_even(num : Int, den : Int): Int = {
    if(num % den != 0) {
      println(s"Non even requirement ${num} ${den}")
    }
    require((num % den) == 0)
    num / den
  }

  val outWordsPerFrame = div_assert_even(depth, outDatatype.size * outCnt)
  val busContract = test_funcs.assertPMBContract(io.bus)
  val asyncStreamContract = test_funcs.assertAsyncStreamContract(io.pop)

  val bufferSize = rsp_latency + 1
  val minBufferSizeInBits = bufferSize * busConfig.dataWidth
  //val bufferSizeInMidWords = ((minBufferSizeInBits + midDatatype.getBitsWidth - 1) / midDatatype.getBitsWidth)
  var bufferSizeInOutWords = ((minBufferSizeInBits + outDatatype.getBitsWidth - 1) / outDatatype.getBitsWidth)
  while(((bufferSizeInOutWords * outDatatype.getBitsWidth) % busConfig.dataWidth) != 0) {
    bufferSizeInOutWords += 1
  }
  val bufferSizeInBits = bufferSizeInOutWords * outDatatype.getBitsWidth
  val armRead = RegInit(True)

  var roundrobin_idx_cmd, roundrobin_idx_rsp = Counter(outCnt)

  val dt_words_per_out = div_assert_even(depth, outCnt)
  //val dt_words_per_mid = midDatatype.getBitsWidth / dataType.getBitsWidth
  //val mid_words_per_out = div_assert_even(dt_words_per_out, dt_words_per_mid)

  // Bus -> Mid -> DT
  val frameSizeInBits = dt_words_per_out * dataType.getBitsWidth
  val busWordsPerOutputChannel = div_assert_even(frameSizeInBits, busConfig.dataWidth)

  val bufferSizeInBusWords = div_assert_even(bufferSizeInBits, busConfig.dataWidth)
  //val busWordsPerMidWord = midDatatype.getBitsWidth / busConfig.dataWidth
  val chunks_per_frame = div_assert_even(frameSizeInBits, bufferSizeInBits)

  //val read_port = PipelinedMemoryBusBuffered(io.bus, busConfig.dataWidth / dataType.getBitsWidth - 1)
  val read_port = cloneOf(io.bus)
  read_port >> io.bus

  read_port.cmd.valid := False
  read_port.cmd.write.assignDontCare()
  read_port.cmd.payload.assignDontCare()
  read_port.cmd.mask.assignDontCare()

  val pop = io.pop.steady_ready()
  val async_readys = pop.async_ready
  val async_valids = pop.async_valid
  async_valids := False

  val signal_valid = new mutable.ArrayBuffer[Boolean]()
  var remainder = busConfig.dataWidth
  for(i <- 0 until bufferSizeInBusWords) {
    remainder += busConfig.dataWidth
    if(remainder > outDatatype.getBitsWidth) {
      remainder -= outDatatype.getBitsWidth
      signal_valid.append(true)
    } else {
      signal_valid.append(false)
    }
  }
  val signal_valid_vec = Vec(signal_valid.map(Bool(_)))

  val dbg_counter, dbg_counter_rsp = Counter(32 bits)
  when(read_port.cmd.fire) {
    dbg_counter.increment()
  }
  when(read_port.rsp.fire) {
    dbg_counter_rsp.increment()
  }

  val chunk_cmd = new Area {
    val read_address_words = Reg(UInt(read_port.config.addressWidth bits)) init(baseAddress)

    read_port.cmd.address := read_address_words
    val cnt = Counter(bufferSizeInBusWords)
    val chunk_idx = Counter(chunks_per_frame)

    val roundrobin_idx_offset_area = CounterTools.multiply_by(roundrobin_idx_cmd, busWordsPerOutputChannel)
    val roundrobin_idx_offset = roundrobin_idx_offset_area.valueNext

    val chunkidx_offset_area = CounterTools.multiply_by(chunk_idx, bufferSizeInBusWords)
    val chunkidx_offset = chunkidx_offset_area.valueNext

    val read_address_words_logical =
      baseAddress +^ ((roundrobin_idx_cmd.value * busWordsPerOutputChannel +^ chunk_idx.value * bufferSizeInBusWords +^ cnt.value))

    read_address_words := (baseAddress +^ ((roundrobin_idx_offset +^ chunkidx_offset +^ cnt.valueNext))).resized

    assert(read_address_words_logical === read_address_words, "Read address is wrong")

    val stall = ~async_readys
    cover(stall)
    when(!stall && armRead) {
      read_port.cmd.valid := True
      read_port.cmd.write := False

      when(read_port.cmd.fire) {
        cnt.increment()
        async_valids := signal_valid_vec(cnt.value)

        when(cnt.willOverflow) {
          roundrobin_idx_cmd.increment()
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


  val read_port_unpack = Flow(Bits(outDatatype.getBitsWidth bits))
  StreamTools.AdaptWidth(read_port.rsp.map(_.data.asBits), read_port_unpack)

  val popCounter = Counter(outWordsPerFrame * outCnt)

  val chunk_rsp = new Area {
    pop.flow.payload._1.assignFromBits(read_port_unpack.payload)
    pop.flow.payload._2 := roundrobin_idx_rsp
    pop.flow.valid := read_port_unpack.fire

    when(read_port_unpack.fire) {
      popCounter.increment()
    }

    when(popCounter.willOverflow) {
      armRead := True
      io.lastFire := True
    }

    val cnt = Counter(bufferSizeInOutWords)
    when(read_port_unpack.fire) {
      cnt.increment()
      when(cnt.willOverflow) {
        roundrobin_idx_rsp.increment()
      }
    }
  }

  def attach_bus(busSlaveFactory: BusIf): Unit = {
    PipelinedMemoryBusLogger.attach_debug_registers(busSlaveFactory, io.bus.setName("strided_reader_bus"))

    RegisterTools.newSection(busSlaveFactory)
    RegisterTools.ReadOnly(busSlaveFactory, "chunk_cmd_cnt", chunk_cmd.cnt.value)
    RegisterTools.ReadOnly(busSlaveFactory, "chunk_rsp_cnt", chunk_rsp.cnt.value)
    RegisterTools.ReadOnly(busSlaveFactory, "roundrobin_idx_rsp", roundrobin_idx_rsp.value)
    RegisterTools.ReadOnly(busSlaveFactory, "readAddress", chunk_cmd.read_address_words)

    RegisterTools.ReadOnly(busSlaveFactory, "chunkidx_offset", chunk_cmd.chunkidx_offset)
    RegisterTools.ReadOnly(busSlaveFactory, "cnt", chunk_cmd.cnt.value)
    RegisterTools.ReadOnly(busSlaveFactory, "roundrobin_idx_cmd", roundrobin_idx_cmd.value)
    RegisterTools.ReadOnly(busSlaveFactory, "fifo_reader_status", armRead ## chunk_cmd.stall)
  }

  override lazy val formalValidInputs = io.bus.formalIsConsumerValid() && io.pop.formalIsConsumerValid()
  override def formalChecks()(implicit useAssumes: Boolean) = new Composite(this, "formalAsserts") {
    HasFormalAsserts.formalAssertsChildren(self, assumesInputValid = useAssumes, useAssumes = true)

    io.pop.formalAsserts()
    io.bus.formalAsserts()
  }
}
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

  val asyncReader = StridedAccessFIFOReaderAsync (dataType, depth, baseAddress, outCnt, busConfig, rsp_latency)
  asyncReader.io.bus <> io.bus

  test_funcs.assertStreamContract(io.pop)
  test_funcs.assertPMBContract(io.bus)
  val midDatatype = Bits(StreamTools.lcm(dataType.getBitsWidth, busConfig.dataWidth) bits)

  val bufferSize = rsp_latency + 1
  val minBufferSizeInBits = bufferSize * busConfig.dataWidth
  val bufferSizeInMidWords = ((minBufferSizeInBits + midDatatype.getBitsWidth - 1) / midDatatype.getBitsWidth)
  val bufferSizeInBits = bufferSizeInMidWords * midDatatype.getBitsWidth

  var fifos = (0 until outCnt).map(i => StreamFifo(midDatatype, bufferSizeInMidWords))
  fifos.filter(_.logic != null).foreach(_.logic.ram.addAttribute("syn_ramstyle", "distributed"))

  for(idx <- 0 until outCnt) {
    //fifos(idx).io.push <> asyncReader.io.pop(idx).toStream.map(_.asBits)
  }

  val popCounter = Counter(asyncReader.dt_words_per_out, inc = io.pop.fire)
  val adapedFifos = fifos.map(f => StreamTools.AdaptWidth(f.io.pop, Bits(dataType.getBitsWidth bits)).map(_.as(dataType)))
  io.pop <> StreamJoin.vec(adapedFifos).addFragmentLast(popCounter)

  def attach_bus(busSlaveFactory: BusIf): Unit = {
    PipelinedMemoryBusLogger.attach_debug_registers(busSlaveFactory, io.bus.setName("strided_reader_bus"))

    RegisterTools.newSection(busSlaveFactory)
    RegisterTools.Counter(busSlaveFactory, "pop_fire", io.pop.fire)
    RegisterTools.Counter(busSlaveFactory, "pop_lastFire", io.pop.lastFire)
    RegisterTools.ReadOnly(busSlaveFactory, "popCounter", popCounter.value)
  }

}

