package spinalextras.lib.memory

import spinal.core._
import spinal.lib.bus.regif.BusIf
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusConfig}
import spinal.lib.{Counter, Flow, Fragment, StreamFifo, StreamJoin, master}
import spinalextras.lib.bus.{PipelinedMemoryBusCmdExt, PipelinedMemoryBusConfigExt}
import spinalextras.lib.formal.fillins.PipelinedMemoryBusFormal.PipelinedMemoryBusFormalExt
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties, FormalProperty}
import spinalextras.lib.logging.PipelinedMemoryBusLogger
import spinalextras.lib.misc._
import spinalextras.lib.testing.test_funcs

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

/***
 * This component orchestrates reading from a PMB of a given data width, and popping out data of a different data width.
 *
 * The reads are done in stride bursts of `outCnt` size. The component requests up to a full stride of data, and then
 * only requests the next stride once the former stride is all returned.
 *
 * @param dataType Pop datatype
 * @param depth internal depth to buffer for
 * @param baseAddress Address to read from
 * @param outCnt Size of the stride. The index of the stride is also returned in the pop
 * @param busConfig Configuration of the bus
 * @param rsp_latency Expected response latency for the reads. So long as latency stays below or at this level, the
 *                    reads are pipelined in such a way that constant cmd.valids gives constant pops.
 */
case class StridedAccessFIFOReaderAsync[T <: Data](
                                                    dataType: HardType[T],
                                               /** Depth in units of dataType */
                                                    depth: Int,
                                                    baseAddress: BigInt,
                                                    outCnt: Int,
                                                    busConfig: PipelinedMemoryBusConfig = PipelinedMemoryBusConfig(32, 32),
                                                    rsp_latency : Int = 0
                                                  ) extends ComponentWithFormalProperties {
  var outSize = (busConfig.dataWidth / dataType.getBitsWidth.floatValue()).ceil.toInt
  require(outSize <= depth)
  while(depth % outSize != 0) {
    outSize += 1
  }

  val baseAddressWords = baseAddress >> busConfig.wordAddressShift
  val outDatatype = Vec(dataType, outSize )

  val io = new Bundle {
    val pop = master (AsyncStream (TupleBundle(outDatatype, UInt(log2Up(outCnt) bits))))
    val lastFire = out Bool()
    val bus = master(PipelinedMemoryBus(busConfig))

    val debug_fake_read = in Bool() default(False)
  }
  io.lastFire := False

  require(depth % outCnt == 0)

  def div_assert_even(num : Int, den : Int): Int = {
    if(den > 0 && num % den != 0) {
      println(s"Non even requirement ${num} ${den}")
    }
    require((num % den) == 0)
    num / den
  }

  val outWordsPerFrame = div_assert_even(depth, outDatatype.size * outCnt)

  val bufferSize = rsp_latency + 1
  val minBufferSizeInBits = bufferSize * busConfig.dataWidth
  //val bufferSizeInMidWords = ((minBufferSizeInBits + midDatatype.getBitsWidth - 1) / midDatatype.getBitsWidth)
  var bufferSizeInOutWords = ((minBufferSizeInBits + outDatatype.getBitsWidth - 1) / outDatatype.getBitsWidth)
  while(((bufferSizeInOutWords * outDatatype.getBitsWidth) % busConfig.dataWidth) != 0) {
    bufferSizeInOutWords += 1
  }
  val bufferSizeInBits = bufferSizeInOutWords * outDatatype.getBitsWidth

  // State that determines where we are in the cycle -- when true we have more to read for this current stride
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

  val pop = io.pop
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
    val read_address_words = Reg(UInt((read_port.config.addressWidth - read_port.config.wordAddressShift) bits)) init(baseAddressWords)

    read_port.cmd.payload.assignWordAddress(read_address_words)
    val cnt = Counter(bufferSizeInBusWords)
    val chunk_idx = Counter(chunks_per_frame)

    val roundrobin_idx_offset_area = CounterTools.multiply_by(roundrobin_idx_cmd, busWordsPerOutputChannel)
    val roundrobin_idx_offset = roundrobin_idx_offset_area.valueNext

    val chunkidx_offset_area = CounterTools.multiply_by(chunk_idx, bufferSizeInBusWords)
    val chunkidx_offset = chunkidx_offset_area.valueNext

    val read_address_words_logical =
      baseAddressWords +^ ((roundrobin_idx_cmd.value * busWordsPerOutputChannel +^ chunk_idx.value * bufferSizeInBusWords +^ cnt.value))

    read_address_words := (baseAddressWords +^ ((roundrobin_idx_offset +^ chunkidx_offset +^ cnt.valueNext))).resized

    assert(read_address_words_logical === read_address_words, "Read address is wrong")

    val stall = ~async_readys
    //cover(stall)
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
          io.lastFire := True
        }

        when(roundrobin_idx_cmd.willOverflow) {
          chunk_idx.increment()
        }
      }
    }
  }

  when(io.lastFire) {
    assert(!io.bus.cmd.isStall)
  }

  val read_port_unpack = Flow(Bits(outDatatype.getBitsWidth bits))
  val adapter = StreamTools.AdaptWidth(read_port.rsp.map(_.data.asBits), read_port_unpack)

  val popCounter = Counter(outWordsPerFrame * outCnt)

  pop.flow.setIdle()

  val chunk_rsp = new Area {

    when(read_port_unpack.valid) {
      when(io.debug_fake_read) {
        pop.flow.payload._1.clearAll()
      } otherwise {
        pop.flow.payload._1.assignFromBits(read_port_unpack.payload)
      }
      pop.flow.payload._2 := roundrobin_idx_rsp
      pop.flow.valid := read_port_unpack.valid
    }

    when(read_port_unpack.fire) {
      popCounter.increment()
    }

    when(popCounter.willOverflow) {
      armRead := True
    }

    val cnt = Counter(bufferSizeInOutWords)
    when(read_port_unpack.fire) {
      cnt.increment()
      when(cnt.willOverflow) {
        roundrobin_idx_rsp.increment()
        //assert(adapter.io.isEmpty)
      }
    }
  }

  override protected def formalProperties(): Seq[FormalProperty] = super.formalProperties() ++ new FormalProperties(this) {

    addFormalProperty(io.bus.contract.outstandingReads.value <= chunk_cmd.cnt, "There should not be more outstanding reads than chunk count")
    addFormalProperty(io.pop.formalContract.outstandingFlows.value <= outCnt)

    val busBitsOutstanding = io.bus.config.dataWidth * io.bus.contract.outstandingReads.value
    val outBitsOutstanding = outDatatype.getBitsWidth * io.pop.formalContract.outstandingFlows.value
    val adapterBits = CombInit(adapter.formalCounter.value)
    addFormalProperty((busBitsOutstanding +^ adapterBits) >= outBitsOutstanding)

    val busReadsLeftInCnt = (chunk_cmd.cnt.end + 1 - chunk_cmd.cnt.value)
    val signal_valid_remaining_lut = ({
      val lst = new ArrayBuffer[Int]()
      lst += signal_valid.last.toInt // 0 0 3
      // 3
      for (i <- 1 until signal_valid.size) {
        lst += lst.last + signal_valid(signal_valid.size - i - 1).toInt
      }
      lst.reverse.map(U(_))
    })

    val validFiresLeftInCnt = UInt(signal_valid_remaining_lut.map(_.getBitsWidth).max bits)
    validFiresLeftInCnt.assignDontCare()
    signal_valid_remaining_lut.zipWithIndex.foreach(v => {
      when(chunk_cmd.cnt === v._2) {
        validFiresLeftInCnt := v._1.resized
      }
    })
    val validFiresBitsLeftInCnt = validFiresLeftInCnt * outDatatype.getBitsWidth
    val outstandingReadsIn = (busBitsOutstanding +^ adapterBits +^ busReadsLeftInCnt * busConfig.dataWidth)
    val outstandingReadsOut = (outBitsOutstanding +^ validFiresBitsLeftInCnt)
    addFormalProperty(outstandingReadsIn === outstandingReadsOut, s"Oustanding reads math failed to resolve for ${this}")
    addFormalProperty(validFiresLeftInCnt =/= 0)

//    val total_req, total_res = Counter(32 bits)
//    assume(!total_req.willOverflow)
//    assume(!total_res.willOverflow)
//
//    when(chunk_cmd.cnt.willIncrement) { total_req.increment() }
//    when(chunk_rsp.cnt.willIncrement) { total_res.increment() }
//
//    assertOrAssume(total_req.value % (chunk_cmd.cnt.end + 1) === chunk_cmd.cnt.value)
//    assertOrAssume(((total_req.value / (chunk_cmd.cnt.end + 1)) % (roundrobin_idx_cmd.end + 1)) === roundrobin_idx_cmd.value)
//    assertOrAssume(((total_req.value / ((chunk_cmd.cnt.end + 1) * (roundrobin_idx_cmd.end + 1))) % (chunk_cmd.chunk_idx.end + 1)) === chunk_cmd.chunk_idx.value)
//
//    assertOrAssume(total_res.value % (chunk_rsp.cnt.end + 1) === chunk_rsp.cnt.value)
//    assertOrAssume((total_res.value / (chunk_rsp.cnt.end + 1)) % (roundrobin_idx_rsp.end + 1) === roundrobin_idx_rsp.value)
//    assertOrAssume(total_res.value % (popCounter.end + 1) === popCounter.value)

    //assertOrAssume(total_res +^ io.bus.formalContract.outstandingReads === total_req)
    val req_size = io.bus.config.dataWidth
    val res_size = outDatatype.getBitsWidth
    //assertOrAssume(total_req * req_size >= total_res * res_size)

    import chunk_cmd._
    val reads_made = (chunk_idx.value * bufferSizeInBusWords * outCnt) +^
      (roundrobin_idx_cmd.value * bufferSizeInBusWords) +^
      cnt.value
    when(armRead) {
      //assertOrAssume(reads_made >= io.bus.formalContract.outstandingReads.value)
    } otherwise {
      addFormalProperty(cnt.value === 0)
      addFormalProperty(roundrobin_idx_cmd.value === 0)
      addFormalProperty(chunk_idx.value === 0)
    }
//    val popsRemaining = CombInit((popCounter.maxValue + 1) - popCounter.value)
//    val popsOutstanding = CombInit(io.pop.formalContract.outstandingFlows.value)
//
//    assertOrAssume(popsRemaining >= popsOutstanding)
    //formalCheckOutputsAndChildren()
  }.formalProperties

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

    val debug_fake_read = GlobalSignals.externalize(io.debug_fake_read)
    debug_fake_read := RegisterTools.Register(busSlaveFactory, "rdr_debug_ctrl", False)
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
) extends ComponentWithFormalProperties {
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

  withAutoPull()
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

