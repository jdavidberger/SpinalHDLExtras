package spinalextras.lib.memory

import spinal.core._
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusConfig}
import spinal.lib.fsm.{EntryPoint, State, StateMachine}
import spinal.lib.{Counter, Flow, Stream, StreamFifo, master, slave, _}
import spinalextras.lib.bus.{PipelinedMemoryBusCmdExt, PipelinedMemoryBusConfigExt}
import spinalextras.lib.formal.StreamFormal.StreamExt
import spinalextras.lib.formal.fillins.PipelinedMemoryBusFormal.PipelinedMemoryBusFormalExt
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties, FormalProperty}
import spinalextras.lib.misc.{CounterUpDownUneven, StreamTools}

import scala.language.postfixOps

/**
 * This is a component that streams information out from a PMB memory source. It tries to cover up command/rsp latency to
 * have a 1 word per cycle throughput
 */
case class PipelinedMemoryBusBuffer[T <: Data](dataType: HardType[T], depthInBytes: Int, baseAddress: Int = 0,
                                               var config: PipelinedMemoryBusConfig = null,
                                               rsp_latency: Int = 0, cmd_latency: Int = 0, has_flush: Boolean = true) extends ComponentWithFormalProperties {
  if (config == null) {
    config = PipelinedMemoryBusConfig(addressWidth = log2Up(depthInBytes), dataWidth = dataType.getBitsWidth)
  }

  val depthInWords = depthInBytes >> config.wordAddressShift
  val midDatatype = Bits(StreamTools.lcm(dataType.getBitsWidth, config.dataWidth) bits)
  val bufferSize = (rsp_latency + 1).max(midDatatype.getWidth / config.dataWidth)

  val bufferSizeInBits = bufferSize * config.dataWidth
  val bufferSizeInMidWords = bufferSizeInBits / midDatatype.getWidth

  assert(bufferSizeInMidWords > 0)
  require(bufferSizeInBits % midDatatype.getBitsWidth == 0)

  val io = new Bundle {
    // Bus to orchestrate reads from
    val bus = master(PipelinedMemoryBus(config))

    // Output pop
    val pop = master Stream (dataType)

    // Flush clears out pending reads.
    val flush = has_flush generate (in Bool() default (False))

    // Check if there is data available. A fire means we've consumed that slot
    val memoryAvailable = slave(Stream(Bool()))
    val occupancy = out UInt (log2Up(bufferSizeInBits) bits)
  }
  val flush = if (has_flush) io.flush else False
  val readBus = io.bus

  val readBusCmd = cloneOf(readBus.cmd)

  // When we flush; queues don't care about handshakes
//  if(cmd_latency > 1) {
//    readBusCmd.addFormalException(flush)
//  }
  val readBusCmdQueue = StreamFifo(readBusCmd.payloadType, cmd_latency)
  //readBusCmdQueue.io.flush := flush
  readBusCmdQueue.io.push <> readBusCmd

  readBus.cmd <> readBusCmdQueue.io.pop

  val read_counter_inc = Bool()
  val read_counter = Counter(depthInWords, read_counter_inc)
  read_counter_inc := False

  readBusCmd.setIdle()
  readBusCmd.payload.write := False
  readBusCmd.payload.assignByteAddress(((read_counter.value << readBus.config.wordAddressShift) +^ baseAddress).resized)
  readBusCmd.payload.assignDontCareToUnasigned()

  val readBusCmdStalled = RegNext(readBusCmd.isStall) init(False)
  val readBusStallPayload = RegNext(readBusCmd.payload)

  var fifo = StreamFifo(midDatatype, bufferSizeInMidWords, latency = 0)
  withAutoPull()

  val adaptPop = StreamTools.AdaptWidth(fifo.io.pop, Bits(dataType.getBitsWidth bits)).map(_.as(dataType))
  adaptPop.setBlocked()

  val usage = new CounterUpDownUneven(bufferSizeInBits, config.dataWidth, midDatatype.getBitsWidth)
  when(readBusCmd.fire) {
    usage.increment()
  }
  when(fifo.io.pop.fire) {
    usage.decrement()
  }

  io.memoryAvailable.ready := False
  val overflow = Bool()

  val read_port_unpack = Flow(midDatatype)
  val adapt_out_width = StreamTools.AdaptWidth(io.bus.rsp.map(_.data.asBits), read_port_unpack)
  read_port_unpack.toStream(overflow) <> fifo.io.push
  assert(~overflow, "overflow buffer is full")

  io.pop.setIdle()

  val fsm = new StateMachine {
    val normal = new State with EntryPoint
    val flushing = new State

    normal.whenIsActive {
      readBusCmd.valid := ~usage.isMax && io.memoryAvailable.valid
      io.memoryAvailable.ready := readBusCmd.fire
      read_counter_inc := readBusCmd.fire && !readBusCmd.write && !flush

      adaptPop <> io.pop

      when(flush) {
        io.memoryAvailable.ready := True
        goto(flushing)
      }
    }

    flushing.whenIsActive {
      readBusCmd.valid := readBusCmdStalled
      //readBusCmd.payload := readBusStallPayload
      io.pop.setIdle()
      adaptPop.freeRun()
      io.occupancy := 0

      when(usage === 0 && readBusCmdStalled === False) {
        read_counter.clearAll()
        when(!io.flush) {
          goto(normal)
        }
      }
    }
  }

  val isFlushing = fsm.isActive(fsm.flushing)
  io.memoryAvailable.addFormalException(isFlushing)


  val occupancy = (readBus.contract.outstandingReads.value + adapt_out_width.io.occupancy + fifo.io.occupancy).resized
  io.occupancy := occupancy

  override def covers(): Seq[FormalProperty] = new FormalProperties(this) {
    addFormalProperty(io.occupancy > 0, "Occupancy should be able to be non zero")
    if (has_flush) {
      val sawFlush = RegInit(False) setWhen (io.flush && io.occupancy > 0)
      addFormalProperty(isFlushing)
      addFormalProperty(!isFlushing && sawFlush, "Should be able to leave flush")
    }
  }

  override def formalComponentInputProperties(): Seq[FormalProperty] = new FormalProperties(this) {
    when(flush) {
      addFormalProperty(io.pop.ready, "Flush requires a ready on the pop")
      //addFormalProperty(io.bus.cmd.ready, "Flush requires a ready on the cmd")
    }
  }

  override def formalComponentProperties(): Seq[FormalProperty] = new FormalProperties(this) {
    val readCmdBusCheck = readBusCmdQueue.formalCheckRam(_.write)
    val readCmdBusCheckOutput = readBusCmdQueue.formalCheckOutputStage(_.write)
    addFormalProperty(readCmdBusCheck.orR === False && readCmdBusCheckOutput === False, "Queue can't have reads")

    addFormalProperty(usage.value <= usage.maxValue, "Usage cant exceed max")
    when(usage.value === usage.maxValue) {
      addFormalProperty(readBusCmdStalled === False, "No way to have stalled read bus cmd when usage is full")
    }

    val fifoOccInBits = fifo.io.occupancy * fifo.dataType.getBitsWidth
    val busInFlightOccInBits = readBus.contract.outstandingReads.value * config.dataWidth
    val usageCheckInBits = fifoOccInBits +^ busInFlightOccInBits +^ readBusCmdQueue.io.occupancy * config.dataWidth
    addFormalProperty((usage.value) === (usageCheckInBits), s"Usage counts should be equal to the usageCheck ${this}")

    addFormalProperty(~overflow, "PMBBuffer overflowed")
  }
}
