package spinalextras.lib.memory

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.{DefaultMapping, SizeMapping}
import spinal.lib.bus.regif.AccessType.{RO, RW}
import spinal.lib.bus.regif.{BusIf, SymbolName}
import spinal.lib.bus.simple._
import spinal.lib.formal.ComponentWithFormalAsserts
import spinalextras.lib.bus.{PipelineMemoryGlobalBus, PipelinedMemoryBusCmdExt, PipelinedMemoryBusConfigExt}
import spinalextras.lib.misc.{CounterUpDownUneven, RegisterTools, StreamTools}
import spinalextras.lib.testing.test_funcs
import spinalextras.lib.bus.bus._

import scala.collection.mutable
import scala.language.postfixOps

case class PipelinedMemoryBusBuffer[T <: Data](dataType : HardType[T], depthInBytes : Int, baseAddress : Int = 0,
                                               var config : PipelinedMemoryBusConfig = null,
                                               rsp_latency : Int = 0, cmd_latency : Int = 0, read_trigger : Int = -1) extends ComponentWithFormalAsserts {
  if(config == null) {
    config = PipelinedMemoryBusConfig(addressWidth = log2Up(depthInBytes), dataWidth = dataType.getBitsWidth)
  }
  val depthInWords = depthInBytes >> config.wordAddressShift

  val midDatatype = Bits(StreamTools.lcm(dataType.getBitsWidth, config.dataWidth) bits)

  val bufferSize = (rsp_latency + 1).max(midDatatype.getWidth / config.dataWidth)

  val bufferSizeInBits = bufferSize * config.dataWidth
  val bufferSizeInMidWords = bufferSizeInBits / midDatatype.getWidth
  val readTriggerInBits = read_trigger * config.dataWidth

  assert(bufferSizeInMidWords > 0)
  require(bufferSizeInBits % midDatatype.getBitsWidth == 0)

  val io = new Bundle {
    val bus = master(PipelinedMemoryBus(config))
    val pop = master Stream (dataType)
    val flush = in Bool() default(False)

    val memoryAvailable = slave(Stream(Bool()))
  }

  val readBus = io.bus

  val readBusCmd = cloneOf(readBus.cmd)
  val readBusCmdQueue = StreamFifo(readBusCmd.payloadType, cmd_latency)
  val haltCmdQueue = RegInit(False)

  readBusCmdQueue.io.flush := io.flush
  readBusCmdQueue.io.push <> readBusCmd
  readBus.cmd <> readBusCmdQueue.io.pop.haltWhen(io.flush)

  val read_counter = Counter(depthInWords, readBusCmd.fire && !readBusCmd.write)

  readBusCmd.setIdle()
  readBusCmd.write := False
  readBusCmd.payload.assignByteAddress(((read_counter.value << readBus.config.wordAddressShift) +^ baseAddress).resized)

  var fifo = StreamFifo(midDatatype, bufferSizeInMidWords, latency = 0)
  withAutoPull()

  val adaptPop = StreamTools.AdaptWidth(fifo.io.pop.haltWhen(io.flush), Bits(dataType.getBitsWidth bits)).map(_.as(dataType))

  val usage = new CounterUpDownUneven(bufferSizeInBits, config.dataWidth, midDatatype.getBitsWidth)
  val burnRtn = Reg(usage.value.clone()) init(0)
  assert(burnRtn.getWidth >= U(usage.decBy).getBitsWidth)
  haltCmdQueue := burnRtn =/= 0

  val fifoOccInBits = fifo.io.occupancy * fifo.dataType.getBitsWidth

  when(burnRtn > 0) {
    io.pop.setIdle()
    adaptPop.freeRun()
    when(fifo.io.pop.fire) {
      burnRtn := burnRtn - usage.decBy
    }
  } otherwise {
    adaptPop <> io.pop
    when(fifo.io.pop.fire) { usage.decrement() }
  }

  when(readBusCmd.fire) { usage.increment() }

  val usageIsMax = usage.isMax

  io.memoryAvailable.ready := readBusCmd.fire

  val readTrigger =
    if(readTriggerInBits >= 0) {
      RegInit(True) setWhen(usage <= readTriggerInBits) clearWhen(io.pop.fire)
    } else {
      True
    }

  when(~usageIsMax) {
    readBusCmd.valid := io.memoryAvailable.valid && readTrigger && !haltCmdQueue
  }

  val overflow = Bool()

  val read_port_unpack = Flow(midDatatype)
   val adapt_out_width = StreamTools.AdaptWidth(io.bus.rsp.map(_.data.asBits), read_port_unpack)
  read_port_unpack.toStream(overflow) <> fifo.io.push
  assert(~overflow, "overflow buffer is full")

  when(io.flush) {
    read_counter.clearAll()
    usage.clear()
    haltCmdQueue := True
    burnRtn := burnRtn + usage.value
  }

  override def formalChecks()(implicit useAssumes: Boolean) = formalAssertsComposite()
  def formalAssertsComposite()(implicit useAssumes: Boolean) = new Composite(this, "formalAsserts") {
    //test_funcs.formalAssumeLibraryComponents(self)
    val readCmdBusCheck = readBusCmdQueue.formalCheckRam(_.write)
    val readCmdBusCheckOutput = readBusCmdQueue.formalCheckOutputStage(_.write)
    assertOrAssume(readCmdBusCheck.orR === False && readCmdBusCheckOutput === False)

    readBusCmdQueue.formalAssumeInputs()
    readBusCmdQueue.formalAssumes()

    val busContract = readBus.formalContract
    val busInFlightOccInBits = readBus.formalContract.outstandingReads.value * config.dataWidth
    val usageCheckInBits = fifoOccInBits +^ busInFlightOccInBits +^ readBusCmdQueue.io.occupancy * config.dataWidth
    assertOrAssume((usage.value +^ burnRtn) === (usageCheckInBits), s"Usage counts should be equal to the usageCheck ${this}")

    fifo.formalAssertInputs()
    fifo.formalAssumes()

    assertOrAssume(burnRtn % usage.decBy === 0)
    assertOrAssume(~overflow, "PMBBuffer overflowed")
    assertOrAssume((burnRtn +^ usage.value).msb === False )
    assertOrAssume((burnRtn +^ usage.value) <= bufferSizeInBits)

    val occupancy = (busContract.outstandingReads.value + adapt_out_width.io.occupancy + fifo.io.occupancy).resized

    // Outputs
    io.pop.formalAsserts()

    formalCheckOutputsAndChildren()
  }
}

case class PipelinedMemoryBusFIFO[T <: Data](dataType : HardType[T],
                                             sm : SizeMapping,
                                             sysBus : Option[PipelineMemoryGlobalBus] = None, localPushDepth : Int = 0, localPopDepth : Int = 0) extends ComponentWithFormalAsserts {
  val depthInBytes = sm.size.toInt
  val baseAddress = sm.base.toInt

  val config = sysBus.map(g => g.config).getOrElse(PipelinedMemoryBusConfig(addressWidth = log2Up(depthInBytes), dataWidth = dataType.getBitsWidth))
  val depthInWords = depthInBytes >> config.wordAddressShift

  require(log2Up(depthInBytes + baseAddress) <= config.addressWidth)

  val readBus = sysBus.get.add_master(s"pmb_fifo_read_${baseAddress.hexString()}")
  val bus = sysBus.map(g => g.add_master(s"pmb_fifo_write_${baseAddress.hexString()}")).getOrElse(master(PipelinedMemoryBus(config)))

  val io = new Bundle {
    val push = slave Stream(dataType)
    val pop = master Stream(dataType)
    val empty = out Bool()

    val flush = in Bool() default(False)
    val occupancy    = out UInt (log2Up(depthInWords + 1) bits)
    val availability = out UInt (log2Up(depthInWords + 1) bits)

    val debug_fake_write = in (Bool()) default(False)
  }

  val (writeBus, sharedBus) = sysBus.map((bus, _)).getOrElse(
    {
      val sysBus = new PipelineMemoryGlobalBus(config)
      bus <> sysBus.add_slave("data_store", DefaultMapping)
      (sysBus.add_master(s"pmb_fifo_write_${baseAddress.hexString()}"), sysBus)
    }
  )

//  val writeBusContract = test_funcs.assertPMBContract(writeBus)
//  val readBusContract = test_funcs.assertPMBContract(readBus)
  //assert(writeBusContract.outstanding_cnt.value === 0)

  when(!io.debug_fake_write) {
    assert(!writeBus.cmd.valid || writeBus.cmd.write)
  }

  val ramBackedBuffer = PipelinedMemoryBusBuffer(dataType, depthInBytes, baseAddress, sharedBus.config, rsp_latency = localPopDepth)

  ramBackedBuffer.io.bus <> readBus
  ramBackedBuffer.io.memoryAvailable.payload := False

  val inFlight = new CounterUpDown(depthInWords, handleOverflow = false)
  when(ramBackedBuffer.burnRtn > 0) {
    assert(inFlight.value === 0)
  }
  withAutoPull()

  val occupancy, ramOccupancy = new CounterUpDown(depthInWords, handleOverflow = false)
  when(writeBus.cmd.fire) {
    occupancy.increment()
    ramOccupancy.increment()
  }

  for (c <- Seq(inFlight, occupancy, ramOccupancy)) {
    test_funcs.assertCounter(c)
  }

  assert((ramOccupancy.value +^ inFlight.value) === occupancy.value)

  io.occupancy := occupancy.resized
  io.availability := depthInWords - occupancy.value
  ramBackedBuffer.io.memoryAvailable.valid := ramOccupancy > 0

  when(ramBackedBuffer.io.pop.fire) {
    inFlight.decrement()
  }

  ramBackedBuffer.io.pop <> io.pop
  when(io.pop.fire) {
    occupancy.decrement()
  }

  when(ramBackedBuffer.io.memoryAvailable.fire) {
    inFlight.increment()
    ramOccupancy.decrement()
  }

  val write_counter = Counter(depthInWords, writeBus.cmd.fire)

  val full = occupancy.value === (depthInWords - 1)
  val push = if(localPushDepth > 0) io.push.queue(localPushDepth) else io.push
  push.map(wrd => {
    val cmd = PipelinedMemoryBusCmd(writeBus.config)
    cmd.mask.setAll()
    cmd.write := ~io.debug_fake_write
    val byteAddress = (write_counter.value << writeBus.config.wordAddressShift).resize(cmd.address.getBitsWidth bits) +^ baseAddress
    assert(byteAddress.msb === False)
    cmd.assignByteAddress(byteAddress.resized)
    cmd.data := wrd.asBits
    cmd
  }).haltWhen(full | io.flush) >> writeBus.cmd

  val empty = occupancy === 0 // && readFifo.io.occupancy === 0
  io.empty := empty

  ramBackedBuffer.io.flush := io.flush
  when(io.flush) {
    occupancy.clearAll()
    ramOccupancy.clearAll()
    inFlight.clearAll()
    write_counter.clearAll()

    io.pop.valid := False
  }

  def covers(): Unit = {
    cover(full | io.flush)
    cover(full)
    cover(empty)
    cover(write_counter.willOverflowIfInc)
  }

  def attach_bus(busSlaveFactory: BusIf): Unit = {
    for(v : Data <- Seq(io.occupancy, occupancy.value, inFlight.value, full)) {
      val reg = busSlaveFactory.newReg(f"${v.name} ${name}")(SymbolName(v.name))
      val field = reg.field(Bits(32 bits), RO, s"${name}_occ")
      field := v.asBits.resized
    }

    RegisterTools.Counter(busSlaveFactory, "total_stall", io.push.isStall)
    RegisterTools.Counter(busSlaveFactory, "total_push", io.push.fire)
    RegisterTools.Counter(busSlaveFactory, "total_pop", io.push.fire)

    val highwater_reg = busSlaveFactory.newReg(f"highwater ${name}")
    val highwater = highwater_reg.field(cloneOf(io.occupancy), RW, s"${name}_highwater")
    when(RegNext(io.occupancy > highwater)) {
      highwater := io.occupancy
    }

  }

  override def formalChecks()(implicit useAssumes: Boolean) =
    new Composite(this, FormalCompositeName) {
      val ramBackedBufferAsserts = ramBackedBuffer.formalAssertsComposite()

      assertOrAssume(readBus.formalContract.outstandingReads === ramBackedBufferAsserts.busContract.outstandingReads)
      assertOrAssume(inFlight.value === ramBackedBufferAsserts.occupancy)
      formalCheckOutputsAndChildren()
    }
}
