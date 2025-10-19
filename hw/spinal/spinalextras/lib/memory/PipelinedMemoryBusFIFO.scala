package spinalextras.lib.memory

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.{DefaultMapping, SizeMapping}
import spinal.lib.bus.regif.AccessType.{RO, RW}
import spinal.lib.bus.regif.{BusIf, SymbolName}
import spinal.lib.bus.simple._
import spinal.lib.fsm.{EntryPoint, State, StateMachine}
import spinalextras.lib.bus.{PipelineMemoryGlobalBus, PipelinedMemoryBusCmdExt, PipelinedMemoryBusConfigExt}
import spinalextras.lib.formal.StreamFormal.StreamExt
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties, FormalProperty}
import spinalextras.lib.misc.RegisterTools
import spinalextras.lib.testing.test_funcs

import scala.language.postfixOps



case class PipelinedMemoryBusFIFO[T <: Data](dataType : HardType[T],
                                             sm : SizeMapping,
                                             sysBus : Option[PipelineMemoryGlobalBus] = None, localPushDepth : Int = 0, localPopDepth : Int = 0) extends ComponentWithFormalProperties {
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
  writeBus.cmd.setIdle()
  val isWriteBusStalled = RegNext(writeBus.cmd.isStall)
  val writeBusStalled = RegNextWhen(writeBus.cmd.payload, writeBus.cmd.isStall)
  writeBus.cmd.payload := writeBusStalled

  when(!io.debug_fake_write) {
    assert(!writeBus.cmd.valid || writeBus.cmd.write)
  }

  val ramBackedBuffer = PipelinedMemoryBusBuffer(dataType, depthInBytes, baseAddress, sharedBus.config, rsp_latency = localPopDepth)
  ramBackedBuffer.io.bus <> readBus
  ramBackedBuffer.io.memoryAvailable.payload := False

  val inFlight = new CounterUpDown(depthInWords, handleOverflow = false)
  withAutoPull()

  val
    // Number of items in the fifo currently
  occupancy,
   // ???
  ramOccupancy = new CounterUpDown(depthInWords, handleOverflow = false)

  for (c <- Seq(inFlight, occupancy, ramOccupancy)) {
    test_funcs.assertCounter(c)
  }

  val isFlushing = Bool()
  isFlushing := False

  io.occupancy := occupancy.resized
  io.availability := depthInWords - occupancy.value
  ramBackedBuffer.io.memoryAvailable.valid := ramOccupancy > 0
  val memoryAvailableStall = RegNext(ramBackedBuffer.io.memoryAvailable.isStall) init(False)

  when(ramBackedBuffer.io.pop.fire) {
    inFlight.decrement()
  }

  ramBackedBuffer.io.pop <> io.pop
  when(io.pop.fire) {
    occupancy.decrement()
  }

  val write_counter = Counter(depthInWords, writeBus.cmd.fire)

  val full = CombInit(occupancy.willOverflowIfInc)// CombInit(occupancy.value === (depthInWords - 1))

  val push = io.push.continueWhen(!isFlushing).addFormalException(isFlushing)

  val queuedPush = if(localPushDepth > 0) push.queue(localPushDepth) else push
  val pushToCmd = queuedPush.map(wrd => {
    val cmd = PipelinedMemoryBusCmd(writeBus.config)
    cmd.mask.setAll()
    cmd.write := ~io.debug_fake_write
    val byteAddress = (write_counter.value << writeBus.config.wordAddressShift).resize(cmd.address.getBitsWidth bits) +^ baseAddress
    //assert(byteAddress.msb === False)
    cmd.assignByteAddress(byteAddress.resized)
    cmd.data := wrd.asBits
    cmd
  }).addFormalException(io.flush)
  pushToCmd.setBlocked()

  val empty = CombInit(occupancy === 0) // && readFifo.io.occupancy === 0
  io.empty := empty

  ramBackedBuffer.io.flush := io.flush

  val fsm = new StateMachine {
    val normal = new State with EntryPoint
    val flushing = new State

    normal.whenIsActive {
      pushToCmd.haltWhen(full) >> writeBus.cmd

      when(writeBus.cmd.fire) {
        occupancy.increment()
        ramOccupancy.increment()
      }

      when(ramBackedBuffer.io.memoryAvailable.fire) {
        inFlight.increment()
        ramOccupancy.decrement()
      }

      when(io.flush) {
        occupancy.clearAll()
        ramOccupancy.clearAll()
        inFlight.clearAll()

        goto(flushing)
      }
    }

    flushing.whenIsActive {
      ramBackedBuffer.io.flush := True
      ramBackedBuffer.io.pop.ready := True

      isFlushing := True

      ramBackedBuffer.io.memoryAvailable.valid := memoryAvailableStall
      writeBus.cmd.valid := isWriteBusStalled
      pushToCmd.freeRun()

      io.pop.valid := False
      io.push.ready := False

      when(!isWriteBusStalled && !memoryAvailableStall) {
        write_counter.clearAll()

        when(!io.flush) {
          goto(normal)
        }
      }
    }
  }

  override def covers() = new FormalProperties(this) {
    addFormalProperty(full | io.flush)
    addFormalProperty(full)
    addFormalProperty(empty)
    addFormalProperty(write_counter.willOverflowIfInc)
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

  override def formalComponentInputProperties(): Seq[FormalProperty] = new FormalProperties(this) {
    when(io.flush) {
      addFormalProperty(io.pop.ready, "Flush requires a ready on the pop")
    }
  }

  override def formalComponentProperties(): Seq[FormalProperty] = new FormalProperties(this) {
    withAutoPull()
    addFormalProperty((ramOccupancy.value +^ inFlight.value) === occupancy.value, "RamOcc + inFlight should equal occ")
    //addFormalProperty(readBus.contract.outstandingReads === ramBackedBuffer.readBus.contract.outstandingReads)
    addFormalProperty(inFlight.value === ramBackedBuffer.io.occupancy, "Inflight and occupanccy counter is off")

    when(isFlushing) {
      addFormalProperty(occupancy === 0, "Occupancy must be zero while flushing")
      addFormalProperty(ramOccupancy === 0, "Ram Occ must be zero while flushing")
      addFormalProperty(inFlight === 0, "In flight occ must be zero while flushing")
    }
  }

}
