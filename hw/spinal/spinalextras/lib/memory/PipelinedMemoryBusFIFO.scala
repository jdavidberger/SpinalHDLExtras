package spinalextras.lib.memory

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.{DefaultMapping, SizeMapping}
import spinal.lib.bus.regif.AccessType.{RO, RW}
import spinal.lib.bus.regif.{BusIf, SymbolName}
import spinal.lib.bus.simple._
import spinalextras.lib.bus.PipelineMemoryGlobalBus
import spinalextras.lib.testing.test_funcs

import scala.language.postfixOps

case class PipelinedMemoryBusBuffer[T <: Data](dataType : HardType[T], depth : Int, baseAddress : Int = 0,
                                               var config : PipelinedMemoryBusConfig = null) extends Component {
  if(config == null) {
    config = PipelinedMemoryBusConfig(addressWidth = log2Up(depth), dataWidth = dataType.getBitsWidth)
  }

  val io = new Bundle {
    val bus = master(PipelinedMemoryBus(config))
    val pop = master Stream (dataType)
    val memoryValid = slave Stream(Bool())
  }
  val readBus = io.bus
  test_funcs.assertStreamContract(io.bus.cmd)

  val read_counter = Counter(0 until depth, readBus.cmd.fire && !readBus.cmd.write)

  readBus.cmd.setIdle()
  readBus.cmd.write := False
  readBus.cmd.address := (read_counter.value.resize(readBus.cmd.address.getBitsWidth bits) + baseAddress)

  val mem_latency = 8
  var readFifo = new StreamFifo(
    dataType = dataType,
    depth = mem_latency,
    forFMax = true
  )

  io.memoryValid.ready := readBus.cmd.fire
  val outstanding = CounterUpDown(mem_latency, incWhen = readBus.cmd.fire && !readBus.cmd.write, decWhen = readFifo.io.push.fire)
  when(io.memoryValid.valid && RegNext(readFifo.io.availability > (outstanding.value + 1))) {
    readBus.cmd.valid := True
  }

  readFifo.io.push.payload.assignFromBits(readBus.rsp.data)
  readFifo.io.push.valid := readBus.rsp.valid
  assert(!readFifo.io.push.isStall, "ReadFifo stalled")

  readFifo.io.pop <> io.pop
}

case class PipelinedMemoryBusFIFO[T <: Data](dataType : HardType[T],
                                             sm : SizeMapping,
                                             sysBus : Option[PipelineMemoryGlobalBus] = None, localFifoDepth : Int = 0) extends Component {
  val depth = sm.size.toInt
  val baseAddress = sm.base.toInt

  val config = sysBus.map(g => g.config).getOrElse(PipelinedMemoryBusConfig(addressWidth = log2Up(depth), dataWidth = dataType.getBitsWidth))
  val readBus = sysBus.get.add_master(s"pmb_fifo_read_${baseAddress.hexString()}")
  val bus = sysBus.map(g => g.add_master(s"pmb_fifo_write_${baseAddress.hexString()}")).getOrElse(master(PipelinedMemoryBus(config)))

  val io = new Bundle {
    val push = slave Stream(dataType)
    val pop = master Stream(dataType)
    val empty = out Bool()

    val flush = in Bool() default(False)
    val occupancy    = out UInt (log2Up(depth + 1) bits)
    val availability = out UInt (log2Up(depth + 1) bits)
  }

  test_funcs.assertStreamContract(io.push)
  test_funcs.assertStreamContract(io.pop)

  val (writeBus, sharedBus) = sysBus.map((bus, _)).getOrElse(
    {
      val sysBus = new PipelineMemoryGlobalBus(config)
      bus <> sysBus.add_slave("data_store", DefaultMapping)
      (sysBus.add_master(s"pmb_fifo_write_${baseAddress.hexString()}"), sysBus)
    }
  )
  val flushArea = new ResetArea(io.flush, cumulative = true) {

    test_funcs.assertStreamContract(writeBus.cmd)

    val ramBackedBuffer = PipelinedMemoryBusBuffer(dataType, depth, baseAddress, sharedBus.config)
    ramBackedBuffer.io.bus <> readBus

    val inFlight = new CounterUpDown(depth, handleOverflow = false)
    when(ramBackedBuffer.io.pop.fire) {
      inFlight.decrement()
    }

    val ramOccupancy = new CounterUpDown(depth, handleOverflow = false)
    when(writeBus.cmd.fire) {
      ramOccupancy.increment()
    }
    io.occupancy := ramOccupancy.resized
    io.availability := depth - ramOccupancy.value

    ramBackedBuffer.io.pop <> io.pop
    ramBackedBuffer.io.memoryValid.payload := False
    ramBackedBuffer.io.memoryValid.valid := ramOccupancy > 0

    when(ramBackedBuffer.io.memoryValid.fire) {
      ramOccupancy.decrement()
      inFlight.increment()
    }

    val write_counter = Counter(0 until depth, writeBus.cmd.fire)

    val full = ramOccupancy.value === (depth - 1)
    val push = if(localFifoDepth > 0) io.push.queue(localFifoDepth) else io.push
    push.map(wrd => {
      val cmd = PipelinedMemoryBusCmd(writeBus.config)
      cmd.mask.setAll()
      cmd.write := True
      cmd.address := write_counter.value.resize(cmd.address.getBitsWidth bits) + baseAddress
      cmd.data := wrd.asBits
      cmd
    }).haltWhen(full | io.flush) >> writeBus.cmd

    val empty = ramOccupancy === 0 && inFlight.value === 0 // && readFifo.io.occupancy === 0
    io.empty := empty
  }

  def attach_bus(busSlaveFactory: BusIf): Unit = {
    for(v : Data <- Seq(io.occupancy, flushArea.ramOccupancy.value, flushArea.inFlight.value, flushArea.full)) {
      val reg = busSlaveFactory.newReg(f"${v.name} ${name}")(SymbolName(v.name))
      val field = reg.field(Bits(32 bits), RO, s"${name}_occ")
      field := v.asBits.resized
    }

    val total_push_reg = busSlaveFactory.newReg(f"total_push ${name}")
    val total_push = total_push_reg.field(UInt(32 bits), RW, s"${name}_total_push")
    when(io.push.fire) {
      total_push := total_push + 1
    }

    val total_pop_reg = busSlaveFactory.newReg(f"total_pop ${name}")
    val total_pop = total_pop_reg.field(UInt(32 bits), RW, s"${name}_total_pop")
    when(io.pop.fire) {
      total_pop := total_pop + 1
    }

    val highwater_reg = busSlaveFactory.newReg(f"highwater ${name}")
    val highwater = highwater_reg.field(cloneOf(io.occupancy), RW, s"${name}_highwater")
    when(RegNext(io.occupancy > highwater)) {
      highwater := io.occupancy
    }

  }
}
