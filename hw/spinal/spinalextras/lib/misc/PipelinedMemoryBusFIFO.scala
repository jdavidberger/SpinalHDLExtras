package spinalextras.lib.misc

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.DefaultMapping
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusCmd, PipelinedMemoryBusConfig}
import spinalextras.lib.testing.test_funcs

case class PipelinedMemoryBusBuffer[T <: Data](dataType : HardType[T], depth : Int, baseAddress : Int = 0) extends Component {
  val config = PipelinedMemoryBusConfig(addressWidth = log2Up(depth), dataWidth = dataType.getBitsWidth)

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
  readBus.cmd.address := (read_counter.value + baseAddress).resized

  val mem_latency = 4
  var readFifo = new StreamFifo(
    dataType = dataType,
    depth = mem_latency,
    withAsyncRead = true,
    withBypass = true
  )

  io.memoryValid.ready := readBus.cmd.fire
  val outstanding = CounterUpDown(mem_latency, incWhen = readBus.cmd.fire && !readBus.cmd.write, decWhen = readFifo.io.push.fire)
  when(io.memoryValid.valid && readFifo.io.availability > outstanding) {
    readBus.cmd.valid := True
  }

  readFifo.io.push.payload.assignFromBits(readBus.rsp.data)
  readFifo.io.push.valid := readBus.rsp.valid

  readFifo.io.pop <> io.pop
}

case class PipelinedMemoryBusFIFO[T <: Data](dataType : HardType[T],
                                             depth : Int, sysBus : Option[PipelineMemoryGlobalBus] = None,
                                             baseAddress : Int = 0) extends Component {
  val config = sysBus.map(g => g.config).getOrElse(PipelinedMemoryBusConfig(addressWidth = log2Up(depth), dataWidth = dataType.getBitsWidth))

  val io = new Bundle {
    val bus = sysBus.map(g => g.add_master(s"pmb_fifo_write_${baseAddress.toHexString}")).getOrElse(master(PipelinedMemoryBus(config)))
    val push = slave Stream(dataType)
    val pop = master Stream(dataType)
    val empty = out Bool()

    val flush = in Bool() default(False)
    val occupancy    = out UInt (log2Up(depth + 1) bits)
    val availability = out UInt (log2Up(depth + 1) bits)
  }

  val (writeBus, sharedBus) = sysBus.map((io.bus, _)).getOrElse(
    {
      val sysBus = new PipelineMemoryGlobalBus(config)
      io.bus <> sysBus.add_slave("data_store", DefaultMapping)
      (sysBus.add_master(s"pmb_fifo_write_${baseAddress.hexString()}"), sysBus)
    }
  )
  val flushArea = new ResetArea(io.flush, cumulative = true) {

    test_funcs.assertStreamContract(writeBus.cmd)

    val ramBackedBuffer = PipelinedMemoryBusBuffer(dataType, depth, baseAddress)
    ramBackedBuffer.io.bus <> sharedBus.add_master(s"pmb_fifo_read_${baseAddress.hexString()}")

    val inFlight = new CounterUpDown(depth, handleOverflow = false)
    when(ramBackedBuffer.io.pop.fire) {
      inFlight.decrement()
    }

    val ramOccupancy = new CounterUpDown(depth, handleOverflow = false)
    when(writeBus.cmd.fire) {
      ramOccupancy.increment()
    }
    io.occupancy := ramOccupancy
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
    io.push.map(wrd => {
      val cmd = PipelinedMemoryBusCmd(writeBus.config)
      cmd.mask.setAll()
      cmd.write := True
      cmd.address := (write_counter.value + baseAddress).resized
      cmd.data := wrd.asBits
      cmd
    }).haltWhen(full) >> writeBus.cmd

    val empty = ramOccupancy === 0 && inFlight.value === 0 // && readFifo.io.occupancy === 0
    io.empty := empty
  }
}