package spinalextras.lib.misc

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.misc.DefaultMapping
import spinal.lib.bus.simple._
import spinal.lib.sim._
import spinalextras.lib._
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
    }).haltWhen(full | io.flush) >> writeBus.cmd

    val empty = ramOccupancy === 0 && inFlight.value === 0 // && readFifo.io.occupancy === 0
    io.empty := empty
  }
}

case class MemoryFifo[T <: Data](dataType : HardType[T], depth : Int,
                                 technologyKind: MemTechnologyKind = auto,
                                 mem_factory : (MemoryRequirement[T], MemTechnologyKind) => HardwareMemory[T] = Memories.apply[T] _) extends Component {
  val io = new Bundle {
    val push = slave Stream(dataType)
    val pop = master Stream(dataType)
    val empty = out Bool()

    val flush = in Bool() default(False)
    val occupancy    = out UInt (log2Up(depth + 1) bits)
    val availability = out UInt (log2Up(depth + 1) bits)
  }

  val mem = mem_factory(MemoryRequirement(dataType, depth, 0, 1, 1), technologyKind)
  val fifo = PipelinedMemoryBusFIFO(dataType, depth = depth)
  fifo.io.bus <> mem.pmbs().head
  fifo.io.push <> io.push
  fifo.io.pop <> io.pop
  fifo.io.empty <> io.empty
  fifo.io.flush <> io.flush
  fifo.io.occupancy <> io.occupancy
  fifo.io.availability <> io.availability
}

class MemMemoryTest extends AnyFunSuite {
  def doTest[T <: BitVector](dataType : HardType[T], depth : Int): Unit = {
    Config.sim.doSim(
      new MemoryFifo(dataType, depth)
    ) { dut =>
      SimTimeout(5000 us)
      dut.io.push.valid #= false
      dut.io.pop.ready #= false
      dut.io.flush #= false
      dut.clockDomain.forkStimulus(100 MHz)
      dut.clockDomain.waitSampling(10)

      val sco = new ScoreboardInOrder[BigInt]()

      StreamMonitor(dut.io.pop, dut.clockDomain) {
        datum => {
          println(s"Got ${datum.toBigInt}")
          sco.pushDut(datum.toBigInt)
        }
      }
      dut.io.pop.ready #= true

      for(i <- 0 until depth * 3) {
        dut.io.push.valid #= true
        val randValue = simRandom.nextLong().abs
        sco.pushRef(randValue)
        println(s"Pushing ${randValue}")
        dut.io.push.payload #= randValue
        dut.clockDomain.waitSamplingWhere(dut.io.push.ready.toBoolean)
      }
      dut.io.push.valid #= false

      while(dut.io.occupancy.toBigInt > 0) {
        dut.clockDomain.waitSampling()
      }

      sco.checkEmptyness()
    }
  }

  test("basic") {
    doTest(Bits(95 bits), 1000)
    doTest(Bits(96 bits), 1000)
  }
}

