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

import scala.language.postfixOps

class MemoryBackedFifo[T <: Data](val dataType: HardType[T],
                                  val depth: Int,
                                  val allowExtraMsb : Boolean = true,
                                  val forFMax : Boolean = false,
                                  val mem_factory : ((MemoryRequirement[T]) => HardwareMemory[T]) = Memories.applyAuto[T] _
                                 ) extends Component {
  require(depth >= 0)

  val io = new Bundle with StreamFifoInterface[T]{
    val push = slave Stream (dataType)
    val pop = master Stream (dataType)
    val flush = in Bool() default(False)
    val occupancy    = out UInt (log2Up(depth + 1) bits)
    val availability = out UInt (log2Up(depth + 1) bits)
    override def pushOccupancy = occupancy
    override def popOccupancy = occupancy
  }

  class CounterUpDownFmax(states : BigInt, init : BigInt) extends Area{
    val incr, decr = Bool()
    val value = Reg(UInt(log2Up(states) bits)) init(init)
    val plusOne = KeepAttribute(value + 1)
    val minusOne = KeepAttribute(value - 1)
    when(incr =/= decr){
      value := incr.mux(plusOne, minusOne)
    }
    when(io.flush) { value := init }
  }

  val withExtraMsb = allowExtraMsb && isPow2(depth)
  val bypass = (depth == 0) generate new Area {
    io.push >> io.pop
    io.occupancy := 0
    io.availability := 0
  }
  val oneStage = (depth == 1) generate new Area {
    val doFlush = CombInit(io.flush)
    val buffer = io.push.m2sPipe(flush = doFlush)
    io.pop << buffer
    io.occupancy := U(buffer.valid)
    io.availability := U(!buffer.valid)
  }
  val logic = (depth > 1) generate new Area {
    val ram = mem_factory(MemoryRequirement(dataType, depth, 0, 1, 1))
    assert(ram.io.writePorts.size >= 1)
    assert(ram.io.readPorts.size >= 1)

    val ptr = new Area{
      val doPush, doPop = Bool()
      val full, empty = Bool()
      val push = Reg(UInt(log2Up(depth) + withExtraMsb.toInt bits)) init(0)
      val pop  = Reg(UInt(log2Up(depth) + withExtraMsb.toInt bits)) init(0)
      val occupancy = cloneOf(io.occupancy)
      val popOnIo = cloneOf(pop) // Used to track the global occupancy of the fifo (the extra buffer of !withAsyncRead)
      val wentUp = RegNextWhen(doPush, doPush =/= doPop) init(False) clearWhen (io.flush)

      val arb = new Area {
        val area = !forFMax generate {
          withExtraMsb match {
            case true => { //as we have extra MSB, we don't need the "wentUp"
              full := (push ^ popOnIo ^ depth) === 0
              empty := push === pop
            }
            case false => {
              full := push === popOnIo && wentUp
              empty := push === pop && !wentUp
            }
          }
        }

        val fmax = forFMax generate new Area {
          val counterWidth = log2Up(depth) + 1
          val emptyTracker = new CounterUpDownFmax(1 << counterWidth, 1 << (counterWidth - 1)) {
            incr := doPop
            decr := doPush
            empty := value.msb
          }

          val fullTracker = new CounterUpDownFmax(1 << counterWidth, (1 << (counterWidth - 1)) - depth) {
            incr := io.push.fire
            decr := io.pop.fire
            full := value.msb
          }
        }
      }


      when(doPush){
        push := push + 1
        if(!isPow2(depth)) when(push === depth - 1){ push := 0 }
      }
      when(doPop){
        pop := pop + 1
        if(!isPow2(depth)) when(pop === depth - 1){ pop := 0 }
      }

      when(io.flush){
        push := 0
        pop := 0
      }


      val forPow2 = (withExtraMsb && !forFMax) generate new Area{
        occupancy := push - popOnIo  //if no extra msb, could be U(full ## (push - popOnIo))
      }

      val notPow2 = (!withExtraMsb && !forFMax) generate new Area{
        val counter = Reg(UInt(log2Up(depth + 1) bits)) init(0)
        counter := counter + U(io.push.fire) - U(io.pop.fire)
        occupancy := counter

        when(io.flush) { counter := 0 }
      }
      val fmax = forFMax generate new CounterUpDownFmax(depth + 1, 0){
        incr := io.push.fire
        decr := io.pop.fire
        occupancy := value
      }
    }

    val push = new Area {
      io.push.ready := !ptr.full
      ptr.doPush := io.push.fire

      val write = ram.io.writePorts.head
      write.cmd.valid := io.push.fire

      if(write.cmd.mask != null)
        write.cmd.mask.setAll()
      write.cmd.address := ptr.push.resized
      write.cmd.data := io.push.payload.asBits
    }

    val pop = new Area{
      val addressGen = Stream(UInt(log2Up(depth) bits))
      addressGen.valid := !ptr.empty
      addressGen.payload := ptr.pop.resized
      ptr.doPop := addressGen.fire

      val sync = new Area{
        val readArbitation = addressGen.m2sPipe(flush = io.flush)
        val readPort = ram.io.readPorts.head
        readPort.cmd := addressGen.toFlowFire

        val overflow = Bool()
        io.pop <> readPort.rsp.map(_.data.as(dataType)).toStream(overflow, fifoSize = (ram.latency + 1), overflowOccupancyAt = ram.latency)
        readArbitation.ready := io.pop.ready && !overflow

        val popReg = RegNextWhen(ptr.pop, readArbitation.fire) init(0)
        ptr.popOnIo := popReg
        when(io.flush){ popReg := 0 }
      }
    }

    io.occupancy := ptr.occupancy
    if(!forFMax) io.availability := depth - ptr.occupancy
    val fmaxAvail = forFMax generate new CounterUpDownFmax(depth + 1, depth){
      incr := io.pop.fire
      decr := io.push.fire
      io.availability := value
    }
  }
}

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
      new MemoryBackedFifo(dataType, depth)
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

