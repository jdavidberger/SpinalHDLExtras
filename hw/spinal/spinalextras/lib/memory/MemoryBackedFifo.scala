package spinalextras.lib.memory

import spinal.core.{Area, Bool, BooleanPimped, Bundle, CombInit, Component, Data, False, HardType, IntToBuilder, Reg, RegNextWhen, True, U, UInt, assert, cloneOf, in, isPow2, log2Up, out, when}
import spinal.lib.bus.simple.PipelinedMemoryBusConfig
import spinal.lib.{KeepAttribute, Stream, StreamFifoInterface, master, slave}
import spinalextras.lib.HardwareMemory.HardwareMemoryReadWriteCmd
import spinalextras.lib.bus.PipelineMemoryGlobalBus
import spinalextras.lib.{HardwareMemory, Memories, MemoryRequirement}

class MemoryBackedFifo1[T <: Data](val dataType: HardType[T],
                                  val depth: Int,
                                  val allowExtraMsb: Boolean = true,
                                  val forFMax: Boolean = false,
                                  val mem_factory: ((MemoryRequirement[T]) => HardwareMemory[T]) = Memories.applyAuto[T] _
                                 ) extends Component {
  require(depth >= 0)

  val io = new Bundle with StreamFifoInterface[T] {
    val push = slave Stream (dataType)
    val pop = master Stream (dataType)
    val flush = in Bool() default (False)
    val occupancy = out UInt (log2Up(depth + 1) bits)
    val availability = out UInt (log2Up(depth + 1) bits)

    override def pushOccupancy = occupancy

    override def popOccupancy = occupancy
  }

  class CounterUpDownFmax(states: BigInt, init: BigInt) extends Area {
    val incr, decr = Bool()
    val value = Reg(UInt(log2Up(states) bits)) init (init)
    val plusOne = KeepAttribute(value + 1)
    val minusOne = KeepAttribute(value - 1)
    when(incr =/= decr) {
      value := incr.mux(plusOne, minusOne)
    }
    when(io.flush) {
      value := init
    }
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

    val ptr = new Area {
      val doPush, doPop = Bool()
      val full, empty = Bool()
      val push = Reg(UInt(log2Up(depth) + withExtraMsb.toInt bits)) init (0)
      val pop = Reg(UInt(log2Up(depth) + withExtraMsb.toInt bits)) init (0)
      val occupancy = cloneOf(io.occupancy)
      val popOnIo = cloneOf(pop) // Used to track the global occupancy of the fifo (the extra buffer of !withAsyncRead)
      val wentUp = RegNextWhen(doPush, doPush =/= doPop) init (False) clearWhen (io.flush)

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


      when(doPush) {
        push := push + 1
        if (!isPow2(depth)) when(push === depth - 1) {
          push := 0
        }
      }
      when(doPop) {
        pop := pop + 1
        if (!isPow2(depth)) when(pop === depth - 1) {
          pop := 0
        }
      }

      when(io.flush) {
        push := 0
        pop := 0
      }


      val forPow2 = (withExtraMsb && !forFMax) generate new Area {
        occupancy := push - popOnIo //if no extra msb, could be U(full ## (push - popOnIo))
      }

      val notPow2 = (!withExtraMsb && !forFMax) generate new Area {
        val counter = Reg(UInt(log2Up(depth + 1) bits)) init (0)
        counter := counter + U(io.push.fire) - U(io.pop.fire)
        occupancy := counter

        when(io.flush) {
          counter := 0
        }
      }
      val fmax = forFMax generate new CounterUpDownFmax(depth + 1, 0) {
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

      if (write.cmd.mask != null)
        write.cmd.mask.setAll()
      write.cmd.address := ptr.push.resized
      write.cmd.data := io.push.payload.asBits
    }

    val pop = new Area {
      val addressGen = Stream(UInt(log2Up(depth) bits))
      addressGen.valid := !ptr.empty
      addressGen.payload := ptr.pop.resized
      ptr.doPop := addressGen.fire

      val sync = new Area {
        val readArbitation = addressGen.m2sPipe(flush = io.flush)
        val readPort = ram.io.readPorts.head
        readPort.cmd := addressGen.toFlowFire.map(_.resize(readPort.cmd.payload.getBitsWidth))

        val overflow = Bool()
        assert(!overflow, "Buffer overflow in queue")
        val readStream = readPort.rsp.map(_.data.as(dataType)).toStream(overflow)
        val qSize = ram.latency + 2
        val (pop, occ) = readStream.queueWithOccupancy(qSize, latency = 0)
        val shouldRead = True
        io.pop <> pop
        readArbitation.ready := io.pop.ready && shouldRead

        val popReg = RegNextWhen(ptr.pop, readArbitation.fire) init (0)
        ptr.popOnIo := popReg
        when(io.flush) {
          popReg := 0
        }
      }
    }

    io.occupancy := ptr.occupancy
    if (!forFMax) io.availability := depth - ptr.occupancy
    val fmaxAvail = forFMax generate new CounterUpDownFmax(depth + 1, depth) {
      incr := io.pop.fire
      decr := io.push.fire
      io.availability := value
    }
  }
}


class MemoryBackedFifo[T <: Data](val dataType: HardType[T],
                                  val depth: Int,
                                  val mem_factory: ((MemoryRequirement[T]) => HardwareMemory[T]) = Memories.applyAuto[T] _
                                 ) extends Component {
  val mem = mem_factory(MemoryRequirement(dataType, depth, 2, 0, 0))
  val io = slave(new FifoInterface[T](dataType, depth))

  val sysBus = PipelineMemoryGlobalBus(PipelinedMemoryBusConfig(log2Up(depth), dataType.getBitsWidth))
  val fifo = new PipelinedMemoryBusFIFO(dataType, (0, depth), Some(sysBus))

  io.push <> fifo.io.push
  io.pop <> fifo.io.pop
  io.availability <> fifo.io.availability
  io.occupancy <> fifo.io.occupancy
  io.flush <> fifo.io.flush

  require(sysBus.masters.size == 2)
  require(sysBus.slaves.size == 0)

  mem.io.readWritePorts.zipWithIndex.foreach({case (rw, idx) => {
    val memBus = sysBus.masters(idx)._1
    memBus.cmd.map(x => {
      val rtn = HardwareMemoryReadWriteCmd(mem.config)
      rtn.address := x.address
      rtn.data := x.data
      rtn.write := x.write
      if (rtn.mask != null)
        rtn.mask := x.mask
      rtn
    }).toFlow <> rw.cmd

    rw.rsp >> memBus.rsp
  }})
  sysBus.cancel()
}
