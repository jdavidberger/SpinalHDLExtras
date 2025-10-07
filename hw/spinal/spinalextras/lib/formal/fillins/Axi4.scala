package spinalextras.lib.formal.fillins

import spinal.core.Component.push
import spinal.core.Data
import spinal.lib.{Counter, CounterUpDown, Flow, IMasterSlave, Stream, StreamFifo, StreamJoin, slave}
import spinalextras.lib.formal.StreamFormal.StreamExt
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalDataWithEquivalnce, FormalMasterSlave, FormalProperties, FormalProperty, StreamFormal, fillins}
import spinal.core._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Ax, Axi4B, Axi4Bus, Axi4Config, Axi4R, Axi4ReadOnly, Axi4Shared, Axi4W, Axi4WriteOnly}
import spinalextras.lib.misc.CounterVariableChange

import scala.collection.mutable
import scala.language.postfixOps
import scala.reflect.ClassTag

object Axi4Formal {
  def makeAxiCounter(a_fire: Bool, size: UInt, r: Bool) = {
    val outstanding = new CounterVariableChange(32)
    when(a_fire) {
      outstanding.increment_by(size +^ 1)
    }
    when(r) {
      outstanding.decrement_by(1)
    }

    Component.toplevel.addPrePopTask(() => {
      spinal.core.assume(!outstanding.willOverflow) // This is required for the inductive formal methods to work
    })

    outstanding
  }

  def createFifo[T <: Data](f : Flow[T], maxQueue : Int, latency : Int = 0) = {
    val fifoOverflow = Bool()
    val fifo = StreamFifo(cloneOf(f.payload), maxQueue, latency = latency)
    f.toStream(fifoOverflow) <> fifo.io.push
    assume(!fifoOverflow)

    fifo
  }

  def validFire[T <: Data](x: Stream[T]): Flow[T] = {
    val f = Flow(cloneOf(x.payload))
    val alreadyFired = RegInit(False) setWhen (x.isStall) clearWhen (x.fire)
    f.valid := x.valid && !alreadyFired
    f.payload := x.payload
    f
  }

  case class Axi4ReadStreamContract(config : Axi4Config, maxQueue : Int = 32) extends ComponentWithFormalProperties {
    val io = new Bundle {
      val arComplete = slave(Flow(UInt(config.lenWidth bits)))
      val r = slave(Flow(Bool()))

      val outstandingReads = out UInt(32 bits)
      val violation = out Bool()
    }

    val outstandingReads = makeAxiCounter(io.arComplete.fire, io.arComplete.payload, io.r.fire)
    io.outstandingReads := outstandingReads.value.asUInt.resized

    val counter = Counter(config.lenWidth bits, inc = io.r.fire)
    val arFifo = createFifo(io.arComplete, maxQueue, latency = 1)
    val rFifo = io.r.takeWhen(io.r.payload).translateWith(counter.value)

    when(io.r.fire && io.r.payload) {
      counter.clear()
    }

    val noArForRead, mismatchedLength = CombInit(False)
    io.violation := noArForRead || mismatchedLength

    when(io.r.fire) {
      noArForRead := arFifo.io.pop.valid === False
    }

    arFifo.io.pop.ready := rFifo.fire
    when(rFifo.fire) {
      mismatchedLength := arFifo.io.pop.payload =/= rFifo.payload
    } elsewhen(io.r.fire) {
      mismatchedLength := arFifo.io.pop.payload <= counter.valueNext
    }
  }

  class Axi4ReadContractEnforcer[T <: Axi4Ax](axType: HardType[T], config : Axi4Config, maxQueue : Int = 32) extends Component {
    val io = new Bundle {
      val arComplete = slave Flow(axType)
      val r = slave Flow(Axi4R(config))

      val outstandingReads = out UInt(32 bits)
    }

    var outstandingReads = U(0, 32 bits)
    // The spec allows AW and W to stream through independently. So we can have all the W beats first and then the
    // AW beats follow or vice versa. This means we have to track this.
    val individualStreamContracts = Array.fill(1 << config.idWidth)(Axi4ReadStreamContract(config))
    individualStreamContracts.zipWithIndex.foreach(x => {
      val (c, idx) = x
      c.io.arComplete.valid := io.arComplete.fire && idx === io.arComplete.id
      c.io.arComplete.payload := io.arComplete.len

      c.io.r.valid := io.r.valid && idx === io.r.id
      c.io.r.payload := io.r.last

      outstandingReads = outstandingReads + c.io.outstandingReads
    })
    io.outstandingReads := outstandingReads

    def formalIsConsumerValid() = new FormalProperties(this) {
      individualStreamContracts.zipWithIndex.foreach(c => {
        addFormalProperty(!c._1.io.violation, f"Read contract violation on ID ${c._2}")
      })
    }
  }

  class Axi4ReadContract[T <: Axi4Ax](bus: Nameable, ar: Stream[T], r: Stream[Axi4R], is_write: Bool = False) extends Composite(bus) {
    val enforcer = new Axi4ReadContractEnforcer(ar.payload, r.config)
    enforcer.setWeakName(s"axi_${bus.name}_read_enforcer")

    enforcer.io.arComplete <> ar.toFlowFire
    enforcer.io.r <> validFire(r)

    val outstandingReads = enforcer.io.outstandingReads

    def formalIsProducerValid() = new FormalProperties(bus) {
      addFormalProperties(StreamFormal.formalIsProducerValid(ar))
    }

    def formalIsConsumerValid() = new FormalProperties(bus) {
      addFormalProperties(StreamFormal.formalIsProducerValid(r))
      addFormalProperties(enforcer.formalIsConsumerValid())
    }
  }

  class Axi4WriteContractEnforcer[T <: Axi4Ax](axType: HardType[T], config : Axi4Config, maxQueue : Int = 32) extends ComponentWithFormalProperties {
    val io = new Bundle {
      val aw = slave Flow(axType)
      val awComplete = slave Flow(axType)

      val b = slave Flow(Axi4B(config))

      val w = slave Flow(Axi4W(config))
      val wComplete = slave Flow(Axi4W(config))
    }
    val (aw, w, b) = (io.aw, io.w, io.b)
    // The spec allows AW and W to stream through independently. So we can have all the W beats first and then the
    // AW beats follow or vice versa. This means we have to track this.

    val beatCounter = Counter(aw.len.maxValue, inc = w.fire)
    val beatLengthFifo = createFifo(w.takeWhen(w.last).translateWith(beatCounter.value), maxQueue)
    when(beatLengthFifo.io.push.fire) {
      beatCounter.clear()
    }
    assume(beatLengthFifo.io.push.ready)


    // When 'LAST' exists, it is legal to have FEWER beats than LEN but you can never have more beats than specified in
    // LEN. So we track AW lens and counted beats and compare them as we have the info for them. 128 is arbitrary but
    // it is unlikey that this limits our proving power realistically.
    val awLengthFifo = createFifo(aw, maxQueue)
    val beatStream = StreamJoin(awLengthFifo.io.pop, beatLengthFifo.io.pop)
    beatStream.freeRun()

    val writeLastCounters = Vec(RegInit(U(0, 8 bits)), 1 << config.idWidth)
    // Assume no overflows
    writeLastCounters.foreach(c => assume(c.getAheadValue() =/= 0 && c.andR))

    val lastCounterVerify = Flow(writeLastCounters.dataType)
    lastCounterVerify.setIdle()

    val awCompleteFifo = createFifo(io.awComplete, maxQueue)
    val wCompleteFifo = createFifo(io.wComplete, maxQueue)
    val awComplete = StreamJoin(awCompleteFifo.io.pop, wCompleteFifo.io.pop).toFlow.map(_._1)
    awComplete.freeRun()

    when(awComplete.fire) {
      writeLastCounters(awComplete.id) := writeLastCounters(awComplete.id) + 1
    }

    when(b.fire) {
      lastCounterVerify.valid := True
      lastCounterVerify.payload := writeLastCounters(b.id)
      writeLastCounters(b.id) := writeLastCounters(b.id) - 1
    }

    def formalIsProducerValid() = new FormalProperties(this) {
      when(beatStream.fire) {
        addFormalProperty(beatStream.payload._1.len >= beatStream.payload._2, "Beats mismatch -- sent more than specified")
      }

      when(lastCounterVerify.valid) {
        addFormalProperty(lastCounterVerify.payload =/= 0)
      }
    }

    def formalIsConsumerValid() = new FormalProperties(this) {
      when(lastCounterVerify.fire) {
        addFormalProperty(lastCounterVerify.payload =/= 0)
      }
    }
  }

  class Axi4WriteContract[T <: Axi4Ax](bus: Nameable, aw: Stream[T], w: Stream[Axi4W], b: Stream[Axi4B], is_write: Bool) extends Composite(bus, "writeContract") {
    require(w.payload.id == null)

    // Outstanding writes / write lasts can be negative per spec, but is tracked anyway
    val outstandingWrites = makeAxiCounter(aw.fire && is_write, aw.len, w.fire)
    val outstandingWriteLasts = makeAxiCounter(aw.fire && is_write, 0, w.fire && w.last)

    val outstandingWriteResponses = makeAxiCounter(w.fire && w.last, 0, b.fire)

    val enforcer = new Axi4WriteContractEnforcer(aw.payload, aw.config)
    enforcer.io.aw <> validFire(aw).takeWhen(is_write)
    enforcer.io.awComplete <> aw.toFlowFire.takeWhen(is_write)
    enforcer.io.wComplete <> w.toFlowFire

    enforcer.io.b <> validFire(b)
    enforcer.io.w <> validFire(w)

    def formalIsProducerValid() = new FormalProperties(bus) {
      Seq(aw, w).foreach(s => addFormalProperties(StreamFormal.formalIsProducerValid(s)))
      addFormalProperties(enforcer.formalIsProducerValid())
    }

    def formalIsConsumerValid() = new FormalProperties(bus) {
      when((outstandingWriteResponses) === 0) {
        addFormalProperty(b.valid === False, "B valid should not be valid when there are no responses outstanding")
      }
      addFormalProperties(enforcer.formalIsConsumerValid())
      addFormalProperties(StreamFormal.formalIsProducerValid(b))
      addFormalProperty(!outstandingWriteResponses.willUnderflow, "Outstanding write responses should not go negative")
      //addFormalProperty(!outstandingWriteLasts.willUnderflow, "Outstanding write lasts should not go negative")
    }
  }

  val readContracts = new mutable.WeakHashMap[Axi4Bus, Axi4ReadContract[_]]()
  val writeContracts = new mutable.WeakHashMap[Axi4Bus, Axi4WriteContract[_]]()

  object Axi4ReadContract {
    def apply(axi: Axi4Shared) = {
      new Axi4ReadContract(axi, axi.arw, axi.r, axi.arw.write)
    }

    def apply(axi: Axi4) = {
      new Axi4ReadContract(axi, axi.ar, axi.r)
    }

    def apply(axi: Axi4ReadOnly) = {
      new Axi4ReadContract(axi, axi.ar, axi.r)
    }
  }

  object Axi4WriteContract {
    def apply(axi: Axi4Shared) = {
      new Axi4WriteContract(axi, axi.arw, axi.w, axi.b, axi.arw.write)
    }

    def apply(axi: Axi4) = {
      new Axi4WriteContract(axi, axi.aw, axi.w, axi.b, True)
    }

    def apply(axi: Axi4WriteOnly) = {
      new Axi4WriteContract(axi, axi.aw, axi.w, axi.b, True)
    }
  }

  abstract class Axi4BaseExt(bus: Data with IMasterSlave) extends FormalMasterSlave with FormalDataWithEquivalnce[Axi4BaseExt] {
    def readContract: Axi4ReadContract[_]

    def writeContract: Axi4WriteContract[_]

    /**
     * @return True if and only if the driving signals are valid
     */
    override def formalIsProducerValid() = new FormalProperties(bus) {
      if (readContract != null) {
        addFormalProperties(readContract.formalIsProducerValid())
      }
      if (writeContract != null) {
        addFormalProperties(writeContract.formalIsProducerValid())
      }
    }

    /**
     * @return True if and only if the response signals are valid
     */
    override def formalIsConsumerValid() = new FormalProperties(bus) {
      if (readContract != null) {
        addFormalProperties(readContract.formalIsConsumerValid())
      }
      if (writeContract != null) {
        addFormalProperties(writeContract.formalIsConsumerValid())
      }
    }

    override def underlyingData: Data = bus

    override def asIMasterSlave: IMasterSlave = bus

    override def selfClassTag: ClassTag[Axi4BaseExt] = scala.reflect.classTag[Axi4BaseExt]

    override def formalAssertEquivalence(that: Axi4BaseExt): Unit = {
      if (readContract != null) {
        assert(this.readContract.outstandingReads === that.readContract.outstandingReads)
      }
      if (this.writeContract != null) {
        assert(this.writeContract.outstandingWrites === that.writeContract.outstandingWrites)
        assert(this.writeContract.outstandingWriteResponses === that.writeContract.outstandingWriteResponses)
      }
    }
  }

  implicit class Axi4SharedFormalExt(bus: Axi4Shared) extends Axi4BaseExt(bus) {
    def readContract = readContracts.getOrElseUpdate(bus, Axi4ReadContract(bus))

    def writeContract = writeContracts.getOrElseUpdate(bus, Axi4WriteContract(bus))
  }

  implicit class Axi4FormalExt(bus: Axi4) extends Axi4BaseExt(bus) {
    def readContract = readContracts.getOrElseUpdate(bus, Axi4ReadContract(bus))

    def writeContract = writeContracts.getOrElseUpdate(bus, Axi4WriteContract(bus))
  }


  implicit class Axi4ReadOnlyExt(bus: Axi4ReadOnly) extends Axi4BaseExt(bus) {
    def readContract = readContracts.getOrElseUpdate(bus, Axi4ReadContract(bus))

    def writeContract = null
  }

  implicit class Axi4WriteOnlyExt(bus: Axi4WriteOnly) extends Axi4BaseExt(bus) {
    def readContract = null

    def writeContract = writeContracts.getOrElseUpdate(bus, Axi4WriteContract(bus))
  }
}