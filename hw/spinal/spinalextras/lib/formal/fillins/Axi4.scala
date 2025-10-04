package spinalextras.lib.formal.fillins

import spinal.core.Component.push
import spinal.core.Data
import spinal.lib.{Counter, CounterUpDown, IMasterSlave, Stream, StreamFifo, StreamJoin}
import spinalextras.lib.formal.StreamFormal.StreamExt
import spinalextras.lib.formal.{FormalDataWithEquivalnce, FormalMasterSlave, FormalProperties, FormalProperty, StreamFormal, fillins}
import spinal.core._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Ax, Axi4B, Axi4Bus, Axi4R, Axi4ReadOnly, Axi4Shared, Axi4W, Axi4WriteOnly}
import spinalextras.lib.misc.CounterVariableChange

import scala.collection.mutable
import scala.language.postfixOps
import scala.reflect.ClassTag

object Axi4Formal {
  def makeAxiCounter(a_fire : Bool, size: UInt, r : Bool) = {
    val outstanding = new CounterVariableChange(32)
    when(a_fire) {
      outstanding.increment_by(size +^ 1)
    }
    when(r) {
      outstanding.decrement_by(1)
    }

    assume(!outstanding.willOverflow) // This is required for the inductive formal methods to work
    outstanding
  }

  class Axi4ReadContract[T <: Axi4Ax](bus: Nameable, ar : Stream[T], r : Stream[Axi4R], is_write : Bool = False) extends Composite(bus) {
    val outstandingReads = makeAxiCounter(ar.fire && !is_write, ar.len, r.fire)

    def formalIsProducerValid() = new FormalProperties(bus) {
      addFormalProperties(StreamFormal.formalIsProducerValid(ar))
    }

    def formalIsConsumerValid() = new FormalProperties(bus) {
      addFormalProperties(StreamFormal.formalIsProducerValid(r))

      when(outstandingReads === 0) {
        addFormalProperty(r.valid === False, "R stream should not be valid when there are no reads outstanding")
      }

      addFormalProperty(!outstandingReads.willUnderflow, "Outstanding reads should not go negative")
    }
  }

  class Axi4WriteContract[T <: Axi4Ax](bus: Nameable, aw : Stream[T], w : Stream[Axi4W], b : Stream[Axi4B], is_write : Bool) extends Composite(bus, "writeContract") {
    // Outstanding writes / write lasts can be negative per spec, but is tracked anyway
    val outstandingWrites = makeAxiCounter(aw.fire && is_write, aw.len, w.fire)
    val outstandingWriteLasts = makeAxiCounter(aw.fire && is_write, 0, w.fire && w.last)

    val outstandingWriteResponses = makeAxiCounter(w.fire && w.last, 0, b.fire)

    // The spec allows AW and W to stream through independently. So we can have all the W beats first and then the
    // AW beats follow or vice versa. This means we have to track this.

    // When 'LAST' exists, it is legal to have FEWER beats than LEN but you can never have more beats than specified in
    // LEN. So we track AW lens and counted beats and compare them as we have the info for them. 128 is arbitrary but
    // it is unlikey that this limits our proving power realistically.
    val awLengthFifoOverflow = Bool()
    val awLengthFifo = StreamFifo(cloneOf(aw.len), 128, latency = 0)
    aw.toFlowFire.takeWhen(is_write).map(_.len).toStream(awLengthFifoOverflow) <> awLengthFifo.io.push
    assume(!awLengthFifoOverflow)

    val beatCounter = Counter(aw.len.maxValue, inc = w.fire)
    val beatLengthFifo = StreamFifo(cloneOf(aw.len), 128, latency = 0)
    beatLengthFifo.io.push.payload := beatCounter
    beatLengthFifo.io.push.valid := w.last && w.fire
    when(beatLengthFifo.io.push.fire) {
      beatCounter.clear()
    }
    assume(beatLengthFifo.io.push.ready)

    val beatStream = StreamJoin(awLengthFifo.io.pop, beatLengthFifo.io.pop)
    beatStream.freeRun()

    def formalIsProducerValid() = new FormalProperties(bus) {
      Seq(aw, w).foreach(s => addFormalProperties(StreamFormal.formalIsProducerValid(s)))

      when(beatStream.fire) {
        addFormalProperty(beatStream.payload._1 > beatStream.payload._2, "Beats mismatch -- sent more than specified")
      }

      when(beatLengthFifo.io.occupancy === 0 && awLengthFifo.io.pop.valid) {
        addFormalProperty(awLengthFifo.io.pop.payload > beatCounter, "Beats mismatch -- exceeded beats for releated AW command")
      }
    }

    def formalIsConsumerValid() = new FormalProperties(bus) {
      when((outstandingWriteResponses) === 0) {
        addFormalProperty(b.valid === False, "B valid should not be valid when there are no responses outstanding")
      }

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

  abstract class Axi4BaseExt(bus : Data with IMasterSlave) extends FormalMasterSlave with FormalDataWithEquivalnce[Axi4BaseExt] {
    def readContract : Axi4ReadContract[_]
    def writeContract : Axi4WriteContract[_]

    /**
     * @return True if and only if the driving signals are valid
     */
    override def formalIsProducerValid() = new FormalProperties(bus) {
      if(readContract != null) {
        addFormalProperties(readContract.formalIsProducerValid())
      }
      if(writeContract != null) {
        addFormalProperties(writeContract.formalIsProducerValid())
      }
    }

    /**
     * @return True if and only if the response signals are valid
     */
    override def formalIsConsumerValid() = new FormalProperties(bus) {
      if(readContract != null) {
        addFormalProperties(readContract.formalIsConsumerValid())
      }
      if(writeContract != null) {
        addFormalProperties(writeContract.formalIsConsumerValid())
      }
    }

    override def underlyingData: Data = bus
    override def asIMasterSlave: IMasterSlave = bus
    override def selfClassTag: ClassTag[Axi4BaseExt] = scala.reflect.classTag[Axi4BaseExt]

    override def formalAssertEquivalence(that: Axi4BaseExt): Unit = {
      if(readContract != null) {
        assert(this.readContract.outstandingReads === that.readContract.outstandingReads)
      }
      if(this.writeContract != null) {
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