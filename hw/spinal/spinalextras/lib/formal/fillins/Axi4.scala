package spinalextras.lib.formal.fillins

import spinal.core.Data
import spinal.lib.{CounterUpDown, IMasterSlave, StreamFifo}
import spinalextras.lib.formal.StreamFormal.StreamExt
import spinalextras.lib.formal.{FormalDataWithEquivalnce, FormalMasterSlave, FormalProperties, FormalProperty, StreamFormal, fillins}
import spinal.core._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Ax}
import spinalextras.lib.misc.CounterVariableChange

import scala.collection.mutable
import scala.language.postfixOps
import scala.reflect.ClassTag

object Axi4Formal {

  class Axi4Contract(bus: Axi4) extends FormalProperties(bus) {
    def makeAxiCounter(a_fire : Bool, size: UInt, r : Bool) = {
      val outstanding = new CounterVariableChange(32)
      when(a_fire) {
        outstanding.increment(size + 1)
      }
      when(r) {
        outstanding.decrement(1)
      }

      assume(!outstanding.willOverflow) // This is required for the inductive formal methods to work
      outstanding
    }

    val outstandingReads = makeAxiCounter(bus.ar.fire, bus.ar.size, bus.r.fire)
    val outstandingWrites = makeAxiCounter(bus.aw.fire, bus.aw.size, bus.w.fire)
  }

  val contracts = new mutable.WeakHashMap[Axi4, Axi4Contract]()

  implicit class Axi4FormalExt(bus: Axi4) extends FormalMasterSlave with FormalDataWithEquivalnce[Axi4FormalExt] {
    override def selfClassTag: ClassTag[Axi4FormalExt] = scala.reflect.classTag[Axi4FormalExt]

    /**
     * @return True if and only if the driving signals are valid
     */
    override def formalIsProducerValid() = new FormalProperties {
      Seq(bus.aw, bus.ar, bus.w).foreach(s => addFormalProperties(StreamFormal.formalIsProducerValid(s)))
    }

    def contract = contracts.getOrElseUpdate(bus, new Axi4Contract(bus))
    /**
     * @return True if and only if the response signals are valid
     */
    override def formalIsConsumerValid() = new FormalProperties {
      Seq(bus.b, bus.r).foreach(s => addFormalProperties(StreamFormal.formalIsProducerValid(s)))

      addFormalProperty(!contract.outstandingReads.willUnderflow, "Outstanding reads should not go negative")
      addFormalProperty(!contract.outstandingWrites.willUnderflow, "Outstanding writes should not go negative")
    }

    override def asIMasterSlave: IMasterSlave = bus

    override def formalAssertEquivalence(that: Axi4FormalExt): Unit = {
      assert(this.contract.outstandingReads === that.contract.outstandingReads)
    }

  }
}