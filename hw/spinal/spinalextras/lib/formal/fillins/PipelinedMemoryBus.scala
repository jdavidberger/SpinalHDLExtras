package spinalextras.lib.formal.fillins

import spinal.core.Data
import spinal.lib.{CounterUpDown, IMasterSlave}
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusCmd}
import spinalextras.lib.bus.PipelinedMemoryBusExt
import spinalextras.lib.formal.StreamFormal.StreamExt
import spinalextras.lib.formal.{FormalDataWithEquivalnce, FormalMasterSlave, FormalProperties, FormalProperty, StreamFormal, fillins}
import spinal.core._

import scala.collection.mutable
import scala.reflect.ClassTag

object PipelinedMemoryBusFormal {
  def pmb_cmd_equivalence(a : PipelinedMemoryBusCmd, b : PipelinedMemoryBusCmd) = {
    def effective_tuple(a : PipelinedMemoryBusCmd) = {
      TupleBundle(Mux(a.write, a.data ## a.mask, B(0)), a.write, a.address)
    }

    effective_tuple(a) === effective_tuple(b)
  }

  class PipelinedMemoryBusContract(bus: PipelinedMemoryBus) extends FormalProperties(bus) {
    val outstandingReads = CounterUpDown(0x100000000L, incWhen = bus.readRequestFire, decWhen = bus.rsp.valid)
    assume(!outstandingReads.willOverflow) // This is required for the inductive formal methods to work
    val willUnderflow = outstandingReads.value === 0 && outstandingReads.decrementIt
    addFormalProperty(!willUnderflow, s"${bus.name} should not have has more rsp than reads")
  }

  val contracts = new mutable.WeakHashMap[PipelinedMemoryBus, PipelinedMemoryBusContract]()

  implicit class PipelinedMemoryBusFormalExt(bus: PipelinedMemoryBus) extends FormalMasterSlave with FormalDataWithEquivalnce[PipelinedMemoryBusFormalExt] {
    override def selfClassTag: ClassTag[PipelinedMemoryBusFormalExt] = scala.reflect.classTag[PipelinedMemoryBusFormalExt]

    /**
     * @return True if and only if the driving signals are valid
     */
    override def formalIsProducerValid(): Seq[FormalProperty] = {
      StreamFormal.formalIsProducerValid(bus.cmd, equivalence_check = pmb_cmd_equivalence)
    }

    def contract = contracts.getOrElseUpdate(bus, new PipelinedMemoryBusContract(bus))
    /**
     * @return True if and only if the response signals are valid
     */
    override def formalIsConsumerValid(): Seq[FormalProperty] = contract

    override def asIMasterSlave: IMasterSlave = bus

    override def formalAssertEquivalence(that: PipelinedMemoryBusFormalExt): Unit = {
      assert(this.contract.outstandingReads === that.contract.outstandingReads)
    }

  }
}
