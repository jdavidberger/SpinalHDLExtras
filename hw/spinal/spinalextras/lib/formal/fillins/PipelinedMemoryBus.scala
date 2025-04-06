package spinalextras.lib.formal.fillins

import spinal.core.Data
import spinal.lib.{CounterUpDown, IMasterSlave}
import spinal.lib.bus.simple.PipelinedMemoryBus
import spinalextras.lib.bus.PipelinedMemoryBusExt
import spinalextras.lib.formal.StreamFormal.StreamExt
import spinalextras.lib.formal.{FormalMasterSlave, FormalProperties, FormalProperty, fillins}
import spinal.core._

import scala.collection.mutable

object PipelinedMemoryBusFormal {
  class PipelinedMemoryBusContract(bus: PipelinedMemoryBus) extends FormalProperties {
    val outstandingReads = CounterUpDown(0x100000000L, incWhen = bus.readRequestFire, decWhen = bus.rsp.valid)
    assume(!outstandingReads.willOverflow) // This is required for the inductive formal methods to work

    val willUnderflow = outstandingReads.value === 0 && outstandingReads.decrementIt
    addFormalProperty(!willUnderflow, s"${bus.name} should not have has more rsp than reads")
  }

  val contracts = new mutable.WeakHashMap[PipelinedMemoryBus, PipelinedMemoryBusContract]()

  implicit class PipelinedMemoryBusFormalExt(bus: PipelinedMemoryBus) extends FormalMasterSlave {

    /**
     * @return True if and only if the driving signals are valid
     */
    override def formalIsProducerValid(): Seq[FormalProperty] = {
      contracts.getOrElseUpdate(bus, new PipelinedMemoryBusContract(bus)) ++ bus.cmd.formalIsProducerValid
    }

    override def asIMasterSlave: IMasterSlave = bus
  }
}
