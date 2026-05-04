package spinalextras.lib.formal.fillins

import spinal.core._
import spinal.lib._
import spinal.lib.bus.simple.PipelinedMemoryBusArbiter
import spinalextras.lib.formal.StreamFormal.StreamExt
import spinalextras.lib.formal.fillins.PipelinedMemoryBusFormal.PipelinedMemoryBusFormalExt
import spinalextras.lib.formal.{FormalProperties, FormalProperty, HasFormalProperties}
import spinalextras.lib.misc.Complex.i
import spinalextras.lib.misc.StreamFifoExt

class PipelinedMemoryBusArbiterFormal(arbiter: PipelinedMemoryBusArbiter) extends HasFormalProperties {
  lazy val _formalProperties = new FormalProperties(arbiter) {
    var fifo: StreamFifo[Bits] = null
    var internalArbiter: StreamArbiter[Data] = null

    arbiter.walkComponents {
      case f: StreamFifo[Bits] => fifo = f
      case a: StreamArbiter[Data] => internalArbiter = a
      case _ => {}
    }

    if(fifo != null) {
      for (itm <- fifo.formalRawContents().zip(fifo.formalMask())) {
        when(itm._2) {
          val oh_index = OHToUInt(itm._1)
          if (oh_index.getWidth > log2Up(arbiter.portCount)) {
            addFormalProperty(OHToUInt(itm._1) < arbiter.portCount, "Rsp queue has invalid entry")
          }
          addFormalProperty(CountOne(itm._1) === 1, "Rsp queue has invalid entry. Should all be one bit")
        }
      }

      // There is a sort of large state space the internals of the arbiter can be in; there is an async StreamFork which
      // gets split to the output cmd and the rsp queue. We enumerate these possibilities here
      val fifoStalled = CombInit(fifo.io.push.valid)
      val inputStreamStalled = arbiter.io.inputs(OHToUInt(internalArbiter.io.chosenOH)).cmd.valid && !arbiter.io.inputs(OHToUInt(internalArbiter.io.chosenOH)).cmd.write
      val outputStreamStalled = CombInit(arbiter.io.output.cmd.valid && !arbiter.io.output.cmd.write)

      val minus_one_formal_per_input = inputStreamStalled && !fifoStalled
      (0 until arbiter.portCount).foreach(idx => {
        val entry = B(1 << idx, fifo.dataType.getBitsWidth bits)
        addFormalProperty((fifo.formalCount(entry) -^ (internalArbiter.io.chosenOH(idx) && minus_one_formal_per_input).asUInt) === arbiter.io.inputs(idx).contract.outstandingReads, "Oustanding read mismatch")
      })

      addFormalProperty(fifoStalled === False || inputStreamStalled, "Fifo stream can not be stalled unless input stream is too.")
      when(!fifoStalled && !inputStreamStalled) {
        addFormalProperty(fifo.io.occupancy === arbiter.io.output.contract.outstandingReads, "Sum of outstanding reads should match rsp queue")
      } elsewhen (fifoStalled && inputStreamStalled) {
        addFormalProperty((fifo.io.occupancy +^ ((!outputStreamStalled).asUInt)) === arbiter.io.output.contract.outstandingReads, "Sum of outstanding reads should match rsp queue")
      } elsewhen (inputStreamStalled) {
        addFormalProperty(fifo.formalCheckLastPush(x => internalArbiter.io.chosenOH === x), "Stream stalled must be fifo last push")
        addFormalProperty(fifo.io.occupancy === (arbiter.io.output.contract.outstandingReads +^ 1), "Sum of outstanding reads should match rsp queue")
      }
    }

    require(arbiter.transactionLock, "Formal proofs with no transaction lock do not work yet")
    if(!arbiter.transactionLock) {
      arbiter.io.output.cmd.contract.addFormalPayloadInvarianceException(True)
      StreamExt(internalArbiter.io.output).contract.addFormalPayloadInvarianceException(True)
    }
  }

  override protected def formalProperties() = _formalProperties
}