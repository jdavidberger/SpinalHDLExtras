package spinalextras.lib.formal.fillins

import spinal.core._
import spinal.lib._
import spinal.lib.bus.simple.PipelinedMemoryBusArbiter
import spinalextras.lib.formal.fillins.PipelinedMemoryBusFormal.PipelinedMemoryBusFormalExt
import spinalextras.lib.formal.{FormalProperties, FormalProperty, HasFormalProperties}
import spinalextras.lib.misc.Complex.i
import spinalextras.lib.misc.StreamFifoExt

class PipelinedMemoryBusArbiterFormal(arbiter : PipelinedMemoryBusArbiter)  extends HasFormalProperties {
  override protected def formalProperties() = new FormalProperties {
    var fifo : StreamFifo[Bits] = null
    var rspRouteOh : Bits = null
    var internalArbiter : StreamArbiter[_] = null
    arbiter.walkComponents {
      case f: StreamFifo[Bits] => fifo = f
      case a: StreamArbiter[_] => internalArbiter = a
      case _ => {}
    }

    arbiter.logic.foreachReflectableNameables {
      case b: Bits =>
        if(b.name == "rspRouteOh") {
          rspRouteOh = b
        }
      case _ => {}
    }


      for (itm <- fifo.formalRawContents().zip(fifo.formalMask())) {
        when(itm._2) {
          addFormalProperty(itm._1.asUInt < arbiter.portCount, "Rsp queue has invalid entry")
        }
      }

      val counts = Vec((0 until arbiter.portCount).map(idx => fifo.formalCount(B(1 << idx, fifo.dataType.getBitsWidth bits))))

      for(i <- 0 until arbiter.portCount) {
        val isCurrentCmdOutput = internalArbiter.io.chosenOH(i)
        val hasStalled = arbiter.io.output.cmd.isStall
        val offset = U(0, 1 bit)
        when(hasStalled && isCurrentCmdOutput) {
          offset := 1
        }
        addFormalProperty(counts(i) ===
          arbiter.io.inputs(i).contract.outstandingReads, f"Rsp queue ${i} incidents should line up to outstanding reads")
      }

  }
}