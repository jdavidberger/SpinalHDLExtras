package spinalextras.lib.formal.fillins

import spinal.core._
import spinal.lib.StreamArbiter.{NoLock, RoundRobin}
import spinal.lib._
import spinalextras.lib.formal.StreamFormal.StreamExt
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties, FormalProperty, HasFormalProperties}

class StreamArbiterFormalProperties[T <: Data](val arbiter: StreamArbiter[T]) extends HasFormalProperties {
  override protected def formalProperties(): Seq[FormalProperty] = new FormalProperties(arbiter) {
    //if(arbiter.arbitrationPolicy != RoundRobin) {
    if(arbiter.maskLocked != null && arbiter.arbitrationPolicy == RoundRobin) {
      addFormalProperty(CountOne(arbiter.maskLocked) <= 1, "Locked mask should only have one valid bit set for round robin arbitration")
    }
    addFormalProperty(CountOne(arbiter.maskRouted) <= 1, "Routed mask should only have one valid bit set")
    //}
    if(arbiter.lockPolicy == NoLock) {
      arbiter.io.output.addFormalException(True)
    }
    if(arbiter.locked != null) {
      when(arbiter.locked) {
        //addFormalProperty(arbiter.io.inputs(OHToUInt(arbiter.maskLocked)).valid)
        //addFormalProperty(arbiter.io.output.valid, "Arbiter must be valid when locked")
      }
    }
  }
}