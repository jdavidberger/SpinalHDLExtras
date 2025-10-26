package spinalextras.lib.formal.fillins

import spinal.core._
import spinal.lib._
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties, FormalProperty, HasFormalProperties}

class StreamArbiterFormalProperties[T <: Data](val arbiter: StreamArbiter[T]) extends HasFormalProperties {
  override protected def formalProperties(): Seq[FormalProperty] = new FormalProperties(arbiter) {
    when(arbiter.locked) {
      addFormalProperty(arbiter.io.inputs(OHToUInt(arbiter.maskLocked)).valid)
      addFormalProperty(arbiter.io.output.valid)
    }
  }
}