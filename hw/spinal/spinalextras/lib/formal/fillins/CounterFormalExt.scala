package spinalextras.lib.formal.fillins

import spinal.lib.Counter
import spinalextras.lib.formal.{FormalProperties, FormalProperty, HasFormalProperties}

case class CounterFormalExt(counter : Counter) extends HasFormalProperties {
  override protected def formalProperties(): Seq[FormalProperty] = new FormalProperties(counter) {
    addFormalProperty(counter.value <= counter.end)
  }
}