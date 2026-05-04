package spinalextras.lib.formal.fillins

import spinal.core._
import spinal.lib._
import spinalextras.lib.formal.{FormalProperties, FormalProperty, HasFormalProperties}

class StreamForkFormal[T <: Data](val fork: StreamFork[T]) extends HasFormalProperties {
  override protected def formalProperties(): Seq[FormalProperty] = new FormalProperties(fork){
    if(fork.logic.linkEnable != null) {
      when(!fork.io.input.valid) {
        fork.logic.linkEnable.foreach(x => {
          addFormalProperty(x, "Link enables must be high if input is not valid")
        })
      }
    }
  }
}
