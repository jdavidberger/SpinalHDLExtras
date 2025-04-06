package spinalextras.lib.formal

import spinal.core._
import spinal.core.formal.past
import spinal.lib._
import spinal.lib.bus.wishbone.Wishbone

package object StreamFormal {
  implicit class StreamExt[T <: Data](stream: Stream[T]) extends FormalMasterSlave {
    val payloadInvariance = true

    /**
     * @return True if and only if the driving signals are valid
     */
    override def formalIsProducerValid(): Seq[FormalProperty] = new FormalProperties {
      val wasStall = past(stream.isStall) init(False)
      val checkValidHandshake = Mux(wasStall, stream.valid, True)
      addFormalProperty(checkValidHandshake)

      val priorValidPayload = RegNextWhen(stream.payload, stream.valid)
      val checkValidPayloadInvariance = Mux(wasStall && Bool(payloadInvariance),
        priorValidPayload === stream.payload,
        True)
      addFormalProperty(checkValidPayloadInvariance)
    }

    override def asIMasterSlave: IMasterSlave = stream
  }
}

