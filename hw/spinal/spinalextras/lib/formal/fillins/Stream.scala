package spinalextras.lib.formal

import spinal.core._
import spinal.core.formal.past
import spinal.lib._
import spinal.lib.bus.wishbone.Wishbone
import spinalextras.lib.formal.fillins.EquivalenceRegistry

import scala.collection.mutable

package object StreamFormal {

  def formalIsProducerValid[T <: Data](stream: Stream[T], payloadInvariance : Boolean = true): Seq[FormalProperty] = new FormalProperties(stream) {
    val wasStall = past(stream.isStall) init(False)
    val checkValidHandshake = Mux(wasStall, stream.valid, True)
    addFormalProperty(checkValidHandshake, f"Dropped valid before saw ready")

    val priorValidPayload = RegNextWhen(stream.payload, stream.valid)
    if(payloadInvariance) {
      val checkValidPayloadInvariance = Mux(wasStall,
        EquivalenceRegistry.Check(priorValidPayload, stream.payload),
        True)
      addFormalProperty(checkValidPayloadInvariance, s"Payload should not change while transfer is stalled")
    }

  }

  implicit class StreamExt[T <: Data](stream: Stream[T]) extends FormalMasterSlave {
    val payloadInvariance = true

    /**
     * @return True if and only if the driving signals are valid
     */
    override def formalIsProducerValid(): Seq[FormalProperty] = StreamFormal.formalIsProducerValid(stream)

    override def asIMasterSlave: IMasterSlave = stream
  }
}

