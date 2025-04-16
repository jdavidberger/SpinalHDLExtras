package spinalextras.lib.formal

import spinal.core._
import spinal.core.formal.past
import spinal.lib._
import spinal.lib.bus.wishbone.Wishbone

package object StreamFormal {
  def default_equivalence[T <: Data](a : T, b : T) : Bool = {
    a === b
  }
  def formalIsProducerValid[T <: Data](stream: Stream[T], payloadInvariance : Boolean = true,
                                       equivalence_check : (T, T) => Bool = default_equivalence _): Seq[FormalProperty] = new FormalProperties(stream) {
    val wasStall = past(stream.isStall) init(False)
    val checkValidHandshake = Mux(wasStall, stream.valid, True)
    addFormalProperty(checkValidHandshake, f"Dropped valid before saw ready")

    val priorValidPayload = RegNextWhen(stream.payload, stream.valid)
    val checkValidPayloadInvariance = Mux(wasStall && Bool(payloadInvariance),
      equivalence_check(priorValidPayload, stream.payload),
      True)
    addFormalProperty(checkValidPayloadInvariance, "Payload should not change while transfer is stalled")
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

