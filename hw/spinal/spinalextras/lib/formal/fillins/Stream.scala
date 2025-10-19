package spinalextras.lib.formal

import spinal.core._
import spinal.core.formal.past
import spinal.lib._
import spinal.lib.bus.wishbone.Wishbone
import spinalextras.lib.formal.fillins.EquivalenceRegistry

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.{ClassTag, classTag}

package object StreamFormal {

  def formalIsProducerValid[T <: Data](stream: Stream[T], payloadInvariance : Boolean = true, formalExceptionalState : Bool): Seq[FormalProperty] = new FormalProperties(stream) {
    val wasStall = past(stream.isStall) init(False)
    val checkValidHandshake = Mux(wasStall, stream.valid, True)

    when(!formalExceptionalState) {
      addFormalProperty(checkValidHandshake, f"Dropped valid before saw ready")

      val priorValidPayload = RegNextWhen(stream.payload, stream.valid)
      if (payloadInvariance) {
        val checkValidPayloadInvariance = Mux(wasStall,
          EquivalenceRegistry.Check(priorValidPayload, stream.payload),
          True)
        addFormalProperty(checkValidPayloadInvariance, s"Payload should not change while transfer is stalled")
      }

      when(stream.valid) {
        addFormalProperties(FormalData.formalIsStateValid(stream.payload))
      }
    }
  }

  class StreamContract(n : Nameable) {
    val _formalExceptionalState = Bool()
    _formalExceptionalState := False

    val payloadInvariance = true

    val exceptions = new ArrayBuffer[Bool]()

    def addFormalException(b : Bool): Unit = {
      exceptions.append(b)
      when(b) {
        _formalExceptionalState := True
      }
    }

    def formalExceptionalState : Bool = {
      Vec(exceptions).orR.setWeakName(f"${n.name}_formalExceptionalState")
    }

    def formalAssertEquivalence(that: StreamContract): Unit = {
      this.exceptions.appendAll(that.exceptions)
      that.exceptions.appendAll(this.exceptions)
    }
  }

  var contracts = new mutable.HashMap[Stream[_], StreamContract]()

  implicit class StreamExt[T <: Data](stream: Stream[T]) extends FormalMasterSlave with FormalDataWithEquivalnce[StreamExt[T]] {

    lazy val contract = contracts.getOrElseUpdate(stream, new StreamContract(stream))


    def addFormalException(b : Bool) = {
      contract.addFormalException(b)
      stream
    }

    /**
     * @return True if and only if the driving signals are valid
     */
    override def formalIsProducerValid(): Seq[FormalProperty] = StreamFormal.formalIsProducerValid(stream, contract.payloadInvariance, contract.formalExceptionalState)


    override def asIMasterSlave: IMasterSlave = stream

    /**
     * When a bundle has a formal contract which can not be tied directly to it's current state, it is sometimes necessary
     * to assert that two objects have a functionally equivalent internal state.
     *
     * As a concrete example, a pipelined bus will typically have an assertion that it can't receive more data than was
     * requested and so there is an outstanding replies counter which must be part of the busses formal contract. This
     * function would be called when you wanted to ensure that another bus has the exact smae oustanding replies counter.
     *
     * This is typically required for inductive methods which often must be told this, even if the components share
     * all of the same signals.
     *
     * @param that The object to assert equivalence with.
     */
    override def formalAssertEquivalence(that: StreamExt[T]): Unit = {
      contract.formalAssertEquivalence(that.contract)
    }

    override def selfClassTag: ClassTag[StreamExt[T]] = classTag[StreamExt[T]]
  }

  EquivalenceRegistry.AddEquivalenceHandler { case (a : Stream[Data], b : Stream[Data]) => {
    def cleanSignal(x : Stream[Data]) = {
      Mux(x.valid, x.payload.asBits, B(0, x.payload.getBitsWidth bits)) ## x.valid
    }

    cleanSignal(a) === cleanSignal(b)
  } }

  }

