package spinalextras.lib.formal

import spinal.core.Component.push
import spinal.core._
import spinal.core.formal.past
import spinal.lib._
import spinalextras.lib.formal.fillins.EquivalenceRegistry

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.{ClassTag, classTag}

package object StreamFormal {

  def formalIsProducerValid[T <: Data](stream: Stream[T], formalPayloadInvarianceExceptionalState : Bool, formalExceptionalState : Bool): Seq[FormalProperty] = new FormalProperties(stream) {
    val wasStall = past(stream.isStall) init(False)
    val checkValidHandshake = Mux(wasStall, stream.valid, True)

    val priorValidPayload = RegNextWhen(stream.payload, stream.valid)
    val checkValidPayloadInvariance = Mux(wasStall,
      EquivalenceRegistry.Check(priorValidPayload, stream.payload),
      True)

    when(!formalExceptionalState) {
      addFormalProperty(checkValidHandshake, f"Dropped valid before saw ready")

      addFormalProperty(formalPayloadInvarianceExceptionalState || checkValidPayloadInvariance, s"Payload should not change while transfer is stalled")

      when(stream.valid) {
        addFormalProperties(FormalData.formalIsStateValid(stream.payload))
      }
    }
  }

  class StreamContractGroup (val n : Nameable) {
    val _formalExceptionalState, _formalPayloadInvarianceExceptionalState = {
      val r = push(Component.toplevel)
      val rtn = Bool()
      rtn := False
      r.restore()
      rtn
    }
    _formalExceptionalState.setPartialName(n, "formalExceptionalState", true)
    _formalPayloadInvarianceExceptionalState.setPartialName(n, "formalPayloadInvarianceExceptionalState", true)

    var parent : StreamContractGroup = null

    val exceptions, payloadInvarianceExceptions = new ArrayBuffer[Bool]()

    def addFormalPayloadInvarianceException(b : Bool): Unit = {
      payloadInvarianceExceptions.append(b)
      val r = push(Component.toplevel)
      when(b) {
        _formalPayloadInvarianceExceptionalState := True
      }
      r.restore()
    }

    def addFormalException(b : Bool): Unit = {
      exceptions.append(b)
      val restore = push(Component.toplevel)
      when(b) {
        _formalExceptionalState := True
      }
      restore.restore()
    }

    def setParent(r : StreamContractGroup): StreamContractGroup = {
      parent = r
      val restore = push(Component.toplevel)
      when(r._formalExceptionalState) { _formalExceptionalState := True}
      when(r._formalPayloadInvarianceExceptionalState) { _formalPayloadInvarianceExceptionalState := True}
      restore.restore()
      r
    }

    def root : StreamContractGroup = if (parent == null) this else {
      setParent(parent.root)
    }

    def merge(group : StreamContractGroup): Unit = {
      //println(s"Merging ${n.name} and ${group.n.name}")

      group.exceptions.foreach(this.addFormalException)
      group.payloadInvarianceExceptions.foreach(this.addFormalPayloadInvarianceException)

      group.setParent(this)
    }
  }

  class StreamContract(n : Nameable) {
    var streamContractGroup : StreamContractGroup = new StreamContractGroup(n)

    //def exceptions = streamContractGroup.root.exceptions
    //def payloadInvarianceExceptions = streamContractGroup.root.payloadInvarianceExceptions

    def formalExceptionalState : Bool = {
      streamContractGroup._formalExceptionalState
    }

    def formalPayloadInvarianceExceptionalState : Bool = {
      streamContractGroup._formalPayloadInvarianceExceptionalState
    }

    def addFormalPayloadInvarianceException(b : Bool): Unit = {
      streamContractGroup.root.addFormalPayloadInvarianceException(b)
    }

    def addFormalException(b : Bool): Unit = {
      streamContractGroup.root.addFormalException(b)
    }

    def formalAssertEquivalence(that: StreamContract): Unit = {
      that.streamContractGroup.merge(this.streamContractGroup)
    }
  }

  var contracts = new mutable.HashMap[Stream[_], StreamContract]()

  implicit class StreamExt[T <: Data](stream: Stream[T]) extends FormalMasterSlave with FormalDataWithEquivalnce[StreamExt[T]] {

    lazy val contract = contracts.getOrElseUpdate(stream, new StreamContract(stream))

    def addFormalPayloadInvarianceException(b : Bool = True) = {
      contract.addFormalPayloadInvarianceException(b)
      stream
    }

    def addFormalException(b : Bool) = {
      contract.addFormalException(b)
      stream
    }

    /**
     * @return True if and only if the driving signals are valid
     */
    override def formalIsProducerValid(): Seq[FormalProperty] = StreamFormal.formalIsProducerValid(stream,
      contract.formalPayloadInvarianceExceptionalState, contract.formalExceptionalState)


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

