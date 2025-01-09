package spinalextras.lib.misc

import spinal.core._
import spinal.core.formal.HasFormalAsserts.assertOrAssume
import spinal.core.formal.past
import spinal.lib._

case class AsyncStream[T <: Data](val payloadType :  HardType[T]) extends Bundle with IMasterSlave {
  val async_valid, async_ready   = Bool()
  val flow = Flow(payloadType())

  def steady_ready() : AsyncStream[T] = {
    val s2mPipe = AsyncStream(payloadType)

    val oneAhead = RegInit(False) setWhen(async_fire) clearWhen(s2mPipe.async_fire && !async_fire)

    async_valid := !oneAhead

    flow << s2mPipe.flow

    s2mPipe.async_ready := async_ready || oneAhead
    s2mPipe
  }

  override def asMaster(): Unit = {
    out(async_valid)
    master(flow)

    in(async_ready)
  }

  def arbitrationFrom[T2 <: Data](that : AsyncStream[T2]) : Unit = {
    this.flow.valid := that.flow.valid
    this.async_valid := that.async_valid
    that.async_ready := this.async_ready
  }

  def translateWith[T2 <: Data](that: T2) = {
    val next = new AsyncStream(that)
    next.arbitrationFrom(this)
    next.flow.payload := that
    next
  }

  def ~[T2 <: Data](that: T2) = translateWith(that)
  def ~~[T2 <: Data](translate: (T) => T2) = map(translate)
  def map[T2 <: Data](translate: (T) => T2) = {
    val mappedStream = (this ~ translate(this.flow.payload))
    assert(mappedStream.formalContract.outstandingFlows === formalContract.outstandingFlows)
    mappedStream
  }

  def async_fire = async_valid && async_ready

  def toStream = {
    val stream = Stream(payloadType)
    stream.payload := flow.payload
    stream.valid := flow.valid
    stream
  }

  def setIdle() = {
    flow.setIdle()
    async_valid := False
    this
  }

  def payload = flow.payload

  def valid = flow.valid
  def fire = flow.fire
  def async_stall = async_valid && !async_ready

  override def clone = AsyncStream(payloadType)

  def allow_outstanding(outstanding : Int, decWhen : Bool = flow.valid): Area = {
    val self = this
    new Area {
      val counter = new CounterUpDown(outstanding + 1, handleOverflow = false)
      when(async_valid) {
        counter.increment()
      }
      when(decWhen) {
        counter.decrement()
      }
      assert(counter.value === formalContract.outstandingFlows.value)
      val overflow = counter.willOverflowIfInc && counter.incrementIt && ~counter.decrementIt
      val underflow = counter.value === 0 && counter.decrementIt && ~counter.incrementIt
      assert(~overflow, s"allow_outstanding overflow ${self.getRtlPath()}")
      assert(~underflow, s"allow_outstanding underflow ${self.getRtlPath()}")
      async_ready := ~counter.willOverflowIfInc
    }
  }

  lazy val formalContract = new Composite(this, "formalContract") {
    val wasValid = RegNext(async_valid) init (False)
    val wasReady = RegNext(async_ready) init (False)
    val wasFired = RegNext(async_fire) init (False)

    val invalidValidChange = wasValid && !async_valid && !wasFired
    invalidValidChange.setWeakName(name + "_invalidValidChange")
    assert(!invalidValidChange, s"${this} deasserted async_valid before a async_ready")

    val outstandingFlows = CounterUpDown(1L << 32, async_fire, flow.valid)
    assume(~outstandingFlows.willOverflow)

    val async_flow_bounded = Bool()
    async_flow_bounded := outstandingFlows > 0 || ~outstandingFlows.decrementIt
    assert(async_flow_bounded) //, s"${pmb} PMB has miscounted responses")
  }

  def formalIsProducerValid(payloadInvariance : Boolean = true) : Bool = signalCache(s"${this}formalIsProducerValid")(new Composite(this, "formalIsProducerValid"){
    val wasValid = RegNext(async_valid) init (False)
    val wasFired = RegNext(async_fire) init (False)

    val v = (!wasValid || async_valid || wasFired)
  }.v)

  def formalIsConsumerValid() : Bool = formalContract.async_flow_bounded

  def formalIsValid() = {
    !formalContract.invalidValidChange && formalContract.async_flow_bounded
  }

  def formalAsserts()(implicit useAssumes : Boolean = false): Unit = {
    assertOrAssume(formalIsProducerValid())
    assertOrAssume(formalIsConsumerValid())
  }

  def formalDriverAssumptions(): Unit = {
    when(past(async_stall)) {
      assume(async_valid)
    }
    when(formalContract.outstandingFlows === 0) {
      assume(!flow.valid)
    }
  }

  def freeRun() = {
    this.async_ready := True
    this
  }
}
