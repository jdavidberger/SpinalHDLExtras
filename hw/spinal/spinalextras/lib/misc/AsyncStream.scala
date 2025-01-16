package spinalextras.lib.misc

import spinal.core._
import spinal.core.formal.HasFormalAsserts.assertOrAssume
import spinal.core.formal.past
import spinal.lib._

case class AsyncStream[T <: Data](val payloadType :  HardType[T]) extends Bundle with IMasterSlave {
  val async_valid, async_ready   = Bool()
  val flow = Flow(payloadType())

  def steady_ready() : AsyncStream[T] = new Composite(this, "steady_ready") {
    val s2mPipe = AsyncStream(payloadType)

    val oneAhead = RegInit(False) setWhen(async_fire) clearWhen(s2mPipe.async_fire && !async_fire)

    async_valid := !oneAhead

    flow << s2mPipe.flow.stage()

//    assert(
//      (formalContract.outstandingFlows.value) === (s2mPipe.formalContract.outstandingFlows.value + oneAhead.asUInt) ||
//        (formalContract.outstandingFlows.value) === (s2mPipe.formalContract.outstandingFlows.value)
//    )

    s2mPipe.async_ready := async_ready || oneAhead
  }.s2mPipe


  def s2mPipe(flush : Bool = null): AsyncStream[T] = new Composite(this) {
    val s2mPipe = AsyncStream(payloadType)

    val rValidN = RegInit(True) clearWhen(self.async_valid) setWhen(s2mPipe.async_ready)

    self.async_ready := rValidN

    s2mPipe.async_valid := self.async_valid || !rValidN
    s2mPipe.flow << self.flow.stage()

        assert(
          (formalContract.outstandingFlows.value) === (s2mPipe.formalContract.outstandingFlows.value + (!rValidN).asUInt)
        )

    if(flush != null) rValidN.setWhen(flush)
  }.s2mPipe

  def connectFrom(that: AsyncStream[T]): AsyncStream[T] = {
    this.async_valid := that.async_valid
    that.async_ready := this.async_ready
    this.flow << that.flow
    that
  }

  /** Connect that to this
   */
  def <<(that: AsyncStream[T]): AsyncStream[T] = connectFrom(that)

  /** Connect this to that
   */
  def >>(into: AsyncStream[T]): AsyncStream[T] = {
    into << this
    into
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

    val invalidReadyChange = wasReady && !async_ready && !wasFired
    invalidReadyChange.setWeakName(name + "_invalidReadyChange")
    assert(!invalidReadyChange, s"${this} deasserted async_ready before a async_valid")

    val outstandingFlows = CounterUpDown(1L << 32, async_fire, flow.valid)
    assume(~outstandingFlows.willOverflow)

    val async_flow_bounded = Bool()
    async_flow_bounded := outstandingFlows > 0 || ~outstandingFlows.decrementIt
    assert(async_flow_bounded) //, s"${pmb} PMB has miscounted responses")
  }

  def formalIsProducerValid(payloadInvariance : Boolean = true) : Bool = signalCache(s"${this}formalIsProducerValid")(new Composite(this, "formalIsProducerValid"){
    val v = formalContract.async_flow_bounded
  }.v)

  def formalIsConsumerValid() : Bool = new Composite(this) {
    val wasReady = RegNext(async_ready) init (False)
    val wasFired = RegNext(async_fire) init (False)
    val v = (!wasReady || async_ready || wasFired)
  }.v

  def formalIsValid() = {
    !formalContract.invalidReadyChange && formalContract.async_flow_bounded
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
