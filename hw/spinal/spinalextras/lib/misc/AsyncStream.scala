package spinalextras.lib.misc

import spinal.core._
import spinal.lib._

case class AsyncStream[T <: Data](val payloadType :  HardType[T]) extends Bundle with IMasterSlave {
  val async_valid, async_ready   = Bool()
  val flow = Flow(payloadType())

  override def asMaster(): Unit = {
    out(async_valid)
    in(async_ready)
    master(flow)
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
  def map[T2 <: Data](translate: (T) => T2) = (this ~ translate(this.flow.payload))

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
      val overflow = counter.willOverflowIfInc && counter.incrementIt && ~counter.decrementIt
      val underflow = counter.value === 0 && counter.decrementIt && ~counter.incrementIt
      assert(~overflow, s"allow_outstanding overflow ${self.getRtlPath()}")
      assert(~underflow, s"allow_outstanding underflow ${self.getRtlPath()}")
      async_ready := ~counter.willOverflowIfInc
    }
  }

  def freeRun() = {
    this.async_ready := True
    this
  }
}
