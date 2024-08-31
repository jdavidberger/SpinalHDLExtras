package spinalextras.lib.misc

import spinal.core._
import spinal.lib._

object RateLimitFlow {
  def apply[T <: Data](cycles : BigInt, flow : Flow[T]): Flow[T] = {
    val timeout = Timeout(cycles)
    val flowOut = flow.clone()
    val flowReg = RegNextWhen(flow, flow.valid)
    flowOut.payload := flowReg.payload
    flowOut.valid := False
    when(timeout && flowReg.valid) {
      flowOut.valid := True
      flowReg.valid := False
      timeout.clear()
    }
    flowOut
  }

  def apply[T <: Data](time : TimeNumber, flow : Flow[T]): Flow[T] = {
    apply((time * ClockDomain.current.frequency.getValue).toBigInt, flow)
  }
}