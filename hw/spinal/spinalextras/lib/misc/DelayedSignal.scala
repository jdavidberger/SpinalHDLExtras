package spinalextras.lib.misc

import spinal.core._

import scala.language.postfixOps

case class DelayedSignal(latency : Int, crossClockDomain : Boolean = false) extends Component {
  setDefinitionName(s"DelayedSignal${if(crossClockDomain) "CC" else ""}_l${latency}", false)

  require(latency >= 0)

  val io = new Bundle {
    val input = in(Bool())
    val output = out(Bool())
    val busy = out(Bool())

    val pipe = out(Bits(latency bits))

    val clear = in(Bool()) default(False)
  }

  val pipe = Reg(Bits((latency) bits)) init(0)
  if(crossClockDomain) {
    pipe.addTag(spinal.core.crossClockDomain)
  }
  pipe := (pipe |<< 1) | io.input.asBits.resized
  when(io.clear) {
    pipe := io.input.asBits.resized
  }
  io.output := pipe.msb
  io.pipe := pipe
  io.busy := pipe =/= 0
}