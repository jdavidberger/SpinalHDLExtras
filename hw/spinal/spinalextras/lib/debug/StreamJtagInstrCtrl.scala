package spinalextras.lib.debug

import spinal.core.{B, Data, when}
import spinal.lib.{Counter, Stream}
import spinal.lib.com.jtag.JtagTapInstructionCtrl

object StreamJtagInstrCtrl {
  def apply[T <: Data](stream : Stream[T]) : JtagTapInstructionCtrl = {
    val ctrl = new JtagTapInstructionCtrl

    val data = stream.payload ## stream.valid
    val counter = Counter(data.getBitsWidth)

    stream.ready := counter.willOverflow

    when(ctrl.enable) {
      when(ctrl.capture){
        counter.clear()
      }
      when(stream.valid && ctrl.shift) {
        counter.increment()
      }
    }
    ctrl.tdo := B(data)(counter)

    ctrl
  }
}
