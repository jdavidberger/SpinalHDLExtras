package spinalextras.lib.debug

import spinal.core.{B, Bits, Bool, Data, False, IntToBuilder, Reg, RegInit, True, when}
import spinal.lib.{Counter, Flow, Stream}
import spinal.lib.com.jtag.JtagTapInstructionCtrl

object StreamJtagInstrCtrl {
  def apply[T <: Data](streamOut : Stream[T], streamIn : Flow[T]) : JtagTapInstructionCtrl = {
    val ctrl = new JtagTapInstructionCtrl

    {
      val data = streamOut.payload ## streamOut.valid
      val counter = Counter(data.getBitsWidth)

      streamOut.ready := counter.willOverflow

      when(ctrl.enable) {
        when(ctrl.capture) {
          counter.clear()
        }
        when(streamOut.valid && ctrl.shift) {
          counter.increment()
        }
      }
      ctrl.tdo := B(data)(counter)
    }

    {
      val regIn = Reg(Bits((streamIn.getBitsWidth + 1) bits)) init(0)
      when(ctrl.enable) {
        when(ctrl.capture) {
          regIn := 0
        }
        when(ctrl.shift) {
          regIn := (regIn ## ctrl.tdi).resized
        }
      }

      streamIn.setIdle()
      when(regIn.msb) {
        streamIn.valid := True
        streamIn.payload.assignFromBits(regIn.resized)
        regIn := 0
      }

    }

    ctrl
  }
}
