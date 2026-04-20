package spinalextras.lib.misc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

object Slip {
  val END     = 0xC0
  val ESC     = 0xDB
  val ESC_END = 0xDC
  val ESC_ESC = 0xDD
}

class SlipEncoder extends Component {
  val io = new Bundle {
    val input  = slave Stream(Fragment(Bits(8 bits)))
    val output = master Stream(Bits(8 bits))
  }

  import Slip._

  // Defaults
  io.input.ready  := False
  io.output.valid := False
  io.output.payload := 0

  val fsm = new StateMachine {

    val idle: State = new State with EntryPoint {
      whenIsActive {
        io.output.valid := io.input.valid
        io.output.payload := io.input.fragment

        when(io.input.valid) {
          when(io.input.fragment === END || io.input.fragment === ESC) {
            io.output.payload := ESC
            when(io.output.fire) {
              goto(sendEsc_second)
            }
          } otherwise {
            io.input.ready := io.output.ready
            when(io.output.fire) {
              when(io.input.last) {
                goto(sendEnd)
              }
            }
          }
        }

      }
    }

    // --- ESC -> ESC_END / ESC_ESC ---
    val sendEsc_second: State = new State {
      whenIsActive {
        io.output.valid := True

        switch(io.input.fragment) {
          is(END) {
            io.output.payload := ESC_END
          }
          is (ESC) {
            io.output.payload := ESC_ESC
          }
          default {
            assert(False)
          }
        }

        when(io.output.fire) {
          when(io.input.last) {
            goto(sendEnd)
          } otherwise {
            io.input.ready := True
            goto(idle)
          }
        }
      }
    }

    // --- Frame END ---
    val sendEnd: State = new State {
      whenIsActive {
        io.output.valid := True
        io.output.payload := END

        when(io.output.fire) {
          io.input.ready := True
          goto(idle)
        }
      }
    }
  }
}

object SlipEncoder {
  def apply(stream : Stream[Bits]) : Stream[Bits] = {
    require(stream.payload.getBitsWidth % 8 == 0)
    if(stream.getBitsWidth == 8) {
      stream
    } else {
      val encoder = new SlipEncoder()
      val extender = StreamTransactionExtender(stream, encoder.io.input, (stream.getBitsWidth / 8) - 1) {
        (id, payload, last) => {
          val f = Fragment(Bits(8 bits))
          f.last := last
          f.fragment := (payload >> (8 * id)).resized
          f
        }
      }
      encoder.io.output
    }
  }
}