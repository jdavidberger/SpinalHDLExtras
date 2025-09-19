package spinalextras.lib.misc

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinalextras.lib.formal.ComponentWithFormalProperties

import scala.language.postfixOps

class PartialData(dataWidth : Int = 32) extends Bundle {
  // Data is arranged to the LSB -- so something with three bytes is 0x__AABBCC not 0XAABBCC__.
  val data = Bits(dataWidth bits)

  // Number of valid bytes - 1
  val valid_bytes = UInt(log2Up(dataWidth / 8) bits)
}

case class StreamJaggedData(dataWidth : Int = 32) extends ComponentWithFormalProperties {
  val io = new Bundle {
    val input = slave Stream (Fragment(new PartialData(dataWidth)))
    val output = master Stream (Fragment(Bits(dataWidth bits)))
  }

  io.input.ready := False
  io.output.valid := False
  io.output.last := False
  io.output.payload.fragment.assignDontCareToUnasigned()

  val valid_bytes = Reg(UInt(2 bits)) init (0)

  val masks = Vec(Seq(0, 0xff0000L, 0xffff00L, 0xffffffL).map(x => B(x, 24 bits)))
  val shifts = Vec(Seq(0, 1, 2, 3).map(x => U(x, 2 bits)))

  val holdover = Reg(Bits(24 bits)) init (0)

  val isLast = RegInit(False)

  val aligned_input = io.input.data << (shifts(3 - io.input.valid_bytes) * 8)
  //val aligned_input = io.input.data
  val shifted_input = (aligned_input ## B(0, 24 bits)) >> (shifts(valid_bytes) * 8)
  val double_word = ((holdover & masks(valid_bytes)) << 32).resized | shifted_input

  io.output.payload.fragment := double_word(24, 32 bits)
  val next_holdover = double_word(0, 24 bits)
  val input_valid_bytes = io.input.valid_bytes +^ 1
  val next_valid_bytes = valid_bytes +^ input_valid_bytes

  when(isLast) {
    io.output.payload := holdover.resized
    io.output.last := True
    io.output.valid := True
    when(io.output.fire) {
      isLast := False
      valid_bytes := 0
      holdover := 0
    }
  } elsewhen (io.output.ready) {
    io.input.ready := True
    when(io.input.valid) {

      when(next_valid_bytes >= 4) {
        io.output.valid := True

        when(io.output.fire) {
          when(next_valid_bytes === 4) {
            io.output.last := io.input.last
          } otherwise {
            isLast := io.input.last
          }

          valid_bytes := (next_valid_bytes - 4).resized
          holdover := next_holdover
        }
      } otherwise {
        holdover := double_word(32, 24 bits)
        valid_bytes := next_valid_bytes.resized
      }
    }
  }
}