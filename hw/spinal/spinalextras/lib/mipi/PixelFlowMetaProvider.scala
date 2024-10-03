package spinalextras.lib.mipi

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class PixelFlowMeta() extends Bundle {
  val cols = UInt(16 bits)
  val rows = UInt(16 bits)
  val words = Reg(UInt(24 bits)) init(0)
  val frame_idx = UInt(16 bits)
}

case class PixelFlowMetaProvider(WIDTH : Int) extends Component {
  val io = new Bundle {
    val pixelFragment = slave (Flow(Fragment(Bits(WIDTH bits))))

    val meta = master (Flow(PixelFlowMeta()))
  }

  val rows = Reg(UInt(16 bits)) init(0)
  val cols = Reg(UInt(16 bits)) init(0)
  val words = Reg(UInt(24 bits)) init(1)
  val frame_idx = Reg(UInt(16 bits)) init(0)

  io.meta.rows := rows
  io.meta.cols := cols
  io.meta.frame_idx := frame_idx
  io.meta.words := words

  when(io.pixelFragment.valid.rise()) {
    rows := rows + 1
  }

  when(io.pixelFragment.valid.rise()) {
    cols := 1
  } elsewhen(io.pixelFragment.valid) {
    cols := cols + 1
  }
  when(io.pixelFragment.fire) {
    words := words + 1
  }

  io.meta.valid := False
  when(io.pixelFragment.lastFire) {
    frame_idx := frame_idx + 1
    io.meta.valid := True
    cols := 0
    rows := 0
    words := 1
  }
}