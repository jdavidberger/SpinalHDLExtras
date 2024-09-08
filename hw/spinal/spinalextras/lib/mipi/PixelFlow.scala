package spinalextras.lib.mipi

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class PixelFlow(PIXEL_WIDTH: Int) extends Flow[Bits](Bits(PIXEL_WIDTH bits)) {
  val frame_valid = Bool()
  def line_valid = valid

  override def clone: PixelFlow = PixelFlow(PIXEL_WIDTH).asInstanceOf[this.type]

  override def asMaster(): Unit = {
    super.asMaster()
    out(frame_valid)
  }


}
