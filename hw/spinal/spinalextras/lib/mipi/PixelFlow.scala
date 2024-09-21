package spinalextras.lib.mipi

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class PixelFlow[T <: Data](val dataType: HardType[T]) extends Flow[T](dataType) {
  val frame_valid = Bool()
  def line_valid = valid

  override def clone = new PixelFlow(dataType).asInstanceOf[this.type]

  override def asMaster(): Unit = {
    super.asMaster()
    out(frame_valid)
  }

  override def map[T2 <: Data](translate: T => T2) = translateWith(translate(payload))

  override def translateWith[T2 <: Data](that: T2): PixelFlow[T2] = {
    val next = new PixelFlow(that)
    next.valid := this.valid
    next.frame_valid := this.frame_valid
    next.payload := that
    next
  }

}

object PixelFlow {
  def apply(pixels : Int) : PixelFlow[Bits] = {
    new PixelFlow[Bits](Bits(pixels bits))
  }
  def apply[T <: Data](dataType: HardType[T]) : PixelFlow[T] = {
    new PixelFlow[T](dataType)
  }
}
