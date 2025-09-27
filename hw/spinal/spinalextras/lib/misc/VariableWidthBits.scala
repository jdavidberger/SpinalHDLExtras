package spinalextras.lib.misc

import spinal.core.{Bundle, Data, HardType, IntToBuilder, U, UInt, assert, log2Up}
import spinal.idslplugin.Location
import spinalextras.lib.formal.{FormalData, FormalProperties, FormalProperty}

import scala.language.postfixOps

class VariableWidthData[T <: Data](dataType : HardType[T],
                                   val variableGranularityBits : Int,
                                   val allowZeroSize : Boolean = false
                                       ) extends Bundle with FormalData {
  val width = dataType.getBitsWidth
  assert((width % variableGranularityBits) == 0)

  val payload = dataType()
  val sizeCode = UInt(log2Up((width / variableGranularityBits) + (if(allowZeroSize) 1 else 0)) bits)

  def size = {
    if (allowZeroSize) sizeCode else (sizeCode + 1)
  }
  def sizeInBits = size * variableGranularityBits

  def assign(from: VariableWidthData[T]): Unit = {
    assert(variableGranularityBits == from.variableGranularityBits)
    assert(allowZeroSize == from.allowZeroSize)

    payload := from.payload.resized
    size := from.size.resized
  }

  override def formalIsStateValid(): Seq[FormalProperty] = new FormalProperties(this) {
    addFormalProperty(sizeInBits.resized <= width, "Size shouldn't exceed available bits")
    //addFormalProperty((payload >> size).orR === False, "Unused bits should be zero")
  }
}

class VariableWidthBytes[T <: Data](dataType : HardType[T], allowZeroSize : Boolean = false) extends VariableWidthData[T](dataType, 8, allowZeroSize) {

}

case class VariableWidthBits(width: Int) extends Bundle with FormalData {
  val payload = UInt(width bits)
  val size = UInt(log2Up(width + 1) bits)

  def := (that: VariableWidthBits)(implicit loc: Location): Unit = assign(that)

  def assign(from: VariableWidthBits): Unit = {
    payload := from.payload.resized
    size := from.size.resized
  }

  def reversed = {
    val rtn = VariableWidthBits(width)
    rtn.payload := (payload.reversed >> (width - size)).resized
    rtn.size := size
    rtn
  }

  def >>(shiftBy: Int): VariableWidthBits = {
    val rtn = VariableWidthBits(width - shiftBy)
    rtn.payload := payload >> shiftBy
    rtn.size := size - shiftBy
    assert(size >= shiftBy)
    rtn
  }

  def clear(): Unit = {
    payload.clearAll()
    size.clearAll()
  }

  def takeHigh(n : Int) : Optional[UInt] = {
    val rtn = Optional(UInt(n bits))
    rtn.valid := size >= n
    rtn.value := (payload >> (size - n)).resize(n)
    rtn
  }

  def dropHigh(n: Int) = {
    val rtn = VariableWidthBits(width - n)
    //rtn.payload := payload.resized
    rtn.payload := (payload & (~(U((1 << n) - 1) << (size - n))).resized).resized
    rtn.size := (size - n).resized
    assert(size >= n)
    rtn
  }

  def ##(right: VariableWidthBits): VariableWidthBits = {
    val shifted = (payload << right.size)
    val rightMask = (U(1) << right.size) - 1
    val concat = shifted | (right.payload.resized & rightMask).resized

    VariableWidthBits(concat.resize(payload.getBitsWidth + right.payload.getBitsWidth bits), size +^ right.size)
  }

  override type Self = this.type

  /**
   * @return Whether or not the current state of the bundle is valid. Typically either asserted or assumed by a
   *         component which has this bundle as an input or an output.
   *
   *         For complicated properties, consider using the helper class `FormalProperties`
   */
  override def formalIsStateValid(): Seq[FormalProperty] = new FormalProperties(this) {
    addFormalProperty(size <= width, "Size shouldn't exceed available bits")
    //addFormalProperty((payload >> size).orR === False, "Unused bits should be zero")
  }
}

object VariableWidthBits {
  def apply(payload: UInt, size: UInt): VariableWidthBits = {
    //assert(size.getBitsWidth == log2Up(payload.getBitsWidth + 1))
    val bundle = VariableWidthBits(payload.getWidth)
    bundle.payload := payload
    //assert(bundle.size.getBitsWidth >= size.getBitsWidth)
    bundle.size := size.resized
    bundle
  }

  def Empty = VariableWidthBits(0, 0)

  def Empty(size: Int) = VariableWidthBits(U(0, size bits), 0)
}