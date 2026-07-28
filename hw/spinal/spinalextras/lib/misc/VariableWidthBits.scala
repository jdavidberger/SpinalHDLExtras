package spinalextras.lib.misc

import spinal.core.{Bool, Bundle, Data, False, HardType, IntToBuilder, Reg, RegInit, True, U, UInt, assert, log2Up, when}
import spinal.idslplugin.Location
import spinal.lib.{Fragment, Stream}
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
    // sizeCode is only wide enough to hold its own values (0 until 2^n); a plain `+ 1` would
    // silently wrap back to 0 right when sizeCode is at its max (e.g. whenever width/variableGranularityBits
    // is a power of two), so grow by a bit before adding to actually reach the full-size value.
    if (allowZeroSize) sizeCode else (sizeCode +^ 1)
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
  def setValidSizeInBytes(validBytes : UInt): Unit = {
    this.sizeCode := validBytes - 1
  }
  def validByteCount : UInt = this.sizeCode +^ 1
}

object VariableWidthBytes {
  // Converts a Stream[Fragment[VariableWidthBytes[T]]] (which may have allowZeroSize=true) into
  // one with allowZeroSize=false, by dropping a zero-size last beat and instead marking the
  // immediately preceding beat (within the same packet) as last -- that beat becomes a full-size
  // last beat instead of being followed by a separate zero-length one. A zero-size last beat with
  // no preceding beat in the same packet (a fully empty packet) has no allowZeroSize=false
  // representation and triggers an assertion.
  def withoutZeroSize[T <: Data](streamIn: Stream[Fragment[VariableWidthBytes[T]]]): Stream[Fragment[VariableWidthBytes[T]]] = {
    val dataType = HardType(streamIn.payload.fragment.payload)
    val bytesPerBeat = dataType.getBitsWidth / 8

    val inLast = streamIn.payload.last
    val inSize = streamIn.payload.fragment.size
    val isFullNonLast = !inLast
    val isZeroLast = inLast && (inSize === 0)

    // Holds the most recent full non-last beat until we can see whether it's actually the packet's
    // last (fully-utilized) beat -- signaled by a following zero-size last beat -- or the packet
    // simply continues.
    val heldValid = RegInit(False)
    val heldPayload = Reg(dataType())

    val streamOut = Stream(Fragment(new VariableWidthBytes(dataType, allowZeroSize = false)))

    streamOut.valid := False
    streamOut.last := True
    streamOut.fragment.payload := streamIn.payload.fragment.payload
    streamOut.fragment.sizeCode := (inSize - 1).resized
    streamIn.ready := False

    when(heldValid) {
      streamOut.valid := streamIn.valid
      streamOut.fragment.payload := heldPayload
      streamOut.fragment.sizeCode := (bytesPerBeat - 1)

      when(isZeroLast) {
        // Merge: the held beat turns out to have been the final, fully-utilized beat.
        streamOut.last := True
        streamIn.ready := streamOut.ready
        when(streamOut.fire) {
          heldValid := False
        }
      } elsewhen(isFullNonLast) {
        // The held beat wasn't the packet's last one after all -- flush it (last=false) and shift
        // the newly-arrived beat into hold.
        streamOut.last := False
        streamIn.ready := streamOut.ready
        when(streamOut.fire) {
          heldPayload := streamIn.payload.fragment.payload
        }
      } elsewhen(streamIn.valid) {
        // The packet ends with an ordinary (nonzero-size) beat: flush the held beat first: the
        // new beat gets consumed on a later cycle, once nothing is held anymore.
        streamOut.last := False
        streamIn.ready := False
        when(streamOut.fire) {
          heldValid := False
        }
      } otherwise {
        // No new beat has arrived yet -- can't tell what the held beat's last bit should be.
        streamOut.valid := False
      }
    } otherwise {
      when(isFullNonLast) {
        // Capture silently; nothing to emit until we know how this beat's packet ends.
        streamIn.ready := True
        when(streamIn.valid) {
          heldValid := True
          heldPayload := streamIn.payload.fragment.payload
        }
      } otherwise {
        // An ordinary last beat (zero-size only if this is a fully empty packet, see the
        // assertion above) with nothing held -- pass it straight through.
        streamOut.valid := streamIn.valid
        streamIn.ready := streamOut.ready
      }
    }

    streamOut
  }
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
    rtn.has_value := size >= n
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