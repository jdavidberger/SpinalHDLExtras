package spinalextras.lib.misc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps


// This represents a data stream that is not necessarily aligned to the dataType's native bitwidth. The mechanism at play
// here is thus:
// - All non-last beats are full size
// - The last beat in a stream has it's last byte set to the number of valid bytes in that beat. This number can be zero.
class PaddedFragment[T <: Data](dataType : HardType[T]) extends Fragment[T](dataType) {
  def bytesPerBeat = dataType.getBitsWidth / 8
  def validBytesType = UInt(log2Up(bytesPerBeat) bits)
  def validBytesOffset : Int = dataType.getBitsWidth - 8

  def validBytes(): UInt = {
    Mux(this.last, this.fragment.asBits(validBytesOffset, 8 bits).asUInt, U(bytesPerBeat, 8 bits))
  }
  def setValidBytes(validByte : UInt) {
    this.fragment.assignFromBits(validByte.asBits.resized, validBytesOffset, 8 bits)
  }
}

object PaddedFragment {
  def apply[T <: Data](dataType: HardType[T]): PaddedFragment[T] = {
    new PaddedFragment(dataType)
  }

  def apply[T <: Data](f: Fragment[T]): PaddedFragment[T] = {
    val rtn = new PaddedFragment(f.fragment)
    rtn.last := f.last
    rtn.fragment := f.fragment
    rtn
  }

  def encodePaddingToStream[T <: Data](streamIn : Stream[Fragment[VariableWidthBytes[T]]], fragmentationError : Bool): Stream[PaddedFragment[T]] = {
    // Note: the assumption is that every beat of streamIn is full width and translated as thus. When this isn't true,
    // set fragmentationError to true but otherwise proceed as if it were true.
    val dataType = HardType(streamIn.payload.fragment.payload)
    val bytesPerBeat = dataType.getBitsWidth / 8

    val inFragment = streamIn.payload.fragment
    val inLast = streamIn.payload.last

    fragmentationError := streamIn.valid && !inLast && (inFragment.size =/= bytesPerBeat)

    // A last beat that is entirely full has no spare byte left to smuggle the valid-byte-count
    // into, so it's forwarded as an ordinary (non-last) full beat, followed by a synthetic
    // zero-length last beat that carries the terminating count instead.
    val isFullLastBeat = inLast && (inFragment.size === bytesPerBeat)
    val sendingTrailer = RegInit(False)

    val streamOut = Stream(PaddedFragment(dataType))

    streamOut.fragment := inFragment.payload
    streamOut.last := sendingTrailer || (inLast && !isFullLastBeat)
    streamOut.valid := sendingTrailer || streamIn.valid
    streamIn.ready := !sendingTrailer && streamOut.ready

    when(sendingTrailer) {
      streamOut.setValidBytes(U(0))
    } elsewhen(inLast && !isFullLastBeat) {
      streamOut.setValidBytes(inFragment.size.resized)
    }

    when(streamOut.fire) {
      sendingTrailer := !sendingTrailer && isFullLastBeat
    }

    streamOut
  }

  // allowZeroSize controls whether the decoded VariableWidthBytes can represent a zero-size last
  // beat (the synthetic trailer emitted by encodePaddingToStream for a fully-utilized last beat).
  // Defaults to false: callers that know their source never produces a fully-utilized last beat
  // get the simpler allowZeroSize=false representation, and get an assertion instead of silently
  // mis-decoding if that assumption doesn't hold.
  def decodePaddingFromStream[T <: Data](streamIn : Stream[PaddedFragment[T]], allowZeroSize : Boolean = false): Stream[Fragment[VariableWidthBytes[T]]] = {
    val dataType = HardType(streamIn.payload.fragment)

    streamIn.translateWith {
      val result = Fragment(new VariableWidthBytes(dataType, allowZeroSize))
      result.last := streamIn.payload.last

      val size = streamIn.payload.validBytes()
      result.fragment.payload := streamIn.payload.fragment
      if (allowZeroSize) {
        result.fragment.sizeCode := size.resized
      } else {
        when(streamIn.valid) {
          assert(size =/= 0,
            "PaddedFragment.decodePaddingFromStream: encountered a zero-size beat with allowZeroSize=false")
        }
        result.fragment.sizeCode := (size - 1).resized
      }
      result
    }
  }

}
