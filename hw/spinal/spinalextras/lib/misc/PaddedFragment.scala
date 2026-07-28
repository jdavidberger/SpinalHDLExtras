package spinalextras.lib.misc

import spinal.core._
import spinal.lib._
import spinalextras.lib.misc.StreamTools.CreateFragment

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

  def asFragment() : Fragment[T] = {
    CreateFragment(fragment, last)
  }

  override def clone: this.type = {
    new PaddedFragment(fragmentType).asInstanceOf[this.type]
  }
}

object PaddedFragment {
  implicit class PaddedFragmentStream[T <: Data](stream : Stream[PaddedFragment[T]]) {
    def decode(allowZeroSize : Boolean = false): Stream[Fragment[VariableWidthBytes[T]]] = {
      PaddedFragment.decodePaddingFromStream(stream, allowZeroSize)
    }

    def asFragmentStream() : Stream[Fragment[T]] = {
      stream.map(_.asFragment())
    }
    def insertHeader(header: Vec[T]) = {
      asFragmentStream().insertHeader(header).asPaddedFragmentStream()
    }
    def insertHeader(header: T) = {
      asFragmentStream().insertHeader(header).asPaddedFragmentStream()
    }
    def dropPaddingInformation() : Stream[Fragment[T]] = {
      PaddedFragment.decodePaddingFromStream(stream).fragmentMap(_.payload)
    }
    def lastFire = stream.fire && stream.last
  }

  implicit class FragmentStream[T <: Data](stream : Stream[Fragment[T]]) {
    def fragmentMap[T2 <: Data](fn : T => T2) = {
      stream.map(f => CreateFragment(fn(f.fragment), f.last))
    }

    def asPaddedFragmentStream() : Stream[PaddedFragment[T]] = {
      stream.map(x => {
        val f = new PaddedFragment(x.fragment)
        f.fragment := x.fragment
        f.last := x.last
        f
      })
    }
    def toPaddedFragmentStream() : Stream[PaddedFragment[T]] = {
      StreamTools.insertFooter[T](stream.replaceFragmentLast(False),
        Vec(StreamTools.CreateFragment(B(0, stream.fragment.getBitsWidth bits).as[T](stream.fragment), True))
      ).map(x => {
        val f = new PaddedFragment(stream.payload.dataType)
        f.fragment := x.fragment
        f.last := x.last
        f
      })
    }
  }

  def apply[T <: Data](dataType: HardType[T]): PaddedFragment[T] = {
    new PaddedFragment(dataType)
  }

  def apply[T <: Data](f: Fragment[T]): PaddedFragment[T] = {
    val rtn = new PaddedFragment(f.fragment)
    rtn.last := f.last
    rtn.fragment := f.fragment
    rtn
  }

  def encodePaddingToStream[T <: Data](streamIn : Stream[Fragment[VariableWidthBytes[T]]], fragmentationError : Bool = null): Stream[PaddedFragment[T]] = {
    // Note: the assumption is that every beat of streamIn is full width and translated as thus. When this isn't true,
    // set fragmentationError to true but otherwise proceed as if it were true.
    val dataType = HardType(streamIn.payload.fragment.payload)
    val bytesPerBeat = dataType.getBitsWidth / 8

    val inFragment = streamIn.payload.fragment
    val inLast = streamIn.payload.last

    if(fragmentationError != null)
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
      //streamOut.setValidBytes(U(0))
      streamOut.fragment.clearAll()
      streamOut.last := True
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

    val raw = streamIn.translateWith {
      val result = Fragment(new VariableWidthBytes(dataType, allowZeroSize = true))
      result.last := streamIn.payload.last
      result.fragment.payload := streamIn.payload.fragment
      result.fragment.sizeCode := streamIn.payload.validBytes().resized
      result
    }

    if (allowZeroSize) raw else VariableWidthBytes.withoutZeroSize(raw)
  }

}
