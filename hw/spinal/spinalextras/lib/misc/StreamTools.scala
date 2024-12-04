package spinalextras.lib.misc

import spinal.core._
import spinal.lib._
import spinalextras.lib.testing.test_funcs

import scala.language.postfixOps

class AdaptWidth[T <: Data](dataTypeIn : HardType[T], dataTypeOut : HardType[T]) extends Component {
  val io = new Bundle {
    val in = slave(Stream(dataTypeIn))
    val out = master(Stream(dataTypeOut))
  }

  val in = io.in
  val out = io.out

  val streamMid = Stream(Bits(StreamTools.lcm(in.payload.getBitsWidth, out.payload.getBitsWidth) bits))
  def latency = streamMid.getBitsWidth / dataTypeIn.getBitsWidth

  test_funcs.assertStreamContract(in)
  test_funcs.assertStreamContract(out)

  StreamWidthAdapter(in, streamMid, endianness = LITTLE)
  StreamWidthAdapter(streamMid.stage(), out, endianness = LITTLE)
}

class AdaptFragmentWidth[T <: Data](dataTypeIn : HardType[T], dataTypeOut : HardType[T]) extends Component {
  val io = new Bundle {
    val in = slave(Stream(Fragment(dataTypeIn)))
    val out = master(Stream(Fragment(dataTypeOut)))
  }

  val in = io.in
  val out = io.out

  val streamMid = Stream(Fragment(Bits(StreamTools.lcm(in.fragment.getBitsWidth, out.fragment.getBitsWidth) bits)))
  def latency = streamMid.fragment.getBitsWidth / dataTypeIn.getBitsWidth

  test_funcs.assertStreamContract(in)
  test_funcs.assertStreamContract(out)

  StreamFragmentWidthAdapter(in, streamMid, endianness = LITTLE, earlyLast = true)
  StreamFragmentWidthAdapter(streamMid.stage(), out, endianness = LITTLE, earlyLast = true)
}

object StreamTools {
  def lcm(numberOne: Int, numberTwo: Int) = {
    val bigger = Math.max(numberOne, numberTwo)
    val smaller = Math.min(numberOne, numberTwo)

    println(bigger, smaller)
    (1 until smaller).filter((factor) => (factor * bigger) % smaller == 0).map((factor) => Math.abs(factor * bigger)).toVector(0)
  }

  def AdaptWidth[T <: Data](in: Stream[T], datatypeOut: HardType[T]): Stream[T] = {
    val stream = Stream(datatypeOut)
    AdaptWidth(in, stream)
    stream
  }

  def AdaptFragmentWidth[T <: Data](in: Stream[Fragment[T]], out: Stream[Fragment[T]]): Int = {
    if(in.getBitsWidth == out.getBitsWidth) {
      in >> out
      1
    } else {
      val adapter = new AdaptFragmentWidth(in.fragment, out.fragment)
      adapter.io.in << in
      adapter.io.out >> out
      adapter.latency
    }
  }

  def AdaptWidth[T <: Data](in: Stream[T], out: Stream[T]): Int = {
    if(in.getBitsWidth == out.getBitsWidth) {
      in >> out
      1
    } else {
      val adapter = new AdaptWidth(in.payloadType, out.payloadType)
      adapter.io.in << in
      adapter.io.out >> out
      adapter.latency
    }
  }

  def AdaptWidth[T <: Data](in: Flow[T], out: Flow[T]): Int = {
    val streamOut = Stream(Bits(out.payload.getBitsWidth bits))

    val overflow = Bool()
    assert(!overflow, s"${in} ${out} ${in.payload.getBitsWidth} ${out.payload.getBitsWidth} Adaption width overflowed")
    streamOut.toFlow.map(_.as(cloneOf(out.payload))) >> out

    AdaptWidth(in.toStream(overflow).map(_.asBits), streamOut)
  }
}

