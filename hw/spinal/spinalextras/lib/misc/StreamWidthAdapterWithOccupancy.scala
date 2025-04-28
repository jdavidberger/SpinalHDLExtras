package spinalextras.lib.misc

import spinal.core._
import spinal.lib._
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties, FormalProperty}

class StreamWidthAdapterWithOccupancy[T <: Data, T2 <: Data](inputDataType: HardType[T], outputDataType: HardType[T2], endianness: Endianness = LITTLE, padding: Boolean = false) extends ComponentWithFormalProperties {
  val inputWidth = inputDataType.getBitsWidth
  val outputWidth = outputDataType.getBitsWidth

  val factor = {
    if (inputWidth == outputWidth) {
      0
    } else if (inputWidth > outputWidth) {
      (inputWidth + outputWidth - 1) / outputWidth
    } else {
      (outputWidth + inputWidth - 1) / inputWidth
    }
  }

  val (max_occupancy, max_processed) = {
    if(inputWidth > outputWidth)
      (0, factor)
    else
      (factor, 0)
  }

  val io = new Bundle {
    val input = slave(Stream(inputDataType))
    val output = master(Stream(outputDataType))

    val processed = out(UInt(log2Up(max_processed) bits))
    val occupancy = out(UInt(log2Up(max_occupancy) bits))
  }
  val input = io.input
  val output = io.output

  if (inputWidth == outputWidth) {
    output.arbitrationFrom(input)
    output.payload.assignFromBits(input.payload.asBits)
    io.occupancy := 0
    io.processed := 0
  } else if (inputWidth > outputWidth) new Composite(this, "adapt_down") {
    require(inputWidth % outputWidth == 0 || padding)
    val factor = (inputWidth + outputWidth - 1) / outputWidth
    val paddedInputWidth = factor * outputWidth
    val counter = Counter(factor, inc = output.fire)
    io.processed := counter.value
    io.occupancy := 0

    output.valid := input.valid
    endianness match {
      case `LITTLE` => output.payload.assignFromBits(input.payload.asBits.resize(paddedInputWidth).subdivideIn(factor slices).read(counter))
      case `BIG` => output.payload.assignFromBits(input.payload.asBits.resize(paddedInputWidth).subdivideIn(factor slices).reverse.read(counter))
    }
    input.ready := output.ready && counter.willOverflowIfInc
  } else new Composite(this, "adapt_up") {
    require(outputWidth % inputWidth == 0 || padding)
    val factor = (outputWidth + inputWidth - 1) / inputWidth
    val paddedOutputWidth = factor * inputWidth
    val counter = Counter(factor, inc = input.fire)
    io.occupancy := counter.value
    io.processed := 0

    val buffer = Reg(Bits(paddedOutputWidth - inputWidth bits))
    when(input.fire) {
      buffer := input.payload ## (buffer >> inputWidth)
    }
    output.valid := input.valid && counter.willOverflowIfInc
    endianness match {
      case `LITTLE` => output.payload.assignFromBits((input.payload ## buffer).resize(outputWidth))
      case `BIG` => output.payload.assignFromBits((input.payload ## buffer).subdivideIn(factor slices).reverse.asBits().resize(outputWidth))
    }
    input.ready := !(!output.ready && counter.willOverflowIfInc)
  }

  override def covers(): Seq[FormalProperty] = new FormalProperties(this) {
    addFormalProperty(io.input.fire)
    addFormalProperty(io.output.fire)
    if(max_occupancy > 0) {
      addFormalProperty(io.occupancy === max_occupancy - 1)
    }
    if(max_processed > 0) {
      addFormalProperty(io.processed === max_processed - 1)
    }
  }
}

object StreamWidthAdapterWithOccupancy {
  def apply[T <: Data, T2 <: Data](input: Stream[T], output: Stream[T2], endianness: Endianness = LITTLE, padding: Boolean = false): StreamWidthAdapterWithOccupancy[T, T2] = {
    val dut = new StreamWidthAdapterWithOccupancy(input.payload, output.payload, endianness, padding = padding)
    dut.io.input <> input
    dut.io.output <> output
    dut
  }
}