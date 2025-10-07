package spinalextras.lib.misc

import spinal.core._
import spinal.lib._
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties, FormalProperty}

class StreamFragmentWidthAdapterWithOccupancy[T <: Data, T2 <: Data](inputDataType: HardType[T], outputDataType: HardType[T2], endianness: Endianness = LITTLE, padding: Boolean = false, earlyLast : Boolean = false) extends ComponentWithFormalProperties {
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
    val input = slave(Stream(Fragment(inputDataType)))
    val output = master(Stream(Fragment(outputDataType)))

    val processed = out(UInt(log2Up(max_processed) bits))
    val occupancy = out(UInt(log2Up(max_occupancy) bits))
  }
  val input = io.input
  val output = io.output

  val dataMask = Bits((outputWidth+inputWidth-1)/inputWidth bits)
  dataMask.setAll()
  dataMask.allowOverride()

  if (inputWidth == outputWidth) {
    output.arbitrationFrom(input)
    output.payload.assignFromBits(input.payload.asBits)
    io.occupancy := 0
    io.processed := 0
  } else if (inputWidth > outputWidth) new Composite(this, "adapt_down") {
    require(inputWidth % outputWidth == 0 || padding)
    val factor = (inputWidth + outputWidth - 1) / outputWidth
    val paddedInputWidth = factor * outputWidth
    val counter = Counter(factor,inc = output.fire)
    output.valid := input.valid
    endianness match {
      case `LITTLE` => output.fragment.assignFromBits(input.fragment.asBits.resize(paddedInputWidth).subdivideIn(factor slices).read(counter))
      case `BIG`    => output.fragment.assignFromBits(input.fragment.asBits.resize(paddedInputWidth).subdivideIn(factor slices).reverse.read(counter))
    }
    io.processed := counter.value
    io.occupancy := 0

    output.last := input.last && counter.willOverflowIfInc
    input.ready := output.ready && counter.willOverflowIfInc
    dataMask.setAll()
  } else new Composite(this, "adapt_up") {
    require(outputWidth % inputWidth == 0 || padding)
    val factor  = (outputWidth + inputWidth - 1) / inputWidth
    val paddedOutputWidth = factor * inputWidth
    val counter = Counter(factor,inc = input.fire)
    val buffer  = Reg(Bits(paddedOutputWidth - inputWidth bits))
    val sendIt = CombInit(counter.willOverflowIfInc)
    output.valid := input.valid && sendIt
    output.last := input.last
    input.ready := output.ready || !sendIt
    io.occupancy := counter.value
    io.processed := 0

    if(earlyLast){
      sendIt setWhen(input.last)
      when(input.valid && input.last && output.ready) {
        counter.clear()
      }
    }

    val data = CombInit(input.fragment ## buffer)
    endianness match {
      case `LITTLE` => output.fragment.assignFromBits(data.resize(outputWidth))
      case `BIG`    => output.fragment.assignFromBits(data.subdivideIn(factor slices).reverse.asBits().resize(outputWidth))
    }

    earlyLast match {
      case false => {
        dataMask.setAll()
        when(input.fire) {
          buffer := input.fragment ## (buffer >> inputWidth)
        }
      }
      case true  => {
        endianness match {
          case `LITTLE` => for((bit, id) <- dataMask.asBools.zipWithIndex) bit := counter >= id
          case `BIG`    => for((bit, id) <- dataMask.asBools.reverse.zipWithIndex) bit := counter >= id
        }
        for((bit, id) <- dataMask.asBools.zipWithIndex) bit := counter >= id

        when(input.fire) {
          whenIndexed(buffer.subdivideIn(inputWidth bits), counter, relaxedWidth = true) {
            _ := input.fragment.asBits
          }
        }
        whenIndexed(data.subdivideIn(inputWidth bits).dropRight(1), counter, relaxedWidth = true) {
          _ := input.fragment.asBits
        }
      }
    }
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

class StreamWidthAdapterWithOccupancy[T <: Data, T2 <: Data](inputDataType: HardType[T], outputDataType: HardType[T2], endianness: Endianness = LITTLE, padding: Boolean = false) extends ComponentWithFormalProperties {
  val dut = new StreamFragmentWidthAdapterWithOccupancy(inputDataType, outputDataType, endianness, padding, false)

  val io = new Bundle {
    val input = slave(Stream(inputDataType))
    val output = master(Stream(outputDataType))

    val processed = out(cloneOf(dut.io.processed))
    val occupancy = out(cloneOf(dut.io.occupancy))
  }

  io.input.addFragmentLast(False) <> dut.io.input
  dut.io.output.map(_.fragment) <> io.output

  io.processed := dut.io.processed
  io.occupancy := dut.io.occupancy
}

object StreamWidthAdapterWithOccupancy {
  def apply[T <: Data, T2 <: Data](input: Stream[T], output: Stream[T2], endianness: Endianness = LITTLE, padding: Boolean = false): StreamWidthAdapterWithOccupancy[T, T2] = {
    val dut = new StreamWidthAdapterWithOccupancy(input.payload, output.payload, endianness, padding = padding)
    dut.io.input <> input
    dut.io.output <> output
    dut
  }
}

object StreamFragmentWidthAdapterWithOccupancy {
  def apply[T <: Data, T2 <: Data](input: Stream[Fragment[T]], output: Stream[Fragment[T2]], endianness: Endianness = LITTLE, padding: Boolean = false) = {
    val dut = new StreamFragmentWidthAdapterWithOccupancy[T, T2](input.fragment, output.fragment, endianness, padding = padding)
    dut.io.input <> input
    dut.io.output <> output
    dut
  }
}