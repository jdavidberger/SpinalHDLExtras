package spinalextras.lib.misc

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._
import spinal.lib.fsm.{EntryPoint, State, StateMachine}
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties, FormalProperty}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut, test_funcs}

import scala.collection.mutable
import scala.language.postfixOps


class AdaptWidthByState[T <: Data](dataTypeIn : HardType[T], dataTypeOut : HardType[T]) extends Component {
  val io = new Bundle {
    val in = slave(Stream(dataTypeIn))
    val out = master(Stream(dataTypeOut))
  }

  require(dataTypeIn.getBitsWidth < dataTypeOut.getBitsWidth)

  var last_remainder = dataTypeIn.getBitsWidth
  val remainders = new mutable.ArrayBuffer[Int]()
  while(last_remainder != 0) {
    last_remainder = last_remainder + dataTypeIn.getBitsWidth - dataTypeOut.getBitsWidth
    remainders.append(last_remainder)
  }

  val last_value = RegNextWhen(io.in.payload, io.in.fire)

  val fsm = new StateMachine {
    val start = new State with EntryPoint

    val remainder_states = remainders.map(rem => new State().setName(s"remainder_state_${rem}"))
    for(i <- remainder_states.indices) {
      val remainder = remainders(i)
      remainder_states(i).whenIsActive {
        io.in.ready := io.out.ready
        io.out.payload.assignFromBits((io.in.payload ## last_value.asBits(0, remainder bits)).resized)
        io.out.valid := io.in.valid
        when(io.in.fire) {
          if(i == remainder_states.size - 1) {
            goto(start)
          } else {
            goto(remainder_states(i + 1))
          }
        }
      }
    }

    start.whenIsActive {
      io.in.ready := True
      when(io.in.fire) {
        goto(remainder_states(0))
      }
    }
  }


  // 21 - 16

  // 48
  // 48* (96-64) 32
  // 48* (80-64) 16
  // 48* (64-64)

  // 64
  // 64* (128-72) 56
  // 64* (120-72) 48
  // 64* (112-72) 40
  // 64* (104-72) 36

  // 64
  // 64 (128-84) 44
  // 64 (108-84) 24
  // 64 (88-84) 4
  // 64 (68-

  // 64 * n > 84 * (n-1)
  // 64 * n > 84 * n - 84
  // 0 > 20 * n - 84
  // 84 > 20 * n
  // 84 / 20 > n


}

abstract trait StreamMapComponent[T1 <: Data, T2 <: Data] extends ComponentWithKnownLatency {
  def InputStream() : Stream[T1]
  def OutputStream() : Stream[T2]

  def apply[T3 <: Data](prior : Stream[T3])(map_fn : (T3) => T1) : Stream[TupleBundle2[T2, T3]] = {
    val (componentInput, mergeInput) = StreamFork2(prior, synchronous = false)
    componentInput.map(map_fn) <> InputStream()
    StreamJoin(OutputStream(), mergeInput.queue(this.latency(), latency = 0))
  }

  def apply(prior : Stream[T1]) = {
    InputStream() <> prior
    OutputStream()
  }
}

class AdaptWidth[T <: Data](dataTypeIn : HardType[T], dataTypeOut : HardType[T], endianness: Endianness = LITTLE) extends ComponentWithFormalProperties {

  val io = new Bundle {
    val in = slave(Stream(dataTypeIn))
    val output = master(Stream(dataTypeOut))

    val occupancy = out(UInt(1 bit))
    val isEmpty = out(Bool())
  }

  val logic =
    if(dataTypeIn.getBitsWidth == dataTypeOut.getBitsWidth) {
      io.in >> io.output
      io.occupancy := 0
      io.isEmpty := True

      val latency = 1

      null
    } else new Composite(this, "logic_multi") {
      val in = io.in
      val output = io.output

      val streamMid = Stream(Bits(StreamTools.lcm(in.payload.getBitsWidth, output.payload.getBitsWidth) bits))

      val swaIn = StreamWidthAdapterWithOccupancy(in, streamMid, endianness = endianness)
      val streamMid_stage = streamMid.stage()
      val swaOut = StreamWidthAdapterWithOccupancy(streamMid_stage, output, endianness = endianness)

      val insPerMid = streamMid.payload.getBitsWidth / in.payload.getBitsWidth

      val counterIn = Counter(streamMid.payload.getBitsWidth / in.payload.getBitsWidth, inc = in.fire)
      val counterOut = Counter(streamMid.payload.getBitsWidth / output.payload.getBitsWidth, inc = output.fire)

      io.occupancy := streamMid_stage.valid.asUInt

      io.isEmpty := streamMid_stage.valid === False && counterIn.value === 0 && counterOut.value === 0

      val latency = streamMid.getBitsWidth / dataTypeIn.getBitsWidth
    }

  lazy val formalCounter = {
    val counter = new CounterUpDownUneven(dataTypeIn.getBitsWidth * dataTypeOut.getBitsWidth * 1 << 16, dataTypeIn.getBitsWidth, dataTypeOut.getBitsWidth)
    when(io.in.fire) {
      counter.increment()
    }
    when(io.output.fire) {
      counter.decrement()
    }
    counter
  }

  val latency = if(logic == null) 1 else logic.latency

  override def covers() = new FormalProperties(this) {
    addFormalProperty(io.in.fire)
    addFormalProperty(io.output.fire)
    addFormalProperty(io.isEmpty)

    if(logic != null) {
      addFormalProperty(io.occupancy(0))
    }
  }

  override def formalComponentProperties(): Seq[FormalProperty] = new FormalProperties(this) {
    val counter = formalCounter
    if(logic != null) new Composite(this, "logic") {
      val inBitsOcc = logic.swaIn.io.occupancy * dataTypeIn.getBitsWidth
      val midBitsOcc = logic.streamMid_stage.valid.asUInt * logic.streamMid.payload.getWidth
      val outBitsOcc = logic.swaOut.io.processed * dataTypeOut.getBitsWidth

      val calcOcc = inBitsOcc +^ midBitsOcc -^ outBitsOcc
      addFormalProperty(midBitsOcc >= outBitsOcc)
      addFormalProperty(calcOcc === counter.value, "Counter value should match calculated occ")
    } else {
      addFormalProperty(0 === counter.value)
    }
  }

}

class AdaptFragmentWidth[T <: Data](dataTypeIn : HardType[T], dataTypeOut : HardType[T]) extends ComponentWithFormalProperties {
  val io = new Bundle {
    val in = slave(Stream(Fragment(dataTypeIn)))
    val out = master(Stream(Fragment(dataTypeOut)))
  }

  val in = io.in
  val out = io.out

  val streamMid = Stream(Fragment(Bits(StreamTools.lcm(in.fragment.getBitsWidth, out.fragment.getBitsWidth) bits)))
  def latency = streamMid.fragment.getBitsWidth / dataTypeIn.getBitsWidth

  StreamFragmentWidthAdapter(in, streamMid, endianness = LITTLE, earlyLast = true)
  StreamFragmentWidthAdapter(streamMid.stage(), out, endianness = LITTLE, earlyLast = true)
}

class StreamGather[T <: Data](dataType : HardType[T], gatherCount : Int) extends ComponentWithFormalProperties {
  val io = new Bundle {
    val input = slave Stream(dataType)
    val output = master Stream(TupleBundle(Vec(new Optional(dataType), gatherCount), dataType()))
  }

  val registers = Array.fill(gatherCount)(RegInit(Optional.Empty(dataType)))
  for(i <- 0 until gatherCount) {
    when(io.input.fire) {
      if(i > 0) {
        registers(i - 1) := registers(i)
      }
    }
  }

  when(io.input.fire) {
    registers(gatherCount - 1).value := io.input.payload
    registers(gatherCount - 1).valid := True
  }

  io.output.payload._2 := registers(0).value

  for(i <- 0 until gatherCount - 1) {
    io.output.payload._1(i).value := registers(i + 1).value
    io.output.payload._1(i).valid := registers(i + 1).valid
  }
  io.output.payload._1(gatherCount - 1).value := io.input.payload
  io.output.payload._1(gatherCount - 1).valid := True

  io.output.valid := (io.input.valid && registers(0).valid)
  io.input.ready := (io.output.ready || !registers(0).valid)

  //io.output.payload.assignDontCareToUnasigned()
}

class StreamFragmentGather[T <: Data](dataType : HardType[T], gatherCount : Int) extends ComponentWithFormalProperties {
  val io = new Bundle {
    val input = slave Stream(Fragment(dataType))
    val output = master Stream(Fragment(TupleBundle(Vec(new Optional(dataType), gatherCount), dataType())))
  }

  val output = cloneOf(io.output)

  val flush = RegInit(False) setWhen(io.input.lastFire) clearWhen(output.lastFire)
  val registers = Array.fill(gatherCount)(RegInit(Optional.Empty(dataType)))
  for(i <- 0 until gatherCount) {
    when(io.input.fire || (flush && output.fire)) {
      if(i > 0) {
        registers(i - 1) := registers(i)
      }
    }
  }

  when(io.input.fire) {
    registers(gatherCount - 1).value := io.input.payload
    registers(gatherCount - 1).valid := True
  } elsewhen(flush && output.fire) {
    registers(gatherCount - 1).value.assignDontCare()
    registers(gatherCount - 1).valid := False
  }

  output.payload._2 := registers(0).value

  for(i <- 0 until gatherCount - 1) {
    output.payload._1(i).value := registers(i + 1).value
    output.payload._1(i).valid := registers(i + 1).valid
  }
  output.payload._1(gatherCount - 1).value := io.input.payload
  output.payload._1(gatherCount - 1).valid := !flush

  output.valid := (io.input.valid && registers(0).valid) || flush
  io.input.ready := (output.ready || !registers(0).valid)

  output.last := flush && !registers(1).valid

  io.output <> output.discardWhen(!registers(0).valid)
}

object StreamTools {
  def CreateFragment[T <: Data](v : T, last : Bool) = {
    val rtn = Fragment(cloneOf(v))
    rtn.fragment := v
    rtn.last := last
    rtn
  }

  def gather[T <: Data](stream : Stream[T], gatherCount : Int) = {
    val gatherer = new StreamGather(cloneOf(stream.payload), gatherCount)
    gatherer.io.input <> stream
    gatherer.io.output
  }

  def gatherFragment[ T <: Data](stream : Stream[Fragment[T]], gatherCount : Int) = {
    val gatherer = new StreamFragmentGather(cloneOf(stream.fragment), gatherCount)
    gatherer.io.input <> stream
    gatherer.io.output
  }

  def gcd(a: Int, b: Int): Int = {
    if (b == 0) 0
    else gcd(b, a % b);
  }
  def lcm(numberOne: Int, numberTwo: Int) = {
    val bigger = Math.max(numberOne, numberTwo)
    val smaller = Math.min(numberOne, numberTwo)

    println(bigger, smaller)
    (1 to smaller).filter((factor) => (factor * bigger) % smaller == 0).map((factor) => Math.abs(factor * bigger)).toVector(0)
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

  def AdaptWidth[T <: Data](in: Stream[T], out: Stream[T]) : AdaptWidth[T] = {
    val adapter = new AdaptWidth(in.payloadType, out.payloadType)
    adapter.io.in << in
    adapter.io.output >> out
    adapter
  }

  def AdaptWidth[T <: Data](in: Flow[T], out: Flow[T]) : AdaptWidth[Bits] = {
    val streamOut = Stream(Bits(out.payload.getBitsWidth bits))

    val overflow = Bool()
    assert(!overflow, s"${in} ${out} ${in.payload.getBitsWidth} ${out.payload.getBitsWidth} Adaption width overflowed")
    streamOut.toFlow.map(_.as(cloneOf(out.payload))) >> out

    AdaptWidth(in.toStream(overflow).map(_.asBits), streamOut)
  }

  def insertFooter[T <: Data](stream : Stream[Fragment[T]], footer: Vec[T]): Stream[Fragment[T]] = {
    val ret = cloneOf(stream)
    val counter = Counter(footer.size)

    ret.setIdle()
    stream.setBlocked()

    val fsm = new StateMachine {
      val content : State = new State with EntryPoint {
        whenIsActive {
          stream.replaceFragmentLast(False) >> ret
          when(stream.lastFire) {
            goto(addFooter)
          }
        }
      }

      val addFooter : State = new State {
        whenIsActive {
          ret.valid := True
          ret.payload.fragment := footer(counter.value)
          ret.payload.last := counter.willOverflowIfInc

          when(ret.fire) {
            counter.increment()
          }

          when(ret.lastFire) {
            goto(content)
          }
        }
      }
    }

    ret
  }
}

class StreamPipelinedMux[T1 <: Data, T2 <: Data](val dataType : HardType[T1],
                                                 val dataTypeOut : HardType[T2], n : Int, latency : Int = 8)
  extends ComponentWithFormalProperties with StreamMapComponent[TupleBundle2[UInt, T1], T2] {
  val selectType = UInt(log2Up(n) bits)
  val io = new Bundle {
    val input = slave Stream(TupleBundle(selectType, dataType()))
    val splitOutputs = Vec(master Stream(dataType), n)

    val splitInputs = Vec(slave Stream(dataTypeOut), n)
    val output = master Stream(dataTypeOut)
  }

  val rspFifo = StreamFifo(selectType, latency, latency = 0)

  val (outputStream, selectStream) = StreamFork2(io.input)

  val outputStreams = StreamDemux(outputStream, outputStream.payload._1, n)
  io.splitOutputs.zip(outputStreams).foreach(x => {
    x._1 <> x._2.map(_._2)
  })

  rspFifo.io.push <> selectStream.map(x => x._1)

  rspFifo.io.pop.ready := io.output.fire
  io.output <> StreamMux(rspFifo.io.pop.payload, io.splitInputs)

  override def InputStream() = io.input

  override def OutputStream(): Stream[T2] = io.output

  override def latency(): Int = latency
}

