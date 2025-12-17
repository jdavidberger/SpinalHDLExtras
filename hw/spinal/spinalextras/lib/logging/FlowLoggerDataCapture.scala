package spinalextras.lib.logging

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._
import spinalextras.lib.formal.StreamFormal.StreamExt
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties, FormalProperty}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

import scala.language.postfixOps

class FlowLoggerDataCapture(dataWidth: Int, val index_size: Int, val logBits: Int = 95, val sysclkBits: Int = 64,
                            val datumName: String = "", var cd: ClockDomain = null, idx: Int, localDepth : Int = 0) extends ComponentWithFormalProperties {
  if (cd == null) {
    cd = ClockDomain.current
  }

  val io = new Bundle {
    val flow = slave Flow (Bits(dataWidth bits))
    val manual_trigger = in Bool()
    val channel_active = in Bool()

    val flow_fire = out Bool()

    val syscnt = in(UInt(sysclkBits bits))

    val stamped_stream = master(Stream(Bits(logBits bits)))
    val overflow = out(Bool())
  }

  val time_bits = logBits - dataWidth - index_size

  var manual_trigger = io.manual_trigger
  var channel_active = io.channel_active
  if (ClockDomain.current != cd) {
    new ClockingArea(cd) {
      manual_trigger = BufferCC(manual_trigger)
      channel_active = BufferCC(channel_active)
    }
  }

  val flow = cloneOf(io.flow)
  flow.payload := io.flow.payload
  flow.valid := (channel_active && io.flow.valid) || manual_trigger
  io.overflow := False

  val output_stream_in_current_clock = {
    if (ClockDomain.current != cd) {
      val overflow = Bool()
      val rCCed = flow.toStream(overflow).queue(4, pushClock = cd, popClock = ClockDomain.current)
      when(BufferCC(overflow)) {
        io.overflow := True
      }
      rCCed.setName(s"${datumName}_fifo_cc")
      rCCed.toFlow
    } else {
      flow
    }
  }

  val output_stream = {
    val r = Flow(Bits(time_bits + dataWidth bits))
    r.payload := io.syscnt.resize(time_bits bits) ## output_stream_in_current_clock.payload
    r.valid := output_stream_in_current_clock.valid
    io.flow_fire := r.valid

    val overflow = Bool()
    when(overflow) {
      io.overflow := overflow
    }
    r.toStream(overflow).addFormalException(RegNext(io.overflow, init = False)).queue(localDepth)
  }

  assert(time_bits > 0, s"${io.flow} has too many bits for logger ${logBits} ${output_stream.payload.getBitsWidth} ${index_size}")

  val stamped_stream = output_stream.stage().s2mPipe().map(p => p ## B(idx, index_size bits))

  stamped_stream <> io.stamped_stream

  override def covers(): Seq[FormalProperty] = new FormalProperties(this) {
    addFormalProperty(io.manual_trigger)
    addFormalProperty(io.stamped_stream.fire)
    addFormalProperty(io.stamped_stream.isStall)
  }
}

object FlowLoggerDataCapture {
  def apply(logger: FlowLogger, dataType: HardType[Bits], datum: (Data, ClockDomain), idx: Int, localDepth : Int = 0) = {
    new FlowLoggerDataCapture(dataWidth = dataType.getBitsWidth, index_size = logger.index_size, logBits = logger.cfg.logBits, datumName = datum._1.name, cd = datum._2, idx = idx, localDepth = localDepth)
  }
}

class FlowLoggerDataCaptureFormalTest extends AnyFunSuite with FormalTestSuite {
  formalTests().foreach(t => test(t._1) { t._2() })

  override def defaultDepth(): Int = 20
  override def CoverConfig() = formalConfig.withCover(100)

  override def generateRtl() = Seq(
    ("Basic", () => GeneralFormalDut ( () => new FlowLoggerDataCapture(8, 5, idx = 0)))
  )
}
