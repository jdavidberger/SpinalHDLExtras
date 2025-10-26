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

  val output_stream = {
    val r = Flow(Bits(time_bits + dataWidth bits))
    var manual_trigger = io.manual_trigger
    if (ClockDomain.current != cd) {
      new ClockingArea(cd) {
        manual_trigger = BufferCC(manual_trigger)
      }
    }
    r.payload := io.syscnt.resize(time_bits bits) ## io.flow.payload
    r.valid := (io.channel_active && io.flow.valid) || manual_trigger
    if (ClockDomain.current == cd) {
      io.flow_fire := r.valid
      r.toStream(io.overflow).addFormalException(RegNext(io.overflow, init = False)).queue(localDepth)
    } else {
      val rCCed = r.toStream.queue(4, pushClock = cd, popClock = ClockDomain.current)
      rCCed.setName(s"${datumName}_fifo_cc")
      io.flow_fire := rCCed.fire
      rCCed
    }
  }

  assert(time_bits > 0, s"${io.flow} has too many bits for logger ${logBits} ${output_stream.payload.getBitsWidth} ${index_size}")

  val stamped_stream = output_stream.map(p => p ## B(idx, index_size bits))

  stamped_stream.stage().s2mPipe()  <> io.stamped_stream

  override def covers(): Seq[FormalProperty] = new FormalProperties(this) {
    addFormalProperty(io.manual_trigger)
    addFormalProperty(io.stamped_stream.fire)
    addFormalProperty(io.stamped_stream.isStall)
  }
}

object FlowLoggerDataCapture {
  def apply(logger: FlowLogger, dataType: HardType[Bits], datum: (Data, ClockDomain), idx: Int) = {
    new FlowLoggerDataCapture(dataWidth = dataType.getBitsWidth, index_size = logger.index_size, logBits = logger.logBits, datumName = datum._1.name, cd = datum._2, idx = idx)
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
