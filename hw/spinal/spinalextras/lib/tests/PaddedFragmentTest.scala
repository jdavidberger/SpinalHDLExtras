package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim.{FlowMonitor, ScoreboardInOrder, StreamDriver, StreamMonitor, StreamReadyRandomizer}
import spinalextras.lib.Config
import spinalextras.lib.misc.{PaddedFragment, VariableWidthBytes}

import scala.collection.mutable

class PaddedFragmentTest extends AnyFunSuite {
  // Wires a logical (unpadded) Fragment(VariableWidthBytes) stream through encodePaddingToStream to
  // get a padded wire stream, then decodePaddingFromStream + encodePaddingToStream again to see
  // whether re-encoding what was decoded reproduces the original padded stream exactly.
  class DecodeEncodeRoundTrip(width: Int) extends Component {
    val dataType = HardType(Bits(width bits))

    val io = new Bundle {
      val input = slave(Stream(Fragment(new VariableWidthBytes(dataType, allowZeroSize = true))))
      val padded = master(Flow(PaddedFragment(dataType)))
      val reEncoded = master(Stream(PaddedFragment(dataType)))
    }

    val padded = PaddedFragment.encodePaddingToStream(io.input, fragmentationError = false)
    val decoded = PaddedFragment.decodePaddingFromStream(padded)
    val reEncoded = PaddedFragment.encodePaddingToStream(decoded, fragmentationError = false)

    // padded.valid can stay high across multiple stalled cycles before it's actually consumed by
    // the internal decoder, so tap on fire (not valid) to report each beat exactly once.
    io.padded.valid := padded.fire
    io.padded.payload := padded.payload
    io.reEncoded << reEncoded
  }

  def runRoundTrip(width: Int, packets: Int = 200): Unit = {
    val bytesPerBeat = width / 8

    Config.sim.withWave
      .doSim(new DecodeEncodeRoundTrip(width).setDefinitionName(s"PaddedFragmentDecodeEncodeRoundTrip_$width")) { dut =>
        dut.clockDomain.forkStimulus(100 MHz)
        SimTimeout(4 ms)

        case class Beat(data: BigInt, size: Int, last: Boolean)
        val beatQueue = mutable.Queue[Beat]()
        val fullMask = (BigInt(1) << width) - 1

        for (_ <- 0 until packets) {
          val beats = 1 + simRandom.nextInt(4)
          for (b <- 0 until beats) {
            val isLast = b == beats - 1
            val data = BigInt(width, simRandom) & fullMask
            // Exercise every last-beat size, including a fully-utilized last beat (size ==
            // bytesPerBeat), which is the case that forces encodePaddingToStream to split into
            // a full non-last beat followed by a synthetic zero-length trailer.
            val size = if (isLast) simRandom.nextInt(bytesPerBeat + 1) else bytesPerBeat
            beatQueue.enqueue(Beat(data, size, isLast))
          }
        }

        StreamDriver(dut.io.input, dut.clockDomain) { payload =>
          if (beatQueue.isEmpty) {
            false
          } else {
            val beat = beatQueue.dequeue()
            payload.fragment.payload #= beat.data
            payload.fragment.sizeCode #= beat.size
            payload.last #= beat.last
            true
          }
        }

        StreamReadyRandomizer(dut.io.reEncoded, dut.clockDomain)

        val sco = ScoreboardInOrder[(BigInt, Boolean)]()
        var refCount = 0

        FlowMonitor(dut.io.padded, dut.clockDomain) { payload =>
          sco.pushRef((payload.fragment.toBigInt, payload.last.toBoolean))
          refCount += 1
        }
        StreamMonitor(dut.io.reEncoded, dut.clockDomain) { payload =>
          sco.pushDut((payload.fragment.toBigInt, payload.last.toBoolean))
        }

        dut.clockDomain.waitSamplingWhere(beatQueue.isEmpty && refCount > 0 && sco.matches >= refCount)
        dut.clockDomain.waitSampling(50)

        assert(refCount >= packets, "expected at least one padded beat per packet")
        sco.checkEmptyness()
      }
  }

  for (width <- Seq(16, 24, 32, 64)) {
    test(s"DecodeThenEncodeReproducesThePaddedStream_width$width") {
      runRoundTrip(width)
    }
  }
}
