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
  class DecodeEncodeRoundTrip(width: Int, decodeAllowZeroSize: Boolean) extends Component {
    val dataType = HardType(Bits(width bits))

    val io = new Bundle {
      val input = slave(Stream(Fragment(new VariableWidthBytes(dataType, allowZeroSize = true))))
      val padded = master(Flow(PaddedFragment(dataType)))
      val reEncoded = master(Stream(PaddedFragment(dataType)))
      val fragmentationError = out(Bool())
    }

    val firstEncodeError = Bool()
    val secondEncodeError = Bool()
    io.fragmentationError := firstEncodeError || secondEncodeError

    val padded = PaddedFragment.encodePaddingToStream(io.input, firstEncodeError)
    val decoded = PaddedFragment.decodePaddingFromStream(padded, decodeAllowZeroSize)
    val reEncoded = PaddedFragment.encodePaddingToStream(decoded, secondEncodeError)

    // padded.valid can stay high across multiple stalled cycles before it's actually consumed by
    // the internal decoder, so tap on fire (not valid) to report each beat exactly once.
    io.padded.valid := padded.fire
    io.padded.payload := padded.payload
    io.reEncoded << reEncoded
  }

  def runRoundTrip(width: Int, decodeAllowZeroSize: Boolean, lastSizeRange: (Int, Int), packets: Int = 200): Unit = {
    val bytesPerBeat = width / 8
    val (minLastSize, maxLastSize) = lastSizeRange

    Config.sim.withWave
      .doSim(new DecodeEncodeRoundTrip(width, decodeAllowZeroSize)
        .setDefinitionName(s"PaddedFragmentDecodeEncodeRoundTrip_${width}_$decodeAllowZeroSize")) { dut =>
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
            val size = if (isLast) minLastSize + simRandom.nextInt(maxLastSize - minLastSize + 1) else bytesPerBeat
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

        // On a last beat, only the low `validBytes` data bytes and the top (count) byte are
        // meaningful; the bytes in between are unconstrained padding. Those padding bytes are
        // *not* guaranteed to survive decodeAllowZeroSize=false, since withoutZeroSize buffers
        // beats (shifting exactly what garbage happens to be in flight when a trailer is
        // synthesized), so mask them out of the comparison rather than comparing raw bits.
        def meaningfulBits(data: BigInt, last: Boolean): BigInt = {
          if (!last) {
            data
          } else {
            val validBytes = ((data >> (width - 8)) & 0xff).toInt
            val lowMask = (BigInt(1) << (validBytes * 8)) - 1
            val topByteMask = ((BigInt(1) << 8) - 1) << (width - 8)
            data & (lowMask | topByteMask)
          }
        }

        val sco = ScoreboardInOrder[(BigInt, Boolean)]()
        var refCount = 0

        FlowMonitor(dut.io.padded, dut.clockDomain) { payload =>
          val last = payload.last.toBoolean
          sco.pushRef((meaningfulBits(payload.fragment.toBigInt, last), last))
          refCount += 1
        }
        StreamMonitor(dut.io.reEncoded, dut.clockDomain) { payload =>
          val last = payload.last.toBoolean
          sco.pushDut((meaningfulBits(payload.fragment.toBigInt, last), last))
        }

        var sawFragmentationError = false
        dut.clockDomain.onSamplings {
          if (dut.io.fragmentationError.toBoolean) sawFragmentationError = true
        }

        dut.clockDomain.waitSamplingWhere(beatQueue.isEmpty && refCount > 0 && sco.matches >= refCount)
        dut.clockDomain.waitSampling(50)

        assert(refCount >= packets, "expected at least one padded beat per packet")
        assert(!sawFragmentationError, "well-formed input should never raise fragmentationError")
        sco.checkEmptyness()
      }
  }

  for (width <- Seq(16, 24, 32, 64)) {
    test(s"DecodeThenEncodeReproducesThePaddedStream_width$width") {
      // Exercise every last-beat size, including a fully-utilized one (size == bytesPerBeat),
      // which is the case that forces encodePaddingToStream to split into a full non-last beat
      // followed by a synthetic zero-length trailer -- decode must be told allowZeroSize=true to
      // see that trailer rather than asserting on it.
      runRoundTrip(width, decodeAllowZeroSize = true, lastSizeRange = (0, width / 8))
    }
  }

  for (width <- Seq(16, 32)) {
    test(s"DecodeDefaultAllowZeroSizeFalse_width$width") {
      // The default (allowZeroSize=false) decode drops zero-size beats via
      // VariableWidthBytes.withoutZeroSize, merging them back into the preceding full beat -- so
      // this still needs to exercise the fully-utilized last beat (size == bytesPerBeat), which is
      // the case that produces that zero-size trailer in the first place. It must avoid size == 0
      // as an *input*, though: a single-beat packet with size 0 has no preceding beat to merge
      // into and has no allowZeroSize=false representation at all.
      runRoundTrip(width, decodeAllowZeroSize = false, lastSizeRange = (1, width / 8))
    }
  }

  // encodePaddingToStream assumes non-last beats are always full width; check that the
  // fragmentationError output actually reports a violation of that assumption (rather than being
  // wired to nothing), while the stream still keeps flowing instead of locking up.
  class EncodeOnly(width: Int) extends Component {
    val dataType = HardType(Bits(width bits))
    val io = new Bundle {
      val input = slave(Stream(Fragment(new VariableWidthBytes(dataType, allowZeroSize = true))))
      val output = master(Stream(PaddedFragment(dataType)))
      val fragmentationError = out(Bool())
    }
    io.output << PaddedFragment.encodePaddingToStream(io.input, io.fragmentationError)
  }

  test("FragmentationErrorFlagsANonFullNonLastBeat") {
    val width = 32
    val bytesPerBeat = width / 8

    Config.sim
      .doSim(new EncodeOnly(width).setDefinitionName("PaddedFragmentEncodeOnlyFragmentationError")) { dut =>
        dut.clockDomain.forkStimulus(100 MHz)
        SimTimeout(1 ms)

        dut.io.input.valid #= false
        dut.io.output.ready #= true
        dut.clockDomain.waitSampling(2)

        // A non-last beat that isn't full width -- violates encodePaddingToStream's assumption.
        dut.io.input.fragment.payload #= 0
        dut.io.input.fragment.sizeCode #= bytesPerBeat - 2
        dut.io.input.last #= false
        dut.io.input.valid #= true
        dut.clockDomain.waitSamplingWhere(dut.io.input.ready.toBoolean)

        var sawFragmentationError = false
        dut.clockDomain.onSamplings {
          if (dut.io.fragmentationError.toBoolean) sawFragmentationError = true
        }
        dut.clockDomain.waitSampling(1)
        assert(sawFragmentationError, "a non-full non-last beat should raise fragmentationError")

        // The stream should keep flowing afterwards rather than getting stuck.
        dut.io.input.fragment.sizeCode #= 1
        dut.io.input.last #= true
        dut.clockDomain.waitSamplingWhere(dut.io.input.ready.toBoolean)
        dut.io.input.valid #= false

        dut.clockDomain.waitSampling(10)
      }
  }
}
