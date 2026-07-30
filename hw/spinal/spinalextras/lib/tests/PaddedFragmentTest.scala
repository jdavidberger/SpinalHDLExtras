package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim.{FlowMonitor, ScoreboardInOrder, StreamDriver, StreamMonitor, StreamReadyRandomizer}
import spinalextras.lib.Config
import spinalextras.lib.misc.{PaddedFragment, VariableWidthBytes}
import spinalextras.lib.misc.PaddedFragment._

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

  // asFragment()/asFragmentStream() should pass the fragment bits and last flag straight through
  // with no transformation, and lastFire should pulse exactly on a fired last beat.
  class AsFragmentStreamTest(width: Int) extends Component {
    val dataType = HardType(Bits(width bits))
    val io = new Bundle {
      val input = slave(Stream(PaddedFragment(dataType)))
      val output = master(Stream(Fragment(dataType)))
      val lastFire = out(Bool())
    }
    io.output << io.input.asFragmentStream()
    io.lastFire := io.input.lastFire
  }

  test("AsFragmentStreamAndLastFirePassThroughFragmentAndLast") {
    val width = 32

    Config.sim.doSim(new AsFragmentStreamTest(width).setDefinitionName("PaddedFragmentAsFragmentStream")) { dut =>
      dut.clockDomain.forkStimulus(100 MHz)
      SimTimeout(2 ms)

      case class Beat(data: BigInt, last: Boolean)
      val beatQueue = mutable.Queue[Beat]()
      for (_ <- 0 until 100) {
        val beats = 1 + simRandom.nextInt(3)
        for (b <- 0 until beats) beatQueue.enqueue(Beat(BigInt(width, simRandom), b == beats - 1))
      }
      val totalBeats = beatQueue.size
      val expected = mutable.Queue[Beat]() ++= beatQueue

      StreamDriver(dut.io.input, dut.clockDomain) { payload =>
        if (beatQueue.isEmpty) false
        else {
          val beat = beatQueue.dequeue()
          payload.fragment #= beat.data
          payload.last #= beat.last
          true
        }
      }
      StreamReadyRandomizer(dut.io.output, dut.clockDomain)

      var received = 0
      StreamMonitor(dut.io.output, dut.clockDomain) { payload =>
        val exp = expected.dequeue()
        assert(payload.fragment.toBigInt == exp.data, "asFragment should pass the fragment bits through unchanged")
        assert(payload.last.toBoolean == exp.last, "asFragment should pass last through unchanged")
        assert(dut.io.lastFire.toBoolean == exp.last, "lastFire should be high exactly on a fired last beat")
        received += 1
      }

      dut.clockDomain.waitSamplingWhere(expected.isEmpty)
      dut.clockDomain.waitSampling(20)

      assert(received == totalBeats)
    }
  }

  // insertHeader(header: T) should prepend a single non-last header beat to every packet, leaving
  // the original beats (and their last flags) unmodified.
  class InsertHeaderTest(width: Int, headerValue: BigInt) extends Component {
    val dataType = HardType(Bits(width bits))
    val io = new Bundle {
      val input = slave(Stream(PaddedFragment(dataType)))
      val output = master(Stream(PaddedFragment(dataType)))
    }
    io.output << io.input.insertHeader(B(headerValue, width bits))
  }

  test("InsertHeaderPrependsHeaderBeatToEachPacket") {
    val width = 32
    val headerValue = BigInt("DEADBEEF", 16)

    Config.sim.doSim(new InsertHeaderTest(width, headerValue).setDefinitionName("PaddedFragmentInsertHeader")) { dut =>
      dut.clockDomain.forkStimulus(100 MHz)
      SimTimeout(2 ms)

      case class Beat(data: BigInt, last: Boolean)
      val packets = for (_ <- 0 until 50) yield {
        val beats = 1 + simRandom.nextInt(3)
        (0 until beats).map(b => Beat(BigInt(width, simRandom), b == beats - 1))
      }
      val inputQueue = mutable.Queue[Beat]() ++= packets.flatten

      val expected = mutable.Queue[Beat]()
      for (packet <- packets) {
        expected.enqueue(Beat(headerValue, last = false))
        expected ++= packet
      }
      val totalBeats = expected.size

      StreamDriver(dut.io.input, dut.clockDomain) { payload =>
        if (inputQueue.isEmpty) false
        else {
          val beat = inputQueue.dequeue()
          payload.fragment #= beat.data
          payload.last #= beat.last
          true
        }
      }
      StreamReadyRandomizer(dut.io.output, dut.clockDomain)

      var received = 0
      StreamMonitor(dut.io.output, dut.clockDomain) { payload =>
        val exp = expected.dequeue()
        assert(payload.fragment.toBigInt == exp.data, s"expected fragment ${exp.data}, got ${payload.fragment.toBigInt}")
        assert(payload.last.toBoolean == exp.last, "last flag mismatch")
        received += 1
      }

      dut.clockDomain.waitSamplingWhere(expected.isEmpty)
      dut.clockDomain.waitSampling(20)

      assert(received == totalBeats)
    }
  }

  // insertHeader(header: Vec[T]) should prepend one non-last beat per header element, in order,
  // before forwarding the original packet's beats unmodified.
  class InsertHeaderVecTest(width: Int, headerValues: Seq[BigInt]) extends Component {
    val dataType = HardType(Bits(width bits))
    val io = new Bundle {
      val input = slave(Stream(PaddedFragment(dataType)))
      val output = master(Stream(PaddedFragment(dataType)))
    }
    val header = Vec(headerValues.map(v => B(v, width bits)))
    io.output << io.input.insertHeader(header)
  }

  test("InsertHeaderVecPrependsEveryHeaderBeatToEachPacket") {
    val width = 32
    val headerValues = Seq(BigInt("CAFEF00D", 16), BigInt("F00DCAFE", 16))

    Config.sim.doSim(new InsertHeaderVecTest(width, headerValues).setDefinitionName("PaddedFragmentInsertHeaderVec")) { dut =>
      dut.clockDomain.forkStimulus(100 MHz)
      SimTimeout(2 ms)

      case class Beat(data: BigInt, last: Boolean)
      val packets = for (_ <- 0 until 50) yield {
        val beats = 1 + simRandom.nextInt(3)
        (0 until beats).map(b => Beat(BigInt(width, simRandom), b == beats - 1))
      }
      val inputQueue = mutable.Queue[Beat]() ++= packets.flatten

      val expected = mutable.Queue[Beat]()
      for (packet <- packets) {
        headerValues.foreach(h => expected.enqueue(Beat(h, last = false)))
        expected ++= packet
      }
      val totalBeats = expected.size

      StreamDriver(dut.io.input, dut.clockDomain) { payload =>
        if (inputQueue.isEmpty) false
        else {
          val beat = inputQueue.dequeue()
          payload.fragment #= beat.data
          payload.last #= beat.last
          true
        }
      }
      StreamReadyRandomizer(dut.io.output, dut.clockDomain)

      var received = 0
      StreamMonitor(dut.io.output, dut.clockDomain) { payload =>
        val exp = expected.dequeue()
        assert(payload.fragment.toBigInt == exp.data, s"expected fragment ${exp.data}, got ${payload.fragment.toBigInt}")
        assert(payload.last.toBoolean == exp.last, "last flag mismatch")
        received += 1
      }

      dut.clockDomain.waitSamplingWhere(expected.isEmpty)
      dut.clockDomain.waitSampling(20)

      assert(received == totalBeats)
    }
  }

  // dropPaddingInformation() should forward each beat's raw payload bytes (masked to the valid
  // byte count on the last beat) and last flag, discarding the size metadata entirely.
  class DropPaddingInformationTest(width: Int) extends Component {
    val dataType = HardType(Bits(width bits))
    val io = new Bundle {
      val input = slave(Stream(Fragment(new VariableWidthBytes(dataType, allowZeroSize = true))))
      val output = master(Stream(Fragment(dataType)))
    }
    val padded = PaddedFragment.encodePaddingToStream(io.input)
    io.output << padded.dropPaddingInformation()
  }

  test("DropPaddingInformationForwardsPayloadBytesAndLast") {
    val width = 32
    val bytesPerBeat = width / 8

    Config.sim.doSim(new DropPaddingInformationTest(width).setDefinitionName("PaddedFragmentDropPaddingInformation")) { dut =>
      dut.clockDomain.forkStimulus(100 MHz)
      SimTimeout(4 ms)

      case class Beat(data: BigInt, size: Int, last: Boolean)
      val beatQueue = mutable.Queue[Beat]()
      val fullMask = (BigInt(1) << width) - 1

      // Keep the last beat's size below a full beat so encodePaddingToStream never has to
      // synthesize a zero-size trailer beat -- that merge behavior is covered by the
      // decodePaddingFromStream round-trip tests above, and would otherwise obscure this test's
      // beat-for-beat comparison.
      for (_ <- 0 until 100) {
        val beats = 1 + simRandom.nextInt(4)
        for (b <- 0 until beats) {
          val isLast = b == beats - 1
          val data = BigInt(width, simRandom) & fullMask
          val size = if (isLast) 1 + simRandom.nextInt(bytesPerBeat - 1) else bytesPerBeat
          beatQueue.enqueue(Beat(data, size, isLast))
        }
      }
      val expected = mutable.Queue[Beat]() ++= beatQueue
      val totalBeats = beatQueue.size

      StreamDriver(dut.io.input, dut.clockDomain) { payload =>
        if (beatQueue.isEmpty) false
        else {
          val beat = beatQueue.dequeue()
          payload.fragment.payload #= beat.data
          payload.fragment.sizeCode #= beat.size
          payload.last #= beat.last
          true
        }
      }
      StreamReadyRandomizer(dut.io.output, dut.clockDomain)

      def meaningfulBits(data: BigInt, last: Boolean, size: Int): BigInt = {
        if (!last) data else data & ((BigInt(1) << (size * 8)) - 1)
      }

      var received = 0
      StreamMonitor(dut.io.output, dut.clockDomain) { payload =>
        val exp = expected.dequeue()
        val got = meaningfulBits(payload.fragment.toBigInt, exp.last, exp.size)
        val want = meaningfulBits(exp.data, exp.last, exp.size)
        assert(got == want, s"beat mismatch: got $got want $want")
        assert(payload.last.toBoolean == exp.last, "last flag mismatch")
        received += 1
      }

      dut.clockDomain.waitSamplingWhere(expected.isEmpty)
      dut.clockDomain.waitSampling(20)

      assert(received == totalBeats)
    }
  }

  // decode() (the Stream[PaddedFragment[T]] extension) should be equivalent to calling
  // decodePaddingFromStream directly: sizeCode comes from validBytes(), payload is the raw
  // fragment, and last is preserved.
  class DecodeExtensionTest(width: Int) extends Component {
    val dataType = HardType(Bits(width bits))
    val io = new Bundle {
      val input = slave(Stream(PaddedFragment(dataType)))
      val output = master(Stream(Fragment(new VariableWidthBytes(dataType, allowZeroSize = true))))
    }
    io.output << io.input.decode(allowZeroSize = true)
  }

  test("DecodeExtensionMethodComputesSizeFromValidBytes") {
    val width = 32
    val bytesPerBeat = width / 8

    Config.sim.doSim(new DecodeExtensionTest(width).setDefinitionName("PaddedFragmentDecodeExtension")) { dut =>
      dut.clockDomain.forkStimulus(100 MHz)
      SimTimeout(2 ms)

      case class InBeat(data: BigInt, last: Boolean)
      case class OutBeat(payload: BigInt, size: Int, last: Boolean)

      val fullMask = (BigInt(1) << width) - 1
      val inputQueue = mutable.Queue[InBeat]()
      val expected = mutable.Queue[OutBeat]()

      for (_ <- 0 until 100) {
        val beats = 1 + simRandom.nextInt(3)
        for (b <- 0 until beats) {
          val isLast = b == beats - 1
          if (!isLast) {
            val data = BigInt(width, simRandom) & fullMask
            inputQueue.enqueue(InBeat(data, last = false))
            expected.enqueue(OutBeat(data, bytesPerBeat, last = false))
          } else {
            val size = simRandom.nextInt(bytesPerBeat + 1)
            val low = BigInt(width, simRandom) & ((BigInt(1) << (width - 8)) - 1)
            val data = low | (BigInt(size) << (width - 8))
            inputQueue.enqueue(InBeat(data, last = true))
            expected.enqueue(OutBeat(data, size, last = true))
          }
        }
      }
      val totalBeats = expected.size

      StreamDriver(dut.io.input, dut.clockDomain) { payload =>
        if (inputQueue.isEmpty) false
        else {
          val beat = inputQueue.dequeue()
          payload.fragment #= beat.data
          payload.last #= beat.last
          true
        }
      }
      StreamReadyRandomizer(dut.io.output, dut.clockDomain)

      var received = 0
      StreamMonitor(dut.io.output, dut.clockDomain) { payload =>
        val exp = expected.dequeue()
        assert(payload.fragment.payload.toBigInt == exp.payload, "decode should copy the raw fragment through")
        assert(payload.fragment.sizeCode.toBigInt == exp.size, "decode should compute sizeCode from validBytes")
        assert(payload.last.toBoolean == exp.last, "last should be preserved")
        received += 1
      }

      dut.clockDomain.waitSamplingWhere(expected.isEmpty)
      dut.clockDomain.waitSampling(20)

      assert(received == totalBeats)
    }
  }

  // fragmentMap should apply the given function to the fragment payload while preserving last.
  class FragmentMapTest(width: Int) extends Component {
    val dataType = HardType(UInt(width bits))
    val io = new Bundle {
      val input = slave(Stream(Fragment(dataType)))
      val output = master(Stream(Fragment(dataType)))
    }
    io.output << io.input.fragmentMap(x => x + 1)
  }

  test("FragmentMapAppliesFunctionToFragmentAndKeepsLast") {
    val width = 32
    val mask = (BigInt(1) << width) - 1

    Config.sim.doSim(new FragmentMapTest(width).setDefinitionName("PaddedFragmentFragmentMap")) { dut =>
      dut.clockDomain.forkStimulus(100 MHz)
      SimTimeout(2 ms)

      case class Beat(data: BigInt, last: Boolean)
      val beatQueue = mutable.Queue[Beat]()
      for (_ <- 0 until 100) {
        val beats = 1 + simRandom.nextInt(3)
        for (b <- 0 until beats) beatQueue.enqueue(Beat(BigInt(width, simRandom), b == beats - 1))
      }
      val expected = mutable.Queue[Beat]() ++= beatQueue
      val totalBeats = beatQueue.size

      StreamDriver(dut.io.input, dut.clockDomain) { payload =>
        if (beatQueue.isEmpty) false
        else {
          val beat = beatQueue.dequeue()
          payload.fragment #= beat.data
          payload.last #= beat.last
          true
        }
      }
      StreamReadyRandomizer(dut.io.output, dut.clockDomain)

      var received = 0
      StreamMonitor(dut.io.output, dut.clockDomain) { payload =>
        val exp = expected.dequeue()
        assert(payload.fragment.toBigInt == ((exp.data + 1) & mask), "fragmentMap should apply the function to the fragment")
        assert(payload.last.toBoolean == exp.last, "fragmentMap should preserve last")
        received += 1
      }

      dut.clockDomain.waitSamplingWhere(expected.isEmpty)
      dut.clockDomain.waitSampling(20)

      assert(received == totalBeats)
    }
  }

  // asPaddedFragmentStream() should be a pure repackaging: the fragment bits and last flag come
  // through unmodified, with no size information encoded.
  class AsPaddedFragmentStreamTest(width: Int) extends Component {
    val dataType = HardType(Bits(width bits))
    val io = new Bundle {
      val input = slave(Stream(Fragment(dataType)))
      val output = master(Stream(PaddedFragment(dataType)))
    }
    io.output << io.input.asPaddedFragmentStream()
  }

  test("AsPaddedFragmentStreamPassesFragmentAndLastThrough") {
    val width = 32

    Config.sim.doSim(new AsPaddedFragmentStreamTest(width).setDefinitionName("PaddedFragmentAsPaddedFragmentStream")) { dut =>
      dut.clockDomain.forkStimulus(100 MHz)
      SimTimeout(2 ms)

      case class Beat(data: BigInt, last: Boolean)
      val beatQueue = mutable.Queue[Beat]()
      for (_ <- 0 until 100) {
        val beats = 1 + simRandom.nextInt(3)
        for (b <- 0 until beats) beatQueue.enqueue(Beat(BigInt(width, simRandom), b == beats - 1))
      }
      val expected = mutable.Queue[Beat]() ++= beatQueue
      val totalBeats = beatQueue.size

      StreamDriver(dut.io.input, dut.clockDomain) { payload =>
        if (beatQueue.isEmpty) false
        else {
          val beat = beatQueue.dequeue()
          payload.fragment #= beat.data
          payload.last #= beat.last
          true
        }
      }
      StreamReadyRandomizer(dut.io.output, dut.clockDomain)

      var received = 0
      StreamMonitor(dut.io.output, dut.clockDomain) { payload =>
        val exp = expected.dequeue()
        assert(payload.fragment.toBigInt == exp.data, "asPaddedFragmentStream should preserve the fragment bits")
        assert(payload.last.toBoolean == exp.last, "asPaddedFragmentStream should preserve last")
        received += 1
      }

      dut.clockDomain.waitSamplingWhere(expected.isEmpty)
      dut.clockDomain.waitSampling(20)

      assert(received == totalBeats)
    }
  }

  // PaddedFragment.apply(f: Fragment[T]) is the other way to build a PaddedFragment from a
  // Fragment; it should behave identically to asPaddedFragmentStream (a pure repackaging).
  class PaddedFragmentApplyTest(width: Int) extends Component {
    val dataType = HardType(Bits(width bits))
    val io = new Bundle {
      val input = slave(Stream(Fragment(dataType)))
      val output = master(Stream(PaddedFragment(dataType)))
    }
    io.output << io.input.map(f => PaddedFragment(f))
  }

  test("PaddedFragmentApplyBuildsAPaddedFragmentFromAFragment") {
    val width = 32

    Config.sim.doSim(new PaddedFragmentApplyTest(width).setDefinitionName("PaddedFragmentApply")) { dut =>
      dut.clockDomain.forkStimulus(100 MHz)
      SimTimeout(2 ms)

      case class Beat(data: BigInt, last: Boolean)
      val beatQueue = mutable.Queue[Beat]()
      for (_ <- 0 until 100) {
        val beats = 1 + simRandom.nextInt(3)
        for (b <- 0 until beats) beatQueue.enqueue(Beat(BigInt(width, simRandom), b == beats - 1))
      }
      val expected = mutable.Queue[Beat]() ++= beatQueue
      val totalBeats = beatQueue.size

      StreamDriver(dut.io.input, dut.clockDomain) { payload =>
        if (beatQueue.isEmpty) false
        else {
          val beat = beatQueue.dequeue()
          payload.fragment #= beat.data
          payload.last #= beat.last
          true
        }
      }
      StreamReadyRandomizer(dut.io.output, dut.clockDomain)

      var received = 0
      StreamMonitor(dut.io.output, dut.clockDomain) { payload =>
        val exp = expected.dequeue()
        assert(payload.fragment.toBigInt == exp.data, "PaddedFragment.apply(Fragment) should preserve the fragment bits")
        assert(payload.last.toBoolean == exp.last, "PaddedFragment.apply(Fragment) should preserve last")
        received += 1
      }

      dut.clockDomain.waitSamplingWhere(expected.isEmpty)
      dut.clockDomain.waitSampling(20)

      assert(received == totalBeats)
    }
  }

  // toPaddedFragmentStream() should forward every input beat with last forced low, then append a
  // single synthetic all-zero last beat after each packet.
  class ToPaddedFragmentStreamTest(width: Int) extends Component {
    val dataType = HardType(Bits(width bits))
    val io = new Bundle {
      val input = slave(Stream(Fragment(dataType)))
      val output = master(Stream(PaddedFragment(dataType)))
    }
    io.output << io.input.toPaddedFragmentStream()
  }

  test("ToPaddedFragmentStreamAppendsZeroTrailerAfterEachPacket") {
    val width = 32

    Config.sim.doSim(new ToPaddedFragmentStreamTest(width).setDefinitionName("PaddedFragmentToPaddedFragmentStream")) { dut =>
      dut.clockDomain.forkStimulus(100 MHz)
      SimTimeout(4 ms)

      case class OutBeat(data: BigInt, last: Boolean)

      val inputQueue = mutable.Queue[(BigInt, Boolean)]()
      val expected = mutable.Queue[OutBeat]()

      for (_ <- 0 until 100) {
        val beats = 1 + simRandom.nextInt(4)
        for (b <- 0 until beats) {
          val isLast = b == beats - 1
          val data = BigInt(width, simRandom)
          inputQueue.enqueue((data, isLast))
          expected.enqueue(OutBeat(data, last = false))
        }
        expected.enqueue(OutBeat(BigInt(0), last = true))
      }
      val totalBeats = expected.size

      StreamDriver(dut.io.input, dut.clockDomain) { payload =>
        if (inputQueue.isEmpty) false
        else {
          val (data, last) = inputQueue.dequeue()
          payload.fragment #= data
          payload.last #= last
          true
        }
      }
      StreamReadyRandomizer(dut.io.output, dut.clockDomain)

      var received = 0
      StreamMonitor(dut.io.output, dut.clockDomain) { payload =>
        val exp = expected.dequeue()
        assert(payload.fragment.toBigInt == exp.data, s"expected fragment ${exp.data}, got ${payload.fragment.toBigInt}")
        assert(payload.last.toBoolean == exp.last, "last flag mismatch")
        received += 1
      }

      dut.clockDomain.waitSamplingWhere(expected.isEmpty)
      dut.clockDomain.waitSampling(20)

      assert(received == totalBeats)
    }
  }
}
