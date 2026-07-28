package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim.{ScoreboardInOrder, StreamDriver, StreamMonitor, StreamReadyRandomizer}
import spinalextras.lib.Config
import spinalextras.lib.misc.VariableWidthBytes

import scala.collection.mutable

class VariableWidthBytesTest extends AnyFunSuite {
  class WithoutZeroSize(width: Int) extends Component {
    val dataType = HardType(Bits(width bits))
    val io = new Bundle {
      val input = slave(Stream(Fragment(new VariableWidthBytes(dataType, allowZeroSize = true))))
      val output = master(Stream(Fragment(new VariableWidthBytes(dataType, allowZeroSize = false))))
    }
    io.output << VariableWidthBytes.withoutZeroSize(io.input)
  }

  case class InBeat(data: BigInt, size: Int, last: Boolean)
  case class OutBeat(data: BigInt, size: Int, last: Boolean)

  // Reference model for VariableWidthBytes.withoutZeroSize: a zero-size last beat is dropped and
  // merges into the immediately preceding beat (promoted to a full-size last beat instead); any
  // other beat passes through unchanged, modulo re-expressing its size without allowZeroSize.
  def expected(beats: Seq[InBeat], bytesPerBeat: Int): Seq[OutBeat] = {
    val out = mutable.ArrayBuffer[OutBeat]()
    var held: Option[BigInt] = None
    for (b <- beats) {
      if (!b.last) {
        held.foreach(h => out += OutBeat(h, bytesPerBeat, last = false))
        held = Some(b.data)
      } else if (b.size == 0) {
        val h = held.getOrElse(throw new IllegalArgumentException("zero-size beat with nothing held"))
        out += OutBeat(h, bytesPerBeat, last = true)
        held = None
      } else {
        held.foreach(h => out += OutBeat(h, bytesPerBeat, last = false))
        held = None
        out += OutBeat(b.data, b.size, last = true)
      }
    }
    out.toSeq
  }

  test("MergesZeroSizeLastBeatIntoThePrecedingFullBeat") {
    val width = 32
    val bytesPerBeat = width / 8

    Config.sim.withWave
      .doSim(new WithoutZeroSize(width).setDefinitionName("VariableWidthBytesWithoutZeroSize")) { dut =>
        dut.clockDomain.forkStimulus(100 MHz)
        SimTimeout(4 ms)

        val fullMask = (BigInt(1) << width) - 1
        val inBeats = mutable.Queue[InBeat]()
        val refBeats = mutable.Queue[OutBeat]()

        for (_ <- 0 until 300) {
          val beatsInPacket = 1 + simRandom.nextInt(4)
          val packet = (0 until beatsInPacket).map { b =>
            val isLast = b == beatsInPacket - 1
            val data = BigInt(width, simRandom) & fullMask
            // A single-beat packet has nothing to merge a zero-size beat into, so keep its size
            // (and any other beat with no predecessor) away from zero; only allow zero once there's
            // at least one preceding beat in the same packet.
            val size = if (!isLast) bytesPerBeat
              else if (beatsInPacket == 1) 1 + simRandom.nextInt(bytesPerBeat)
              else simRandom.nextInt(bytesPerBeat + 1)
            InBeat(data, size, isLast)
          }
          inBeats ++= packet
          refBeats ++= expected(packet, bytesPerBeat)
        }

        val expectedCount = refBeats.size
        val sco = ScoreboardInOrder[(BigInt, Int, Boolean)]()
        refBeats.foreach(b => sco.pushRef((b.data, b.size, b.last)))

        StreamDriver(dut.io.input, dut.clockDomain) { payload =>
          if (inBeats.isEmpty) {
            false
          } else {
            val beat = inBeats.dequeue()
            payload.fragment.payload #= beat.data
            payload.fragment.sizeCode #= beat.size
            payload.last #= beat.last
            true
          }
        }

        StreamReadyRandomizer(dut.io.output, dut.clockDomain)

        var dutCount = 0
        StreamMonitor(dut.io.output, dut.clockDomain) { payload =>
          val size = (payload.fragment.sizeCode.toBigInt + 1).toInt
          sco.pushDut((payload.fragment.payload.toBigInt, size, payload.last.toBoolean))
          dutCount += 1
        }

        dut.clockDomain.waitSamplingWhere(inBeats.isEmpty && dutCount >= expectedCount)
        dut.clockDomain.waitSampling(50)

        assert(dutCount == expectedCount, s"expected $expectedCount output beats, saw $dutCount")
        sco.checkEmptyness()
      }
  }

  test("AssertsOnAZeroSizeBeatWithNothingHeld") {
    val width = 32

    assertThrows[Throwable] {
      Config.sim
        .doSim(new WithoutZeroSize(width).setDefinitionName("VariableWidthBytesWithoutZeroSizeLoneEmptyPacket")) { dut =>
          dut.clockDomain.forkStimulus(100 MHz)
          SimTimeout(1 ms)

          dut.io.output.ready #= true
          dut.io.input.valid #= true
          // A single-beat, zero-size, last=true packet: no preceding beat in the same packet to
          // merge into, so this has no allowZeroSize=false representation.
          dut.io.input.fragment.payload #= 0
          dut.io.input.fragment.sizeCode #= 0
          dut.io.input.last #= true

          dut.clockDomain.waitSampling(10)
        }
    }
  }
}
