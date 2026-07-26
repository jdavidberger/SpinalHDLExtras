package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim.{ScoreboardInOrder, StreamDriver, StreamMonitor, StreamReadyRandomizer}
import spinalextras.lib.Config
import spinalextras.lib.misc.{StreamFragmentWidthAdapterDecoder, StreamFragmentWidthAdapterEncoder}

import scala.collection.mutable

class StreamFragmentWidthAdapterTest extends AnyFunSuite {
  class RoundTrip(widthIn: Int, widthOut: Int, endianness: Endianness) extends Component {
    val io = new Bundle {
      val input = slave(Stream(Fragment(Bits(widthIn bits))))
      val output = master(Stream(Fragment(Bits(widthIn bits))))
    }

    val encoder = new StreamFragmentWidthAdapterEncoder(widthIn, widthOut, endianness)
    val decoder = new StreamFragmentWidthAdapterDecoder(widthIn, widthOut, endianness)

    encoder.io.input <> io.input
    decoder.io.input <> encoder.io.output
    io.output <> decoder.io.output
  }

  def runRoundTrip(widthIn: Int, widthOut: Int, endianness: Endianness, packets: Int = 200): Unit = {
    Config.sim.withWave
      .doSim(new RoundTrip(widthIn, widthOut, endianness)
        .setDefinitionName(s"StreamFragmentWidthAdapterRoundTrip_${widthIn}_${widthOut}_${endianness.getClass.getSimpleName}")) { dut =>
        dut.clockDomain.forkStimulus(100 MHz)
        SimTimeout(1 ms)

        val sco = ScoreboardInOrder[(BigInt, Boolean)]()
        val beatQueue = mutable.Queue[(BigInt, Boolean)]()

        val mask = (BigInt(1) << widthIn) - 1

        for (_ <- 0 until packets) {
          val beats = 1 + simRandom.nextInt(6)
          for (b <- 0 until beats) {
            val data = BigInt(widthIn, simRandom) & mask
            val last = b == beats - 1
            beatQueue.enqueue((data, last))
            sco.pushRef((data, last))
          }
        }

        StreamDriver(dut.io.input, dut.clockDomain) { payload =>
          if (beatQueue.isEmpty) {
            false
          } else {
            val (data, last) = beatQueue.dequeue()
            payload.fragment #= data
            payload.last #= last
            true
          }
        }

        StreamReadyRandomizer(dut.io.output, dut.clockDomain)

        StreamMonitor(dut.io.output, dut.clockDomain) { payload =>
          sco.pushDut((payload.fragment.toBigInt, payload.last.toBoolean))
        }

        dut.clockDomain.waitSamplingWhere(beatQueue.isEmpty && sco.matches >= packets)
        dut.clockDomain.waitSampling(50)

        sco.checkEmptyness()
      }
  }

  // widthOut evenly divides widthIn: footer-free demux/mux path, both endiannesses.
  for ((widthIn, widthOut) <- Seq((16, 8), (32, 16), (24, 8), (64, 32))) {
    for (endianness <- Seq(LITTLE, BIG)) {
      test(s"RoundTrip_regular_${widthIn}_${widthOut}_${endianness.getClass.getSimpleName}") {
        runRoundTrip(widthIn, widthOut, endianness)
      }
    }
  }

  // Irregular pairs (neither/only-the-wrong-direction divides): footer-based general packer.
  // BIG endianness is unsupported for this path (guarded by a `require` at elaboration time).
  for ((widthIn, widthOut) <- Seq((8, 12), (12, 8), (8, 32), (24, 40), (24, 16))) {
    test(s"RoundTrip_irregular_${widthIn}_${widthOut}") {
      runRoundTrip(widthIn, widthOut, LITTLE)
    }
  }
}
