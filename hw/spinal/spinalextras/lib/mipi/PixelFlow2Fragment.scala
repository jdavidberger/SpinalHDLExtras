package spinalextras.lib.mipi

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib.sim._
import spinal.lib._
import spinalextras.lib.Config
import spinalextras.lib.logging.{GlobalLogger, SignalLogger}

class PixelFlow2Fragment[T <: Data](val dataType: HardType[T]) extends Component {
  val io = new Bundle {
    val pixelFlow = slave(PixelFlow(dataType))
    val pixelFragment = master (Flow(Fragment(dataType)))
  }

  val overflow = Bool()
  val lastValidPixel = io.pixelFlow.toStream(overflow).stage()
  assert(!overflow)

  io.pixelFragment <>
    lastValidPixel.
      continueWhen(~io.pixelFlow.frame_valid || io.pixelFlow.valid).
      addFragmentLast(~io.pixelFlow.frame_valid).toFlow

  GlobalLogger(
    SignalLogger.concat("p2f", io.pixelFragment.lastFire.setName("lastFire"), io.pixelFlow.frame_valid, overflow)
  )
}

object PixelFlow2Fragment {
  def apply[T <: Data](pixelFlow : PixelFlow[T]) : Flow[Fragment[T]] = {
    val dut = new PixelFlow2Fragment(pixelFlow.dataType)
    dut.io.pixelFlow <> pixelFlow
    dut.io.pixelFragment
  }
}

class PixelFlow2FragmentTest extends AnyFunSuite {
  test("Basic") {
    Config.sim.doSim(
      new Component {
        val dut = new PixelFlow2Fragment(Bits(12 bit))
        val metaP = PixelFlowMetaProvider(dut.dataType.getBitsWidth)
        val io = new Bundle {
          val pixelFlow = slave(PixelFlow(dut.dataType))
          val pixelFragment = master (Flow(Fragment(dut.dataType)))
          val meta = master (Flow(PixelFlowMeta()))
        }
        dut.io.pixelFlow <> io.pixelFlow
        dut.io.pixelFragment <> io.pixelFragment
        metaP.io.pixelFragment <> dut.io.pixelFragment
        io.meta <> metaP.io.meta
      }.setDefinitionName("PixelFlow2FragmentTest")
    ) { dut =>
      dut.clockDomain.forkStimulus(100 MHz)
      dut.io.pixelFlow.valid #= false
      dut.io.pixelFlow.frame_valid #= false
      dut.clockDomain.waitSampling(5)

      val sco = new ScoreboardInOrder[(Int, Boolean)]

      FlowMonitor(dut.io.pixelFragment, dut.clockDomain) {
        px => sco.pushDut((px.fragment.toInt, px.last.toBoolean))
      }

      for(n <- 0 until  5) {
        for(i <- 0 until  10) {
          dut.io.pixelFlow.frame_valid #= true
          dut.clockDomain.waitSampling(10)
          for (j <- 0 until  20) {
            dut.io.pixelFlow.valid #= true
            val pix = simRandom.nextInt(1 << 10)
            sco.pushRef((pix, i == 9 && j == 19))
            dut.io.pixelFlow.payload #= pix
            dut.clockDomain.waitSampling()
            dut.io.pixelFlow.valid #= false
            dut.io.pixelFlow.payload #= simRandom.nextInt(1 << 10)
          }
        }
        dut.io.pixelFlow.frame_valid #= false
        dut.clockDomain.waitSampling(1)
      }


      //
      //        dut.clockDomain.waitSampling(1)
      ////        for(j <- 0 to 10) {
      ////          dut.io.pixelFlow.frame_valid #= true
      ////          dut.io.pixelFlow.payload #= simRandom.nextInt(1 << 10)
      ////          dut.clockDomain.waitSampling(10)
      ////          dut.io.pixelFlow.frame_valid #= false
      ////          dut.io.pixelFlow.payload #= simRandom.nextInt(1 << 10)
      ////          dut.clockDomain.waitSampling(10)
      ////        }
      //      }
      //
      sco.checkEmptyness()
    }
  }
}
