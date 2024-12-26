package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.core._
import spinal.lib.sim.{FlowMonitor, ScoreboardInOrder}
import spinalextras.lib.Config
import spinalextras.lib.mipi.{MIPIConfig, MIPIDataTypes, byte2pixel}

class byte2PixelTest extends AnyFunSuite {
  def doTest(cfg: MIPIConfig, f1: HertzNumber): Unit = {
    Config.sim.withConfig(Config.spinalConfig.copy(defaultClockDomainFrequency = FixedFrequency(f1))).doSim(
      new byte2pixel(cfg, ClockDomain.external("byte", frequency = FixedFrequency(cfg.dphy_byte_freq)))) { dut =>
      dut.io.mipi_header.valid #= false
      dut.io.payload.valid #= false

      dut.clockDomain.forkStimulus(f1)
      dut.byte_cd.forkStimulus(cfg.dphy_byte_freq)

      dut.byte_cd.waitSampling(1)

      dut.clockDomain.waitSampling(5)

      val sco = new ScoreboardInOrder[Int]

      FlowMonitor(dut.io.pixelFlow, dut.clockDomain) {
        px => sco.pushDut(px.toInt)
      }

      dut.io.mipi_header.payload.datatype #= cfg.ref_dt.id
      dut.io.mipi_header.payload.is_long_av_packet #= false
      dut.io.mipi_header.payload.is_long_packet #= false


      def send_mipi_hdr(dt: Int, long: Boolean): Unit = {
        dut.io.mipi_header.payload.datatype #= dt
        dut.io.mipi_header.valid #= true
        dut.io.mipi_header.is_long_packet #= long
        dut.io.mipi_header.is_long_av_packet #= long
        dut.byte_cd.waitSampling(1)
        dut.io.mipi_header.valid #= false
      }

      for (n <- 0 until 5) {
        send_mipi_hdr(0, false)
        send_mipi_hdr(cfg.ref_dt.id, true)
        dut.byte_cd.waitSampling(20)
        for (i <- 0 until 10) {
          for (j <- 0 until 20) {
            dut.io.payload.valid #= true
            val pix = j //simRandom.nextInt(1 << 16)
            dut.io.payload.payload #= pix
            dut.byte_cd.waitSampling()
            dut.io.payload.valid #= false
            dut.io.payload.payload #= simRandom.nextInt(1 << 10)
          }
        }
        dut.byte_cd.waitSampling(20)
        send_mipi_hdr(1, false)
        dut.byte_cd.waitSampling(20)
      }

      //sco.checkEmptyness()
    }
  }

  test("Basic_10bpp") {
    doTest(new MIPIConfig(2, 8, 1, MIPIDataTypes.RAW10, 50 MHz), 83.33 MHz)
  }
  test("Basic_4x12bpp") {
    doTest(new MIPIConfig(2, 8, 4, MIPIDataTypes.RAW12, 125 MHz), 83.33 MHz)
  }
}
