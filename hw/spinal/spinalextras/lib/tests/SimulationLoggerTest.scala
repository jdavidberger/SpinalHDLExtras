package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinalextras.lib.Config
import spinalextras.lib.logging.{GlobalLogger, SignalLogger}

import java.io.File
import scala.language.postfixOps

class SimulationLoggerTest extends AnyFunSuite {
  test("SimulationLoggerTest") {
    val outputFile = new File("simulations/SimulationLoggerTest/SimulationLoggerTest.bin")
    outputFile.getParentFile.mkdirs()
    outputFile.delete()

    Config.sim.doSim(
      new Component {
        val gpio = Bool()
        val io = new Bundle {
          val gpio = in(Bool())
        }
        io.gpio <> gpio

        GlobalLogger(Set("debug"), SignalLogger.flows(gpio))

        val simLog = GlobalLogger.create_simulation_logger(tags = Set("debug"), filename = outputFile.getPath)
      }.setDefinitionName("SimulationLoggerTest")
    ) { dut =>
      dut.clockDomain.forkStimulus(100 MHz)
      dut.clockDomain.waitSampling()
      SimTimeout(200 us)

      dut.simLog.startCapture(dut.clockDomain)

      for (i <- 0 until 200) {
        dut.io.gpio #= (i % 2) == 0
        dut.clockDomain.waitSampling()
      }

      for (i <- 0 until 50) {
        dut.clockDomain.waitSampling()
      }
    }

    assert(outputFile.exists(), s"Expected simulation logger output at ${outputFile.getPath}")
    assert(outputFile.length() > 0, "Simulation logger output file is empty")
    assert(outputFile.length() % 12 == 0, "Simulation logger output is not a multiple of 12-byte records")
  }
}
