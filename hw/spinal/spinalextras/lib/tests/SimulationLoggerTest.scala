package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinalextras.lib.Config
import spinalextras.lib.logging.{GlobalLogger, SignalLogger}

import java.io.File
import scala.language.postfixOps

class SimulationLoggerTest extends AnyFunSuite {
  def buildDut() = new Component {
    val gpio = Bool()
    val io = new Bundle {
      val gpio = in(Bool())
    }
    io.gpio <> gpio

    GlobalLogger(Set("debug"), SignalLogger.flows(gpio))

    val simLog = GlobalLogger.create_simulation_logger(tags = Set("debug"))
  }

  test("SimulationLoggerTest - binary capture") {
    val outputFile = new File("simulations/SimulationLoggerTest/SimulationLoggerTest.bin")
    outputFile.getParentFile.mkdirs()
    outputFile.delete()

    Config.sim.doSim(buildDut().setDefinitionName("SimulationLoggerTestBin")) { dut =>
      dut.clockDomain.forkStimulus(100 MHz)
      dut.clockDomain.waitSampling()
      SimTimeout(200 us)

      dut.simLog.startCapture(dut.clockDomain, outputFile.getPath)

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

  test("SimulationLoggerTest - sqlite capture") {
    val outputFile = new File("simulations/SimulationLoggerTest/SimulationLoggerTestSqlite.sqlite")
    val binFile = new File("simulations/SimulationLoggerTest/SimulationLoggerTestSqlite.bin")
    outputFile.getParentFile.mkdirs()
    outputFile.delete()
    binFile.delete()

    Config.sim.doSim(buildDut().setDefinitionName("SimulationLoggerTestSqlite")) { dut =>
      dut.clockDomain.forkStimulus(100 MHz)
      dut.clockDomain.waitSampling()
      SimTimeout(200 us)

      dut.simLog.startCapture(dut.clockDomain, outputFile.getPath)

      for (i <- 0 until 200) {
        dut.io.gpio #= (i % 2) == 0
        dut.clockDomain.waitSampling()
      }

      for (i <- 0 until 50) {
        dut.clockDomain.waitSampling()
      }
    }

    assert(outputFile.exists(), s"Expected sqlite output at ${outputFile.getPath}")
    assert(outputFile.length() > 100, "Sqlite output file looks empty (smaller than an empty sqlite header)")
    assert(!binFile.exists(), "Intermediary .bin file should have been cleaned up after a successful conversion")
  }
}
