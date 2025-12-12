package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim.{SimBaseTypePimper, SimBoolPimper, SimClockDomainHandlePimper, SimTimeout}
import spinal.core.{Bool, Bundle, Component, IntToBuilder, cloneOf, in}
import spinal.lib.sim.StreamMonitor
import spinal.lib.{StreamFifo, master}
import spinalextras.lib.Config
import spinalextras.lib.logging.{FlowLogger, SignalLogger}

class SignalLoggerTest extends AnyFunSuite {
  test("SignalLoggerTest") {
    Config.sim.doSim(
      new Component {
        val gpio = Bool()
        val logger = FlowLogger(SignalLogger.flows(gpio))
        val fifo = StreamFifo(cloneOf(logger.io.log.payload), 4)
        val io = new Bundle {
          val gpio = in(Bool())
          val log = master(cloneOf(logger.io.log))
          val flush = in(Bool())
        }
        io.flush <> fifo.io.flush
        io.gpio <> gpio

        logger.io.manual_trigger.setIdle()
        logger.io.log <> fifo.io.push
        io.log <> fifo.io.pop

      }.setDefinitionName("SignalLoggerTest")
    ) { dut =>
      dut.clockDomain.forkStimulus(100 MHz)
      dut.clockDomain.waitSampling()
      SimTimeout(100 us)

      dut.io.log.ready #= false
      dut.io.flush #= false

      for (i <- 0 until 200) {
        dut.io.gpio #= (i % 2) == 0
        dut.clockDomain.waitSampling()
      }

      dut.io.log.ready #= true
      StreamMonitor(dut.io.log, dut.clockDomain) { payload =>
        println(f">> ${payload.toBigInt >> 2} ${payload.toBigInt & 2}")
      }

      for (i <- 0 until 20) {
        dut.io.gpio #= (i % 2) == 0
        dut.clockDomain.waitSampling()
      }

      for (i <- 0 until 20) {
        dut.clockDomain.waitSampling()
      }

      println(s"Dropped  ${dut.logger.io.dropped_events.toBigInt}")
      println(s"Captured ${dut.logger.io.captured_events.toBigInt}")
    }

  }
}
