package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.core._
import spinal.lib.{Counter, CounterUpDown}
import spinalextras.lib.Config
import spinalextras.lib.misc.CounterTools

class CounterToolsTest extends AnyFunSuite {
  test("CounterUpDown") {
    Config.sim.doSim(
      new Component {
        val io = new Bundle {
          val up = in(Bool())
          val down = in(Bool())

          val isZero, isMax = out(Bool())
        }
        val counter = CounterUpDown(8)
        when(!counter.willOverflowIfInc && io.up) {
          counter.increment()
        }
        when(counter =/= 0 && io.down) {
          counter.decrement()
        }
        io.isZero := CounterTools.isZero(counter)
        io.isMax := CounterTools.isMax(counter)

      }
    ) { dut =>
      SimTimeout(5000 us)
      dut.clockDomain.forkStimulus(100 MHz)
      dut.io.up #= false
      dut.io.down #= false

      dut.clockDomain.waitSampling(10)

      for (i <- 0 until 10000) {
        dut.io.up #= simRandom.nextBoolean()
        dut.io.down #= simRandom.nextBoolean()
        dut.clockDomain.waitSampling()
      }

    }

  }

  test("Counter") {
    Config.sim.doSim(
      new Component {
        val io = new Bundle {
          val up, clear = in(Bool())

          val isZero, isMax, isLT4 = out(Bool())
        }
        val counter = Counter(8)
        when(!counter.willOverflowIfInc && io.up) {
          counter.increment()
        }
        when(io.clear) {
          counter.clear()
        }

        io.isZero := CounterTools.isZero(counter)
        io.isMax := CounterTools.isMax(counter)
        io.isLT4 := CounterTools.isLessThan(counter, 4)
      }
    ) { dut =>
      SimTimeout(5000 us)
      dut.clockDomain.forkStimulus(100 MHz)
      dut.io.up #= false
      dut.io.clear #= false
      dut.clockDomain.waitSampling(10)

      for (i <- 0 until 10000) {
        dut.io.up #= simRandom.nextBoolean()
        dut.io.clear #= simRandom.nextInt(10) >= 9
        dut.clockDomain.waitSampling()
      }

    }

  }
}
