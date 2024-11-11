package spinalextras.lib.misc

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.regif.SymbolName
import spinalextras.lib.Config

object CounterTools {
  def isTargetState(moveTowardState : Bool,
                    moveAwayFromState : Bool, is_in_state : Bool,
                    is_almost_state : Bool,
                    is_almost_not_state : Bool,
                    is_state_on_clear : Boolean,
                    clear : Bool
                   )(implicit symbol: SymbolName): Bool = {
    val isTargetState_area = new ResetArea(clear, true) {
      val was_in_state = RegNext(is_in_state) init(is_state_on_clear)

      val was_almost_state = RegNext(is_almost_state) init (False)
      val was_almost_not_state = RegNext(is_almost_not_state) init (False)

      val moved_toward = RegNext(moveTowardState && ~moveAwayFromState) init (False)
      val moved_away = RegNext(moveAwayFromState && ~moveTowardState) init (False)

      val was_target_and_still_target = (was_in_state && ~(was_almost_not_state && moved_away))
      val was_almost_target_and_moved_toward = (was_almost_state && moved_toward)
      val is_in_state_rtn = was_target_and_still_target || was_almost_target_and_moved_toward

      assert(is_in_state_rtn === is_in_state, s"Is in state check failed ${symbol.name}")
    }.setName(s"${symbol.name}_isTargetState_area")
    isTargetState_area.is_in_state_rtn
  }

  def isSingleTargetState(moveTowardState : Bool,
                    moveAwayFromState : Bool, is_in_state : Bool,
                    is_almost_state : Bool,
                    is_state_on_clear : Boolean,
                    clear : Bool
                   )(implicit symbol: SymbolName): Bool = {
    isTargetState(moveTowardState, moveAwayFromState, is_in_state, is_almost_state, is_in_state, is_state_on_clear, clear)
  }

  def isZero(increment : Bool, decrement : Bool, is_empty : Bool, almost_empty : Bool, is_state_on_clear : Boolean,
             clear : Bool): Bool = {
    isSingleTargetState(decrement, increment, is_empty, almost_empty, is_state_on_clear, clear)
  }

  def isZero(value : UInt, increment : Bool, decrement : Bool, is_state_on_clear : Boolean, clear : Bool): Bool = {
    isZero(increment, decrement, value === 0, value === 1, is_state_on_clear, clear)
  }

  def isZero(counter: CounterUpDown): Bool = {
    isZero(counter.value, counter.incrementIt, counter.decrementIt, is_state_on_clear = true, False)
  }

  def isZero(counter: Counter): Bool = {
    isZero(counter.value, counter.willIncrement, False, is_state_on_clear = true, clear = counter.willClear)
  }

  def isMax(counter: CounterUpDown): Bool = {
    isSingleTargetState(counter.incrementIt, counter.decrementIt,
      counter.value === counter.maxValue,
      counter.value === (counter.maxValue - 1),
      is_state_on_clear = false, False)
  }
  def isMax(counter: Counter): Bool = {
    isSingleTargetState(counter.willIncrement, False,
      counter.value === counter.maxValue,
      counter.value === (counter.maxValue - 1),
      is_state_on_clear = false, counter.willClear)
  }

  def isLessThan(counter: Counter, v: Int): Bool = {
    isTargetState(
      moveTowardState = False,
      moveAwayFromState = counter.willIncrement,
      is_in_state = v > counter.value,
      is_almost_state = False,
      is_almost_not_state = counter.value === (v - 1),
      is_state_on_clear = true,
      clear = counter.willClear
    )
  }

}

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

      for(i <- 0 until 10000) {
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

      for(i <- 0 until 10000) {
        dut.io.up #= simRandom.nextBoolean()
        dut.io.clear #= simRandom.nextInt(10) >= 9
        dut.clockDomain.waitSampling()
      }

    }

  }
}