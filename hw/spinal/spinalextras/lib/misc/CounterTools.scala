package spinalextras.lib.misc

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.regif.SymbolName
import spinalextras.lib.Config
import spinalextras.lib.formal.{FormalProperties, FormalProperty, HasFormalProperties}
import spinalextras.lib.misc.StreamTools.gcd

import scala.language.postfixOps

class CounterVariableChange(val rangeBits : Int) extends ImplicitArea [SInt] {
  val value = Reg(SInt(rangeBits + 1 bits)) init(0)

  private val increment = UInt(rangeBits bits)
  increment := 0
  def increment_by(v : UInt): Unit = {
    increment := v.resized
  }

  private val decrement = UInt(rangeBits bits)
  decrement := 0
  def decrement_by(v : UInt): Unit = {
    decrement := v
  }

  val delta = increment.asSInt - decrement.asSInt
  value := value + delta

  val willOverflow = (delta +^ value) > (value + delta)
  val willUnderflow = (value + delta) < 0

  override def implicitValue: SInt = value
}

class CounterUpDownUneven(val range : Int, val incBy : Int = 1, val decBy : Int = 1, underFlowAllowed : Boolean = false) extends ImplicitArea[UInt] with HasFormalProperties {
  val valueNext = UInt(log2Up(range + 1) bits)
  val value = RegNext(valueNext) init(0)

  require((range % incBy) == 0)
  require((range % decBy) == 0)

  val incrementIt = False
  val decrementIt = False
  val clearIt = False

  def increment(): Unit = incrementIt := True
  def decrement(): Unit = decrementIt := True
  def clear(): Unit = clearIt := True

  val delta = SInt(S(incBy).getBitsWidth.max(S(-decBy).getBitsWidth) bits)
  delta := 0

  valueNext := (value.asSInt + delta).asUInt.resized
  when(clearIt) {
    valueNext := 0
  } elsewhen(incrementIt && decrementIt) {
    delta := incBy - decBy
  } elsewhen(incrementIt) {
    delta := incBy
  } elsewhen(decrementIt) {
    delta := -decBy
  }

  val willOverflowIfInc: Bool = value > (range - incBy)
  val willUnderflowIfDec: Bool = value < decBy
  val willUderflow = value < delta.abs && delta >= S(0)

  //assert(value <= range, f"Usage overflow ${value} ${this}")

//  val isMax = CounterTools.isSingleTargetState(
//    moveTowardState = incrementIt, moveAwayFromState = decrementIt, is_in_state = value > (range - incBy),
//    is_almost_state = value >= (range - 2 * incBy),
//    is_state_on_clear = false, clear = clearIt
//  )
  val isMax = willOverflowIfInc

//  override lazy val formalValidInputs = new Composite(this, "formalValidInputs") {
//    val validIncrement = (!incrementIt || !willOverflowIfInc || (decrementIt && Bool(incBy <= decBy)))
//    val validDecrement = (!decrementIt || !willUnderflowIfDec || (incrementIt && Bool(incBy >= decBy)))
//    val valid = validDecrement && validIncrement
//  }.valid

  override def implicitValue: UInt = value

  /**
   * @return Returns the list of properties that must be true if the input to the given component-like class are to
   *         be considered valid, and thus signify that the `formalProperties` themselves are all true as well.
   *
   *         For complicated properties, consider using the helper class `FormalProperties`
   */
  override protected def formalInputProperties(): Seq[FormalProperty] = super.formalInputProperties() ++ (if(!underFlowAllowed) {
    Seq(FormalProperty(willUderflow, "Underflow disallowed"))
  }  else Seq())

  /**
   * @return The formal properties which should all be true if the formalInputProperties are true too. These are the main
   *         assertions we are concerned with defining and verifying in formal testing
   *
   *         For complicated properties, consider using the helper class `FormalProperties`
   */
  override protected def formalProperties(): Seq[FormalProperty] = new FormalProperties(this) {
    addFormalProperty(value <= range, f"Usage overflow ${value} ${this}, max ${range}")
    val inc_dec_gcd = gcd(incBy, decBy)
    addFormalProperty((value % inc_dec_gcd) === 0)
  }
}

object CounterTools {
  def multiply_by(counter: Counter, m : Int)= new Area {
    assert(counter.value <= counter.end)

    val end = m * counter.end
    val start = m * counter.start

    val valueNext = UInt(log2Up(end + 1) bit)
    val value = RegNext(valueNext) init(start)

    valueNext := value
    when(counter.willIncrement) {
      valueNext := (value + m).resized
    }

    if (isPow2(end + 1) && start == 0) {   //Check if using overflow follow the spec

    }
    else {
      when(counter.willOverflow){
        valueNext := U(start)
      }
    }

    assert(counter.value * m === value, "Mismatch multiply counter")
    assert(counter.valueNext * m === valueNext, "Mismatch multiply counter valuenext")
  }

  def isTargetState(moveTowardState : Bool,
                    moveAwayFromState : Bool, is_in_state : Bool,
                    is_almost_state : Bool,
                    is_almost_not_state : Bool,
                    is_state_on_clear : Boolean,
                    clear : Bool
                   )(implicit symbol: SymbolName): Bool = {
    val isTargetState_area = new Area {
      val is_in_state1 = CombInit(is_in_state)
      val clear1 = CombInit(clear)
      val was_in_state = RegNext(is_in_state1) init(is_state_on_clear)
      //report(Seq(_is_in_state, " ", was_in_state))

      val is_almost_state_c = CombInit(is_almost_state)
      val was_almost_state = RegNext(is_almost_state) init (False)
      val was_almost_not_state = RegNext(is_almost_not_state) init (False)

      val moved_toward = RegNext(moveTowardState && ~moveAwayFromState) init (False)
      val moved_away = RegNext(moveAwayFromState && ~moveTowardState) init (False)

      when(clear) {
        was_in_state := Bool(is_state_on_clear)
        Seq(was_almost_state, was_almost_not_state, moved_toward, moved_away).foreach(_ := False)
      }

      val was_target_and_still_target = (was_in_state && ~(was_almost_not_state && moved_away))
      val was_almost_target_and_moved_toward = (was_almost_state && moved_toward)
      val is_in_state_rtn = was_target_and_still_target || was_almost_target_and_moved_toward

      assert(is_in_state_rtn === is_in_state, s"Is in state check failed ${symbol.name} ${is_in_state}")
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

  def isSingleTargetState(moveTowardState : Bool,
                          moveAwayFromState : Bool, is_in_state : Bool,
                          is_almost_state : Bool,
                          is_state_on_clear : Boolean,
                         )(implicit symbol: SymbolName): Bool = {
    isTargetState(moveTowardState, moveAwayFromState, is_in_state, is_almost_state, is_in_state, is_state_on_clear, False)
  }

  def isZero(increment : Bool, decrement : Bool, is_empty : Bool, almost_empty : Bool, is_state_on_clear : Boolean,
             clear : Bool): Bool = {
    isSingleTargetState(decrement, increment, is_empty, almost_empty, is_state_on_clear, clear)
  }

  def isZero(value : UInt, increment : Bool, decrement : Bool, is_state_on_clear : Boolean, clear : Bool): Bool = {
    isZero(increment, decrement, value === 0, value === 1, is_state_on_clear, clear)
  }

  def isZero(counter: CounterUpDown): Bool = {
    assert(counter.value <= counter.maxValue)
    assert(counter.value > 0 || counter.incrementIt || !counter.decrementIt, "Underflow not permitted")
    isZero(counter.value, counter.incrementIt, counter.decrementIt, is_state_on_clear = true, False)
  }

  def isZero(counter: Counter): Bool = {
    isZero(counter.value, counter.willIncrement, False, is_state_on_clear = true, clear = counter.willClear)
  }

  def isMax(counter: CounterUpDown): Bool = {
    isSingleTargetState(counter.incrementIt, counter.decrementIt,
      counter.value === counter.stateCount - 1,
      counter.value === (counter.stateCount - 2),
      is_state_on_clear = false, False)
  }
  def isMax(counter: Counter): Bool = {
    isSingleTargetState(counter.willIncrement, False,
      counter.value === counter.maxValue,
      counter.value === (counter.maxValue - 1),
      is_state_on_clear = false, counter.willClear)
  }

  def isLessThan(counter: Counter, v: Int): Bool = {
    assert(counter.value <= counter.end)

    //report(Seq(v.toString, " ", counter.value, " ", (v > counter.value)))
    isTargetState(
      moveTowardState = False,
      moveAwayFromState = counter.willIncrement,
      is_in_state = (v > counter.value),
      is_almost_state = False,
      is_almost_not_state = counter.value === (v - 1),
      is_state_on_clear = true,
      clear = (counter.willClear || counter.willOverflow)
    )
  }

}

