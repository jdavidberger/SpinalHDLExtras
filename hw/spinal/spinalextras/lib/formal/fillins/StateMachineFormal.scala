package spinalextras.lib.formal.fillins

import spinal.core.{Vec, when}
import spinal.lib.fsm.{StateFsm, StateMachine, StateMachineAccessor}
import spinalextras.lib.formal.{FormalProperties, FormalProperty, HasFormalProperties}

class StateMachineFormal(fsm : StateMachine) extends HasFormalProperties{
  override protected def formalProperties(): Seq[FormalProperty] = new FormalProperties(fsm) {
    val isValidState = Vec(fsm.states.map(fsm.isActive)).asBits.orR
    addFormalProperty(isValidState, f"${fsm} has corrupted state")
  }

  val _children = fsm.states.map(findFillin).map {
    case formalExt: HasFormalProperties =>
      formalExt
    case _ => null
  }.filter(_ != null)

  override def formalChildren(): Seq[HasFormalProperties] = _children
}

class StateFsmFormal[T <: StateMachineAccessor](state : StateFsm[T]) extends HasFormalProperties{
  override protected def formalProperties(): Seq[FormalProperty] = new FormalProperties(state) {
    when(!state.getStateMachineAccessor().isActive(state)) {
      addFormalProperty(state.fsm.isStateRegBoot(), "State inner FSM needs to be in boot when state isn't active")
    } otherwise {
      addFormalProperty(!state.fsm.isStateRegBoot(), "State inner FSM needs to be running when state is active")
    }
  }
}