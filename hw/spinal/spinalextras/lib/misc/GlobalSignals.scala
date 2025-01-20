package spinalextras.lib.misc

import spinal.core._
import spinal.lib._

object GlobalSignals {
  def externalize[T <: Data](payload : T): T = {
    externalize(payload, (t : T) => cloneOf(t), Component.current.parent)
  }

  def externalize[T <: Data](payload : T, topComponent : Component): T = {
    externalize(payload, (t : T) => cloneOf(t), topComponent)
  }

  def externalize[T <: Data](payload : T, copySignal : T => T, topComponent : Component = null): T = {
    val dir = direction_function(payload)

    var intermediate : T = payload
    var new_signal : Option[T] = None

    val name = payload.name
    var c = payload.component.parent
    while(c != topComponent) {
      val ctx = Component.push(c)

      var higher_payload = copySignal(payload)
      if(c.parent != topComponent) {
        higher_payload = dir(higher_payload)
      }

      if(new_signal.isEmpty) {
        new_signal = Some(higher_payload)
      }

      intermediate <> higher_payload
      intermediate.setWeakName(name)

      intermediate = higher_payload
      ctx.restore()
      c = c.parent
    }

    intermediate
  }

  def direction_function[T <: Data](payload : T): T => T = {
    payload match {
      case payload : IMasterSlave =>
        t : T => {
          if(payload.isSlaveInterface) t.asInstanceOf[IMasterSlave].intoSlave().asInstanceOf[T]
          else if(payload.isMasterInterface) t.asInstanceOf[IMasterSlave].intoMaster().asInstanceOf[T]
          else t
        }
      case _ => {
        (t : T) =>
          if(payload.isInput) in(t)
          else if(payload.isOutput) out(t)
          else t
      }
    }
  }
}