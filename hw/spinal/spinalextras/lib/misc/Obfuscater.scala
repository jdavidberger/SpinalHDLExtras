package spinalextras.lib.misc

import spinal.core.internals.ExpressionContainer
import spinal.core.native.RefOwnerType
import spinal.core.{BlackBox, Bundle, ClockDomain, Component, Data, GlobalData, HardType, HertzNumber, Nameable, SpinalEnum, SpinalEnumCraft, out}
import spinal.lib.fsm.StateMachineTask

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Obfuscater {
  def apply(c : Component): Component = {
    val obf = new Obfuscater()
    obf.processComponent(c, is_top_level = true)
    c
  }
}

class Obfuscater() {
  val handled = new mutable.HashSet[Any]()
  var _id = 0
  var gprefix = ""

  def id: Int = {
    _id = _id + 1
    _id
  }

  def nextName() : String = {
    s"s${gprefix}_${id}"
  }

  def shouldExclude(n : Nameable): Boolean = {
    n match {
      case _ : BlackBox => true
      case null => false
      case _ => shouldExclude(n.refOwner.asInstanceOf[Nameable])
    }
  }
  def apply(component: Any): Unit = {
    if (handled.contains(component)) {
      return
    }
    handled.add(component)

    component match {
      case e: SpinalEnumCraft[_] => {
        processNameable(e)
        e.spinalEnum.elements.foreach(el => {
          el.setName(nextName())
        })
      }
      case e: SpinalEnum => {
        if(!shouldExclude(e)) {
          processNameable(e)
        }
      }
      case b: Bundle => {
        if(!shouldExclude(b)) {
          processBundle(b)
        }
      }
      case c: BlackBox => {
        c.getGroupedIO(false).foreach(exclude)
      }
      case c: Component => {
        processComponent(c, is_top_level = false)
      }
      case n: Nameable => {
        if(!shouldExclude(n)) {
          processNameable(n)
        }
      }
      case (str, d) => {
        apply(d)
      }
      case _: Int => {}
      case _: Byte => {}
      case _: String => {}
      case None => {}
      case _: Boolean => {}
      case _: ClockDomain => {}
      case _: HertzNumber => {}
      case _: HardType[Data] => {}
      case s: spinal.core.internals.ScopeStatement => {
        s.walkStatements(apply)
      }
      case _: GlobalData => {}
      case _: ExpressionContainer => {}
      case t: Traversable[Any] =>
        t.foreach(x => this(x))
      case s : spinal.core.ScopeProperty.Capture => {
        s.context.mutableMap.foreach(x => apply(x._2))
      }
      case _ => {
        println(s"Unknown component ${component} ${component.getClass}")
        return
      }
    }
  }

  def processNameable(nameable: Nameable): Unit = {
    nameable.foreachReflectableNameables(that => apply (that))
    if (nameable.name != null && nameable.name != "") {
      nameable.setName(nextName())
    }
  }

  def processBundle(component: Bundle): Unit = {
    processNameable(component.asInstanceOf[Nameable])
    component.elements.foreach(x => this (x._2))
  }

  def exclude(n: Nameable): Unit = {
    if (handled.contains(n)) {
      return
    }
    handled.add(n)

    n match {
      case b: Bundle => {
        b.elements.foreach(x => exclude(x._2))
      }
      case _ => {}
    }
  }

  def processComponent(component: Component, is_top_level: Boolean): Component = {
    if (!is_top_level) {
      component.getAllIo.foreach(x => apply(x))
      component.setDefinitionName(nextName())
      component.setName(nextName())
    } else {
      gprefix = component.name.hashCode.abs.toString
      component.getGroupedIO(true).foreach(exclude)
      processNameable(component.asInstanceOf[Nameable])
    }
    component.dslBody.walkDeclarations(x => apply(x))
    component.children.foreach(x => apply(x))
    component
  }
}
