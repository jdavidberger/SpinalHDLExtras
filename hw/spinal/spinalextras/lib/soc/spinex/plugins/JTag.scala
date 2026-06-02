package spinalextras.lib.soc.spinex.plugins

import spinal.core.{Component, IntToBuilder}
import spinal.lib.com.jtag.Jtag
import spinal.lib.{BufferCC, slave}
import spinalextras.lib.Constraints
import spinalextras.lib.debug.JtagChain
import spinalextras.lib.soc.spinex.{Spinex, SpinexPlugin, SpinexPluginAsync}

import scala.collection.mutable
import scala.language.postfixOps

class JTagPlugin extends SpinexPluginAsync {
  val jtags = new mutable.ArrayBuffer[Jtag]()

  lazy val jtag = slave(Jtag())
  var c : Component = null
  override def apply(som: Spinex): Unit = {
    som.io.valCallback(jtag, s"jtag")

    Constraints.create_clock(jtag.tck, 12 MHz)
    c = Component.current
    super.apply(som)
  }

  override def onDone(): Unit = {
    val restore = Component.push(c)
    val jtagChain = JtagChain(jtags:_*)
    jtagChain.io.jtag <> jtag
    restore.restore()
  }
}