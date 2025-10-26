package spinalextras.lib.soc.spinex.plugins

import spinal.lib.com.jtag.Jtag
import spinal.lib.slave
import spinalextras.lib.debug.JtagChain
import spinalextras.lib.soc.spinex.{Spinex, SpinexPlugin}

import scala.collection.mutable

class JTagPlugin extends SpinexPlugin {
  val jtags = new mutable.ArrayBuffer[Jtag]()

  lazy val jtag = slave(Jtag())
  override def apply(som: Spinex): Unit = {
    val jtagChain = JtagChain(jtags:_*)
    jtagChain.io.jtag <> jtag
    som.io.valCallback(jtag, s"jtag")
  }
}