package spinalextras.lib.blackbox.lattice.lifcl

import spinal.core._
import spinal.lib._
import spinal.lib.blackbox.lattice.ecp5.JtaggIo
import spinal.lib.com.jtag.Jtag

case class JtagGeneric (
                         var MCER1EXIST : String = "EXIST",
                         var MCER2EXIST : String = "EXIST" ) extends Generic {}

class JTAG( gen : JtagGeneric = JtagGeneric().copy() ) extends BlackBox {
  val io = new Bundle {
    val jtagio = master(JtaggIo())
    val jtag = slave(Jtag())
  }
  io.jtagio.setPartialName("")
  io.jtag.setPartialName("")
  io.jtag.elements.foreach(x => x._2.setPartialName(x._1.toUpperCase))
  val generic = gen

  noIoPrefix()
}