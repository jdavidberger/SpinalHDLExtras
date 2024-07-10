package spinalextras.lib.blackbox.lattice.lifcl

import spinal.core._

import scala.language.postfixOps

object DCSMODE extends Enumeration {
  type mode = Value
  val VCC, GND, DCS, DCS_1, BUFGCECLK1, BUFGCECLK1_1, BUFGCECLK0, BUFGCECLK0_1, BUF0, BUF1 = Value
}

case class DCS(mode : DCSMODE.mode = DCSMODE.DCS) extends BlackBox {
  addGeneric("DCSMODE", mode.toString)
  val io = new Bundle {
    val CLK0 = in Bool()
    val CLK1 = in Bool()
    val SEL = in Bool()
    val SELFORCE = in Bool() default (False)

    val DCSOUT = out Bool()
  }
  noIoPrefix()
}
