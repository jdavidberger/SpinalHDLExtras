package spinalextras.lib.blackbox.lattice.lifcl

import spinal.core._
import spinal.lib._

object ECLKDIV extends Enumeration {
  type Divs = Value
  val D2, D3P5, D4, D5 = Value
}

case class ECLKDIV(ECLK_DIV: ECLKDIV.Divs, GSR_ENABLE: Boolean = false) extends BlackBox {
  val generic = new Generic {
    val ECLK_DIV = ECLKDIV.this.ECLK_DIV.toString
    val GSR_ENABLE = if(ECLKDIV.this.GSR_ENABLE) "ENABLED" else "DISABLED"
  }

  val io = new Bundle {
    val DIVRST = in Bool()
    val ECLKIN = in Bool()
    val SLIP = in Bool()
    val DIVOUT = out Bool()
  }

  noIoPrefix
  setBlackBoxName("ECLKDIV")
  mapCurrentClockDomain(io.ECLKIN, io.DIVRST)
}