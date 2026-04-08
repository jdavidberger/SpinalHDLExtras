package spinalextras.lib.blackbox.lattice.lifcl

import spinal.core._

class WDT(WDTEN : Boolean = false, WDTMODE : String = "SINGLE", WDTVALUE : Int = 0) extends BlackBox{
  if (WDTEN) {
    addGeneric("WDTEN", if (WDTEN) "DIS" else "EN")
  }
  if (WDTMODE != "SINGLE") {
    addGeneric("WDTMODE", WDTMODE)
  }
  if (WDTVALUE != 0) {
    addGeneric("WDTVALUE", WDTVALUE)
  }
  val io = new Bundle {
    val WDTRELOAD, WDT_CLK, WDT_RST = in Bool() default(False)
  }
  noIoPrefix()
}

object WDT {
  def noop() = {
    new WDT()
  }
}