package spinalextras.lib.blackbox.lattice.lifcl

import spinal.core._
import spinal.lib._

case class ECLKSYNC(GSR_ENABLE: Boolean = false) extends BlackBox {
  val generic = new Generic {
    val GSR_ENABLE = if(ECLKSYNC.this.GSR_ENABLE) "ENABLED" else "DISABLED"
  }

  val io = new Bundle {
    val ECLKIN = in Bool()
    val STOP = in Bool()
    val ECLKOUT = out Bool()
  }
  noIoPrefix()
  setBlackBoxName("ECLKSYNC")
  mapCurrentClockDomain(io.ECLKIN)
}
