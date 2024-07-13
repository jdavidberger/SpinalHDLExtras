package spinalextras.lib.blackbox.lattice.lifcl

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class SP512K(
               OUTREG : Boolean = false,
               GSR : Boolean = true,
               ASYNC_RESET : Boolean = false,
               ASYNC_RESET_RELEASE : Boolean = false,
               ENABLE_ECC : Boolean = false //Enable ECC or Byte-enable support
             ) extends BlackBox {

  addGeneric("OUTREG", if(OUTREG) "OUT_REG" else "NO_REG")
  addGeneric("GSR", if(GSR) "ENABLED" else "DISABLED")
  addGeneric("RESETMODE", if(ASYNC_RESET) "ASYNC" else "SYNC")
  addGeneric("ASYNC_RESET_RELEASE", if(ASYNC_RESET_RELEASE) "ASYNC" else "SYNC")
  addGeneric("ECC_BYTE_SEL", if(ENABLE_ECC) "ECC_EN" else "BYTE_EN")

  val io = new Bundle {
    val CLK = in Bool()

    val DI = in Bits(32 bits)
    val AD = in Bits(14 bits)
    val CE = in Bool()
    val WE = in Bool()
    val CS = in Bool()
    val RSTOUT = in Bool()
    val CEOUT = in Bool()
    val BYTEEN_N = in Bits(4 bits)

    val DDO = out Bits(32 bits)
    val ERRDECA = out Bits(2 bits)
    val ERRDECB = out Bits(2 bits)
  }
  noIoPrefix()

  // Map the generic clock
  mapCurrentClockDomain(clock=io.CLK, reset=io.RSTOUT)
}
