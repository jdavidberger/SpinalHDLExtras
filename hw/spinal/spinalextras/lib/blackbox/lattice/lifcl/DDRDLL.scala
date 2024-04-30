package spinalextras.lib.blackbox.lattice.lifcl

import spinal.core._

import scala.language.postfixOps

class DDRDLL extends BlackBox {
  val CLKIN = in Bool() // Reference clock input to the DDRDLL. Should run at the same frequency as the clock to be delayed.
  val RST = in Bool() // Reset input to the DDRDLL.
  val UDDCNTL_N = in Bool() // Update control to update the delay code. When low, the delay code out of the DDRDLL is updated. Should not be active during a read or a write cycle.
  val FREEZE = in Bool() // Releases the DDRDLL input clock.
  //val DDRDEL = out Bool() // The delay codes from the DDRDLL to be used in DLLDEL.
  val LOCK = out Bool() // Lock output to indicate the DDRDLL has valid delay output.
  val DCNTL = out Bits(9 bits) // The delay codes from the DDRDLL available for the user IP.
  val CODE = out Bits(9 bits)

  mapCurrentClockDomain(clock = CLKIN, reset = RST)
}
