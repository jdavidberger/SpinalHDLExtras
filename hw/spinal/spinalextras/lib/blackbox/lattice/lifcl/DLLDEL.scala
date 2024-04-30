package spinalextras.lib.blackbox.lattice.lifcl

import spinal.core._

import scala.language.postfixOps

class DLLDEL extends BlackBox {
  val CLKIN = in Bool() // Clock input.
  //val DDRDEL = in Bool() // Delay inputs from DDRDLL.
  val LOAD_N = in Bool() // Used to reset back to 90-degree delay.
  val MOVE = in Bool() // Pulse is required to change delay settings. The value on Direction is sampled at the falling edge of MOVE.
  val DIR = in Bool() // Indicates delay direction. 1 to decrease delay and 0 to increase delay.
  val COUT = out Bool() // Indicates the delay counter has reached its maximum value when moving up or minimum value when moving down.
  val CLKOUT = out Bool() // Delayed clock output.
  val CODE = in Bits(9 bits)

  mapCurrentClockDomain(clock = CLKIN)
}