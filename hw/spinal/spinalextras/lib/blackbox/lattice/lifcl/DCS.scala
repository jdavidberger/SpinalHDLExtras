package spinalextras.lib.blackbox.lattice.lifcl

import spinal.core._
import spinalextras.lib.Constraints

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

object DCS {
  def apply(clk0: ClockDomain, clk1: ClockDomain, sel : Bool): ClockDomain = {
    val dcs = DCS()
    dcs.io.CLK0 := clk0.readClockWire
    dcs.io.CLK1 := clk1.readClockWire
    dcs.io.SEL := sel

    val dcs_freq = clk0.frequency.getMax.max(clk1.frequency.getMax)
    dcs.addPrePopTask( () => {
      Constraints.create_clock(dcs.io.DCSOUT, dcs_freq)
    })

    ClockDomain(dcs.io.DCSOUT, frequency = FixedFrequency(
      dcs_freq
    ))
  }
}