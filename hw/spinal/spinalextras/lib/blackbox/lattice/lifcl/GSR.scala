package spinalextras.lib.blackbox.lattice.lifcl

import spinal.core._

class GSR extends BlackBox {
  val io = new Bundle {
    val GSR_N = in Bool()
    val CLK = in Bool()
  }

  noIoPrefix()
  mapCurrentClockDomain(io.CLK, io.GSR_N, resetActiveLevel = LOW)
}

object GSR {
  def no_op() : GSR = {
    new ClockingArea(new ClockDomain(False, False)) {
      val gsr = new GSR().setName("GSR_INST")
    }.gsr
  }

}