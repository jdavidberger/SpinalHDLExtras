package spinalextras.lib.blackbox

import spinal.core._

class ClockGenerator(freqHz: HertzNumber, phaseNs: Double = 0.0) extends BlackBox {
  addRTLPath("hw/verilog/ClockGenerator.v")

  // Define generics
  addGeneric("FREQ_HZ", freqHz.toInt)
  addGeneric("PHASE_NS", phaseNs)

  // Define IO
  val io = new Bundle {
    val async_reset = in Bool() default(ClockDomain.current.isResetActive)
    val clk = out Bool()
  }

  // Map IO to actual port names

  noIoPrefix()
}