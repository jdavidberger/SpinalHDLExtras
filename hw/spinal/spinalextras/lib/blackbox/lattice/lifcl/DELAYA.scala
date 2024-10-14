package spinalextras.lib.blackbox.lattice.lifcl


import spinal.core._

import scala.language.postfixOps

case class DELAYA(
  // Sets the delay mode to be used. Options: SCLK_ZEROHOLD, ECLK_ALIGNED, ECLK_CENTERED, SCLK_ALIGNED, SCLK_CENTERED, DQS_CMD_CLK, DQS_ALIGNED_X2, DQS_ALIGNED_X4, DQS_CENTERED_X2, DQS_CENTERED_X4
  DEL_MODE: String = "USER_DEFINED",
  // Sets delay value when DEL_MODE is set to USER_DEFINED.
  DEL_VALUE: String = "0",
  // Select MC1 or CIB coarse delay code control: 0-Selects from MC1(STATIC), 1-Selects from CIB(DYNAMIC)
  COARSE_DELAY_MODE: String = "STATIC",
  // Coarse delay setting for lol delay cell.
  COARSE_DELAY: String = "0NS",
  // To enable edge monitor when in a IDDR X2, X7to1, X4, or X5 mode.
  EDGE_MONITOR: String = "DISABLED",
  // Used for SPI4.2 implementation.
  WAIT_FOR_EDGE: String = "DISABLED"
) extends BlackBox {

  addGeneric("DEL_MODE", DEL_MODE)
  addGeneric("DEL_VALUE", DEL_VALUE)
  addGeneric("COARSE_DELAY_MODE", COARSE_DELAY_MODE)
  addGeneric("COARSE_DELAY", COARSE_DELAY)
  addGeneric("EDGE_MONITOR", EDGE_MONITOR)
  addGeneric("WAIT_FOR_EDGE", WAIT_FOR_EDGE)

  val io = new Bundle {
    // Data input from pin or output register block.
    val A = in Bool()
    // '0' on LOADN will reset to default delay setting.
    val LOAD_N = in Bool() default(True)
    // 'Pulse' on MOVE will change delay setting. DIRECTION will be sampled at falling edge of MOVE.
    val MOVE = in Bool()
    // '1' to decrease delay and '0' to increase delay.
    val DIRECTION = in Bool()
    // Dynamic coarse delay control (2 bits).
    // 00: no coarse delay
    // 01: 800ps delay
    // 10: 1600ps delay
    // 11: invalid
    val COARSE0, COARSE1 = in Bool() default (False)
    // Delayed data to input register block or to pin.
    val Z = out Bool()
    // Error detected when using the edge monitor logic.
    val EDETERR = out Bool()
    // Flag indicating the delay counter has reached the max (when moving up) or min (when moving down) value.
    val CFLAG = out Bool()
    // 0 - Select delay rank 0 ; 1 - Select delay rank 1.
    val RANKSELECT = in Bool() default(False)
    // 0 - Select bypassed actual path delay code ; 1 - Select registered actual path delay code.
    val RANKENABLE = in Bool() default(False)
    // 0 - Rank-0 registers not enabled ; 1 - Enable Rank-0 registers.
    val RANK0UPDATE = in Bool() default(False)
    // 0 - Rank-1 registers not enabled ; 1 - Enable Rank-1 registers.
    val RANK1UPDATE = in Bool() default(False)
  }

  // Map the BlackBox IO to the Verilog module ports
  noIoPrefix()
}