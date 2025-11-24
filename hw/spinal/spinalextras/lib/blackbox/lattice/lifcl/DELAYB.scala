package spinalextras.lib.blackbox.lattice.lifcl

import spinal.core._

import scala.language.postfixOps

case class DELAYBConfig(
                         // Sets the delay mode to be used. Options: SCLK_ZEROHOLD, ECLK_ALIGNED, ECLK_CENTERED, SCLK_ALIGNED, SCLK_CENTERED, DQS_CMD_CLK, DQS_ALIGNED_X2, DQS_ALIGNED_X4, DQS_CENTERED_X2, DQS_CENTERED_X4
                         DEL_MODE: String = "USER_DEFINED",
                         // Sets delay value when DEL_MODE is set to USER_DEFINED.
                         DEL_VALUE: String = "0",
                         // Select MC1 or CIB coarse delay code control: 0-Selects from MC1(STATIC), 1-Selects from CIB(DYNAMIC)
                         COARSE_DELAY_MODE: String = "STATIC",
                         // Coarse delay setting for lol delay cell.
                         COARSE_DELAY: String = "0NS",
                       ){}

case class DELAYB(cfg : DELAYBConfig = DELAYBConfig()) extends BlackBox {

  addGeneric("DEL_MODE", cfg.DEL_MODE)
  addGeneric("DEL_VALUE", cfg.DEL_VALUE)
  addGeneric("COARSE_DELAY", cfg.COARSE_DELAY)

  val io = new Bundle {
    val A = in Bool()
    val Z = out Bool()
  }
  noIoPrefix()
}

object DELAYB {
  def apply(A: Bool, cfg : DELAYBConfig): Bool = {
    val buf = DELAYB(cfg)
    buf.io.A := A
    CombInit(buf.io.Z)
  }
  def applyBool(A: Bool, cfg : DELAYBConfig, stages : Int): Bool = {
    var z = A;
    for(i <- 0 until stages) {
      z = DELAYB(z, cfg)
    }
    z
  }

  def apply[T <: Data](A: T, cfg : DELAYBConfig, stages : Int): T = {
    val z = cloneOf(A)

    val b = Bits(A.getBitsWidth bits)
    for(i <- 0 until A.getBitsWidth) {
      val in = A.asBits(i)
      b(i) := applyBool(in, cfg, stages)
    }
    z.assignFromBits(b)

    z
  }

  def apply[T <: Data](A: T, cfg : DELAYBConfig): T = {
    DELAYB(A, cfg, 1)
  }

}