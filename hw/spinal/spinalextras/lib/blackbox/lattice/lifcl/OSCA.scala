package spinalextras.lib.blackbox.lattice.lifcl

import spinal.core._
import spinalextras.lib.{FixedFrequencyWithError, misc}
import spinalextras.lib.misc.ClockSpecification

import scala.language.postfixOps

class OSCAConfig(
                       LF_OUTPUT_EN : Boolean = true, // Low frequency clock output enable
                       HF_DIV : Option[Int],
                       HF_SED_SEC_DIV : Option[Int]) {
  def asEnabled(x : Boolean): String = {
    if(x) "ENABLED" else "DISABLED"
  }
  def Parameters : Map[String, Any] = {
    Map(
      "LF_OUTPUT_EN" -> asEnabled(LF_OUTPUT_EN),
      "HF_OSC_EN"-> asEnabled(HF_DIV.nonEmpty || HF_SED_SEC_DIV.nonEmpty),
      "HF_CLK_DIV" -> HF_DIV.getOrElse(1).toString,
      "HF_SED_SEC_DIV" -> HF_SED_SEC_DIV.getOrElse(1).toString
    )
  }

  val hf_frequency = HF_DIV.map(d => FixedFrequencyWithError((450 MHz) / (d + 1.0), 0.10))
  val hf_sed_frequency = HF_SED_SEC_DIV.map(d => FixedFrequencyWithError((450 MHz) / (d + 1.0), 0.10))
  val lf_frequency = if (LF_OUTPUT_EN) Some(FixedFrequencyWithError(32 kHz, .10)) else None
}

object OSCAConfig {
  def find_div(x : ClockSpecification) : Option[Int] = {
    if(x.freq == (0 MHz)) {
      None
    } else {
//      if(x.tolerance < .10) {
//        throw new IllegalArgumentException("The HF clocks are +- 10% by default")
//      }
      if(x.freq < (1.75 MHz) || x.freq > (225 MHz)) {
        throw new IllegalArgumentException("The HF clocks must be between 1.75 and 225")
      }
      if(x.phaseOffset != 0) {
        throw new IllegalArgumentException("The HF clocks can not have a phase offset")
      }

      val div = ((450 MHz) / x.freq).toDouble.round

      println(s"Requested frequency ${x.freq.decomposeString}, given frequency ${((450 MHz) / div).decomposeString}")

      Some(div.toInt - 1)
    }
  }
  def create(hf: ClockSpecification = misc.ClockSpecification(0 MHz), hf_sed : ClockSpecification = misc.ClockSpecification(0 MHz)): OSCAConfig = {
    new OSCAConfig(HF_DIV = find_div(hf), HF_SED_SEC_DIV = find_div(hf_sed))
  }
}

class OSCA(cfg: OSCAConfig) extends BlackBox {
  val io = new Bundle {
    // 450MHz with programmable divider (2~256) to user
    val HFCLKOUT = out (Bool())
    // HF clock (225MHz) output enable: just for test
    val HFOUTEN = in(Bool()) default(True)
    // HF user clock output enable
    val HFSDSCEN = in(Bool()) default(True)
    // Low frequency clock output after div4: 32KHz
    val LFCLKOUT = out (Bool())
    // 450MHz going to Config block. Config Block would generate the clk_config, as well as the other blocks it needs, such as SPI or SED clocks
    val HFCLKCFG = out (Bool())
    // 450MHz with programmable divider (2~256) to CFG
    val HFSDCOUT = out (Bool())
  }

  for ((name, value) <- cfg.Parameters.toList.sortBy(_._1)) {
    addGeneric(name, value)
  }

  noIoPrefix()

  def hf_clk() = cfg.hf_frequency.map(f => ClockDomain(io.HFCLKOUT, reset = ClockDomain.current.readResetWire, frequency = f))
  def hf_sed_clk() = cfg.hf_sed_frequency.map(f => ClockDomain(io.HFSDCOUT, reset = ClockDomain.current.readResetWire, frequency = f))
  def lf_clk() = cfg.lf_frequency.map(f => ClockDomain(io.LFCLKOUT, reset = ClockDomain.current.readResetWire, frequency = f))
}
