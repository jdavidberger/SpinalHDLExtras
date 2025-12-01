package spinalextras.lib.blackbox.lattice.lifcl

import spinal.core._
import spinalextras.lib.{Constraints, FixedFrequencyWithError, misc}
import spinalextras.lib.misc.ClockSpecification

import scala.language.postfixOps
import scala.collection.Seq

class OSCDConfig(
                       DTR_EN : Boolean = true, // DTR block enable from MIB
                       HF_FABRIC_EN : Boolean = false, // High frequency oscillator trim source mux select (for TEST only)
                       LF_FABRIC_EN : Boolean = false,
                       LF_OUTPUT_EN : Boolean = true, // Low frequency clock output enable
                       DEBUG_N : Boolean = false, // Ignore/enable the SLEEP/STOP function during the USER mode. 0: Ignore the SLEEP/STOP mode
                       HF_DIV : Option[Int],
                       HF_SED_SEC_DIV : Option[Int]) extends OSCAConfig(LF_OUTPUT_EN, HF_DIV, HF_SED_SEC_DIV) {
  override def Parameters : Map[String, Any] = {
    super.Parameters ++
    Map(
      "DTR_EN" -> asEnabled(DTR_EN),
      "HF_FABRIC_EN" -> asEnabled(HF_FABRIC_EN),
      "LF_FABRIC_EN" -> asEnabled(LF_FABRIC_EN),
      "DEBUG_N" -> asEnabled(DEBUG_N),
    )
  }
}

object OSCDConfig {
  def create(hf: ClockSpecification = misc.ClockSpecification(0 MHz), hf_sed : ClockSpecification = misc.ClockSpecification(0 MHz)): OSCDConfig = {
    new OSCDConfig(HF_DIV = OSCAConfig.find_div(hf), HF_SED_SEC_DIV = OSCAConfig.find_div(hf_sed))
  }
}

class OSCD(cfg: OSCDConfig) extends BlackBox {
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
    // CIB control to turn on/off HF-OSC, function during user mode with mc1_en_hf_osc_cib high
    val HFOUTCIBEN = in(Bool()) default(False)
    // CIB control to enable/disable hf_clk_config output, function during user mode with mc1_en_hf_config_on_cib high
    val REBOOT = in(Bool()) default(False)
  }

  for ((name, value) <- cfg.Parameters.toList.sortBy(_._1)) {
    addGeneric(name, value)
  }

  noIoPrefix()

  def hf_clk() = cfg.hf_frequency.map(f => ClockDomain(io.HFCLKOUT, reset = ClockDomain.current.isResetActive, frequency = f))
  def hf_sed_clk() = cfg.hf_sed_frequency.map(f => ClockDomain(io.HFSDCOUT, reset = ClockDomain.current.isResetActive, frequency = f))
  def lf_clk() = cfg.lf_frequency.map(f => ClockDomain(io.LFCLKOUT, reset = ClockDomain.current.isResetActive, frequency = f))
  def clocks = Seq(hf_clk(), hf_sed_clk(), lf_clk()).flatten

  addPrePopTask( () => {
    clocks.foreach(cfg_clk => {
      Constraints.create_clock(cfg_clk.readClockWire, cfg_clk.frequency.getValue)
    })
    Constraints.add_clock_group(true, clocks.map(_.readClockWire):_*)
  })
}

object OSCD {
  def apply(cfg: OSCDConfig): OSCD = {
    new ClockingArea(new ClockDomain(False)) {
      val oscd = new OSCD(cfg)
    }
  }.oscd
}
