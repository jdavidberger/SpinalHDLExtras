package spinalextras.lib.blackbox.lattice.lifcl

import spinal.core._
import spinal.lib._
import spinalextras.lib
import spinalextras.lib.{Config, FixedFrequencyWithError, FixedRangeFrequency}
import spinalextras.lib.blackbox.lattice.lifcl.PLLConfig.to_bin_string
import spinalextras.lib.misc._
import spinalextras.lib.tests.TestClockGen
import spinalextras.lib.Constraints

import java.io.FileReader

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import scala.util.control.Breaks.{break, breakable}

case class PLLClockConfig(
                         VCO : HertzNumber = 0 MHz,
                         CLKI_DIV : Int = 0,
                         CLKFB_DIV : Int = 0,
                         CLKFB_DIV_FRAC : Int = 0,
                         CLK_REF: Int = 0,
                         OUTPUTS: Seq[PLLOutputClockConfig] = Seq.empty,
                         CLKFB_FRAC_MODE : Boolean = false,
                         SPREAD_SPECTRUM : Boolean = false
                         ) {

  override def equals(o: Any) = o match {
    case other : PLLClockConfig => {
      VCO == other.VCO &&
        CLKI_DIV == other.CLKI_DIV &&
        CLKFB_DIV == other.CLKFB_DIV &&
        CLKFB_DIV_FRAC == other.CLKFB_DIV_FRAC &&
        CLK_REF == other.CLK_REF &&
        CLKFB_FRAC_MODE == other.CLKFB_FRAC_MODE &&
        SPREAD_SPECTRUM == other.SPREAD_SPECTRUM &&
        OUTPUTS == other.OUTPUTS
    }
    case _ => false
  }

  def print(actualOutputClocks : Seq[ClockSpecification]): Unit = {
//
//    if(actualOutputClocks.size != outputClocks.size && clkn != actualOutputClocks.size - 1) {
//      configs = configs.dropRight(1)
//      actualOutputClocks = actualOutputClocks.dropRight(1)
//    }

    println("Best clock settings")
    println(s"ref_clk ${CLK_REF} fb_div ${CLKFB_DIV} ${CLKFB_DIV_FRAC}/4096 idiv ${CLKI_DIV} VCO ${VCO} frac mode: ${CLKFB_FRAC_MODE}")
    OUTPUTS.zip(actualOutputClocks).foreach {
      case (cfg, clkSpec) =>
            println(s"Target freq ${clkSpec.freq.decomposeString} ${clkSpec.phaseOffset}deg  actual ${cfg.ACTUAL_FREQ} ${cfg.ACTUAL_PHASE}deg ${cfg}")
    }
  }
}

object PLLOutputClockConfig {
  def apply(TRIM: Int,
            DEL : Int,
            DIV : Int,
            PHI : Int,
            ENABLE : Boolean,
            TRIM_BYPASSED: Boolean,
            ACTUAL_FREQ : HertzNumber,
            ACTUAL_PHASE : Double) : PLLOutputClockConfig = PLLOutputClockConfig(TRIM, DEL, DIV, PHI, ENABLE, TRIM_BYPASSED, ACTUAL_FREQ, ACTUAL_PHASE)
}
case class PLLOutputClockConfig(
                                 TRIM: Int = 0,
                                 DEL : Int = 0,
                                 DIV : Int = 1,
                                 PHI : Int = 0,
                                 ENABLE : Boolean = true,
                                 TRIM_BYPASSED: Boolean = false,
                                 //ACTUAL_FREQ : HertzNumber = 0 MHz,
                                 ACTUAL_FREQ : Double = 0, // In hertz
                                 ACTUAL_PHASE : Double = 0
                               ) {
  override def equals(o: Any) = {
    o match {
      case other : PLLOutputClockConfig => {
        TRIM == other.TRIM &&
          DEL == other.DEL &&
          DIV == other.DIV &&
          PHI == other.PHI &&
          ENABLE == other.ENABLE &&
          TRIM_BYPASSED == other.TRIM_BYPASSED
      }
      case _ => false
    }
  }
}

case class PLLConfig(
                      /** \desc = "Input control signal to tune the bias current of ppath cp, when the bit increase, the bias current increase. This current branch is combined with the Ivco_fb current.", \otherValues = "{0000}", \infer = "No" */
                      BW_CTL_BIAS: String = "0b0101",

                      /** \desc = "CLKOP output trim control bits", \otherValues = "{}", \infer = "No" */
                      CLKOP_TRIM: String = "0b0000",

                      /** \desc = "CLKOS output trim control bits", \otherValues = "{}", \infer = "No" */
                      CLKOS_TRIM: String = "0b0000",

                      /** \desc = "CLKOS2 output trim control bits", \otherValues = "{}", \infer = "No" */
                      CLKOS2_TRIM: String = "0b0000",

                      /** \desc = "CLKOS3 output trim control bits", \otherValues = "{}", \infer = "No" */
                      CLKOS3_TRIM: String = "0b0000",

                      /** \desc = "CLKOS4 output trim control bits", \otherValues = "{}", \infer = "No" */
                      CLKOS4_TRIM: String = "0b0000",

                      /** \desc = "CLKOS5 output trim control bits", \otherValues = "{}", \infer = "No" */
                      CLKOS5_TRIM: String = "0b0000",

                      /** \desc = "LPF cap control", \otherValues = "{1P,3P,7P,9P,11P,13P,15P}", \infer = "No" */
                      CRIPPLE: String = "5P",

                      /** \desc = "LPF cap control", \otherValues = "{8P,12P,16P,20P,24P,28P,32P,36P,44P,48P,52P,56P,60P,64P,68P}", \infer = "No" */
                      CSET: String = "40P",

                      /** \desc = "Control signal to adjust the delay of the PFD; default 1b0 = 200ps; 1b1 = 300ps", \otherValues = "{300PS}", \infer = "No" */
                      DELAY_CTRL: String = "200PS",

                      /** \desc = "Output divider phase shift (work with lmmi_diva)", \otherValues = "{}", \infer = "No" */
                      DELA: Int = 0,

                      /** \desc = "Output divider phase shift (work with lmmi_divb)", \otherValues = "{}", \infer = "No" */
                      DELB: Int = 0,

                      /** \desc = "Output divider phase shift (work with lmmi_divc)", \otherValues = "{}", \infer = "No" */
                      DELC: Int = 0,

                      /** \desc = "Output divider phase shift (work with lmmi_divd)", \otherValues = "{}", \infer = "No" */
                      DELD: Int = 0,

                      /** \desc = "Output divider phase shift (work with lmmi_dive)", \otherValues = "{}", \infer = "No" */
                      DELE: Int = 0,

                      /** \desc = "Output divider phase shift (work with lmmi_divf)", \otherValues = "{}", \infer = "No" */
                      DELF: Int = 0,

                      /** \desc = "lmmi equivalent of CIB direction signal", \otherValues = "{ENABLED}", \infer = "No" */
                      DIRECTION: Boolean = false,

                      /** \desc = "Output dividers for clkop", \otherValues = "{}", \infer = "No" */
                      DIVA: Int = 0,

                      /** \desc = "Output dividers for clkos", \otherValues = "{}", \infer = "No" */
                      DIVB: Int = 0,

                      /** \desc = "Output dividers for clkos2", \otherValues = "{}", \infer = "No" */
                      DIVC: Int = 0,

                      /** \desc = "Output dividers for clkos3", \otherValues = "{}", \infer = "No" */
                      DIVD: Int = 0,

                      /** \desc = "Output dividers for clkos4", \otherValues = "{}", \infer = "No" */
                      DIVE: Int = 0,

                      /** \desc = "Output dividers for clkos5", \otherValues = "{}", \infer = "No" */
                      DIVF: Int = 0,

                      /** \desc = "Select dynamic phase selection", \otherValues = "{}", \infer = "No" */
                      DYN_SEL: String = "0b000",

                      /** \desc = "1'b1 select CIB signals for dynamic phase shift", \otherValues = "{DYNAMIC}", \infer = "No" */
                      DYN_SOURCE: String = "STATIC",

                      /** \desc = "Bit 0. Active HIGH: Clock output enable", \otherValues = "{ENABLED}", \infer = "No" */
                      ENCLK_CLKOP: Boolean = false,

                      /** \desc = "Bit 1. Active HIGH: Clock output enable", \otherValues = "{ENABLED}", \infer = "No" */
                      ENCLK_CLKOS: Boolean = false,

                      /** \desc = "Bit 2. Active HIGH: Clock output enable", \otherValues = "{ENABLED}", \infer = "No" */
                      ENCLK_CLKOS2: Boolean = false,

                      /** \desc = "Bit 3. Active HIGH: Clock output enable", \otherValues = "{ENABLED}", \infer = "No" */
                      ENCLK_CLKOS3: Boolean = false,

                      /** \desc = "Bit 4. Active HIGH: Clock output enable", \otherValues = "{ENABLED}", \infer = "No" */
                      ENCLK_CLKOS4: Boolean = false,

                      /** \desc = "Bit 5. Active HIGH: Clock output enable", \otherValues = "{ENABLED}", \infer = "No" */
                      ENCLK_CLKOS5: Boolean = false,

                      /** \desc = "1'b1 enable output(s) sync with clkop", \otherValues = "{ENABLED}", \infer = "No" */
                      ENABLE_SYNC: Boolean = false,

                      /** \desc = "Enable signal for fast lock", \otherValues = "{DISABLED}", \infer = "No" */
                      FAST_LOCK_EN: Boolean = true,

                      /** \desc = "1V supply enable or disable", \otherValues = "{ENABLED}", \infer = "No" */
                      V2I_1V_EN: Boolean = false,

                      /** \desc = "Bleeding current for PI to adjust the linearity", \otherValues = "{}", \infer = "No" */
                      FBK_CUR_BLE: String = "0b00000000",

                      /** \desc = "Select the positive or negative phase of PI output. 0: positive phase; 1: negative phase", \otherValues = "{NEGATIVE}", \infer = "No" */
                      FBK_EDGE_SEL: String = "POSITIVE",

                      /** \desc = "Interface timing control for feedback divider.", \otherValues = "{}", \infer = "No" */
                      FBK_IF_TIMING_CTL: String = "0b00",

                      /** \desc = "Enable the integer mode for feedback divider", \otherValues = "{ENABLED}", \infer = "No" */
                      FBK_INTEGER_MODE: Boolean = false,

                      /** \desc = "Minimum divider ratio control word for feedback divider. For example if n_pll[7:0] or mmd_dig[7:0] is less than lmmi_fbk_mask[7:0], then the MMD divider ratio is determined by lmmi_fbk_mask[7:0]. Otherwise the divider ratio will be determined by n_pll[7:0] or mmd_dig[7:0]", \otherValues = "{00000000}", \infer = "No" */
                      FBK_MASK: String = "0b00001000",

                      /** \desc = "MMD divider ratio setting in integer mode", \otherValues = "{0}", \infer = "No" */
                      FBK_MMD_DIG: Int = 8,

                      /** \desc = "Pulse width control for MMD output clock. If lmmi_fbk_mmd_puls_ctl[3:0]=4'b0110, it means that there's 6 VCO cycles in the MMD output clock.", \otherValues = "{}", \infer = "No" */
                      FBK_MMD_PULS_CTL: String = "0b0000",

                      /** \desc = "Reserved floating control bits", \otherValues = "{}", \infer = "No" */
                      FBK_MODE: String = "0b00",

                      /** \desc = "PI bypass control bit. 0; PI not bypass, 1; PI bypass; it should be connected to lmmi_ssc_pi_bypass", \otherValues = "{BYPASSED}", \infer = "No" */
                      FBK_PI_BYPASS: String = "NOT_BYPASSED",

                      /** \desc = "RC time constant control in PI", \otherValues = "{0000}", \infer = "No" */
                      FBK_PI_RC: String = "0b1100",

                      /** \desc = "Current control for PI to adjust the linearity", \otherValues = "{}", \infer = "No" */
                      FBK_PR_CC: String = "0b0000",

                      /** \desc = "Bias current control for PI", \otherValues = "{0000}", \infer = "No" */
                      FBK_PR_IC: String = "0b1000",

                      /** \desc = "Active HIGH to tri-state the ICP output.", \otherValues = "{ENABLED}", \infer = "No" */
                      FLOAT_CP: Boolean = false,

                      /** \desc = "2 bits control the fast lock period, 00 is 1x, 01 is 2x, 10 is 4x and 11 is 8x", \otherValues = "{1X,4X,8X}", \infer = "No" */
                      FLOCK_CTRL: String = "2X",

                      /** \desc = "1b1 to enable fast lock; after char, default to enabled", \otherValues = "{DISABLED}", \infer = "No" */
                      FLOCK_EN: Boolean = true,

                      /** \desc = "Fast lock source selection; 0 is ref clock; 1 is feedback clock", \otherValues = "{FBCLK}", \infer = "No" */
                      FLOCK_SRC_SEL: String = "REFCLK",

                      /** \desc = "force internal vctrl=analog pad", \otherValues = "{ENABLED}", \infer = "No" */
                      FORCE_FILTER: Boolean = false,

                      /** \desc = "Current tuning", \otherValues = "{8P3UA,14P9UA,12P4UA,19P8UA,17P3UA,24P8UA,22P3UA}", \infer = "No" */
                      I_CTRL: String = "10UA",

                      /** \desc = "i-path CP compensate up/dn mismatch at process variation", \otherValues = "{0000}", \infer = "No" */
                      IPI_CMP: String = "0b1000",

                      /** \desc = "Input control bits to compensate the i-path bias current", \otherValues = "{0000}", \infer = "No" */
                      IPI_CMPN: String = "0b0011",

                      /** \desc = "Enable ipi_cmp", \otherValues = "{ENABLED}", \infer = "No" */
                      IPI_COMP_EN: Boolean = false,

                      /** \desc = "Input control signal to tune the bias current of ppath cp", \otherValues = "{0000}", \infer = "No" */
                      IPP_CTRL: String = "0b1000",

                      /** \desc = "Input control signal to select which ppath cp is on, there are 4 branches at max", \otherValues = "{0000}", \infer = "No" */
                      IPP_SEL: String = "0b1111",

                      /** \desc = "ICO gain controls", \otherValues = "{00000}", \infer = "No" */
                      KP_VCO: String = "0b11001",

                      /** \desc = "Active HIGH to have INT_LOCK_STICKY. Default to be 1b1. 1b0 is for PDE/DE purpose", \otherValues = "{ENABLED}", \infer = "No" */
                      LDT_INT_LOCK_STICKY: Boolean = false,

                      /** \desc = "Frequency lock-detector resolution sensitivity", \otherValues = "{98304CYC,24576CYC,6144CYC}", \infer = "No" */
                      LDT_LOCK: String = "1536CYC",

                      /** \desc = "Lock-detector type select", \otherValues = "{UFREQ,SPHASE,SFREQ,UFREQ_SPHASE,U_PHASE,S_FREQ,U_FREQ_S_PHASE}", \infer = "No" */
                      LDT_LOCK_SEL: String = "U_FREQ",

                      /** \desc = "Active HIGH; enable Legacy mode", \otherValues = "{ENABLED}", \infer = "No" */
                      LEGACY_ATT: Boolean = false,

                      /** \desc = "For load control of divider phase control", \otherValues = "{ENABLED}", \infer = "No" */
                      LOAD_REG: Boolean = false,

                      /** \desc = "Open loop mode enable for mfg testing", \otherValues = "{ENABLED}", \infer = "No" */
                      OPENLOOP_EN: Boolean = false,

                      /** \desc = "Select VCO phase-shift (0..7) for A section", \otherValues = "{}", \infer = "No" */
                      PHIA: Int = 0,

                      /** \desc = "Select VCO phase-shift (0..7) for B section", \otherValues = "{}", \infer = "No" */
                      PHIB: Int = 0,

                      /** \desc = "Select VCO phase-shift (0..7) for C section", \otherValues = "{}", \infer = "No" */
                      PHIC: Int = 0,

                      /** \desc = "Select VCO phase-shift (0..7) for D section", \otherValues = "{}", \infer = "No" */
                      PHID: Int = 0,

                      /** \desc = "Select VCO phase-shift (0..7) for E section", \otherValues = "{}", \infer = "No" */
                      PHIE: Int = 0,

                      /** \desc = "Select VCO phase-shift (0..7) for F section", \otherValues = "{}", \infer = "No" */
                      PHIF: Int = 0,

                      /** \desc = "", \otherValues = "{ENABLED}", \infer = "No" */
                      PLLPDN_EN: Boolean = false,

                      /** \desc = "Active LOW PLL power-down; PLL is NOT USED", \otherValues = "{USED}", \infer = "No" */
                      PLLPD_N: String = "UNUSED",

                      /** \desc = "Active HIGH; Enable PLLRESET CIB Signal", \otherValues = "{ENABLED}", \infer = "No" */
                      PLLRESET_ENA: Boolean = false,

                      /** \desc = "Integer mode control bit for reference clock pre-divider. Default to Integer, 1b1.", \otherValues = "{ENABLED}", \infer = "No" */
                      REF_INTEGER_MODE: Boolean = false,

                      /** \desc = "Minimum divider ratio control word for reference pre-divider", \otherValues = "{}", \infer = "No" */
                      REF_MASK: String = "0b00000000",

                      /** \desc = "MMD divider ratio setting for reference pre-divider when lmmi_ref_integer_mode=1", \otherValues = "{0}", \infer = "No" */
                      REF_MMD_DIG: Int = 8,

                      /** \desc = "MMD divider ratio setting for reference pre-divider when lmmi_ref_integer_mode=0", \otherValues = "{00000000}", \infer = "No" */
                      REF_MMD_IN: String = "0b00001000",

                      /** \desc = "Pulse width control for MMD output clock in reference pre-divider", \otherValues = "{}", \infer = "No" */
                      REF_MMD_PULS_CTL: String = "0b0000",

                      /** \desc = "Interface timing control for reference divider. Default to be 2b00.", \otherValues = "{}", \infer = "No" */
                      REF_TIMING_CTL: String = "0b00",

                      /** \desc = "Lmmi_refin_reset = 1b1 and with switching of REFin_SEL (either L-H or H-L) will generate a PLL reset", \otherValues = "{RESET}", \infer = "No" */
                      REFIN_RESET: String = "SET",

                      /** \desc = "LPF reset enable", \otherValues = "{ENABLED}", \infer = "No" */
                      RESET_LF: Boolean = false,

                      /** \desc = "For VCO phase rotation", \otherValues = "{ENABLED}", \infer = "No" */
                      ROTATE: Boolean = false,

                      /** \desc = "Select output to CLKOP", \otherValues = "{ENABLED}", \infer = "No" */
                      SEL_OUTA: Boolean = false,

                      /** \desc = "Select output to CLKOS", \otherValues = "{ENABLED}", \infer = "No" */
                      SEL_OUTB: Boolean = false,

                      /** \desc = "Select output to CLKOS2", \otherValues = "{ENABLED}", \infer = "No" */
                      SEL_OUTC: Boolean = false,

                      /** \desc = "Select output to CLKOS3", \otherValues = "{ENABLED}", \infer = "No" */
                      SEL_OUTD: Boolean = false,

                      /** \desc = "Select output to CLKOS4", \otherValues = "{ENABLED}", \infer = "No" */
                      SEL_OUTE: Boolean = false,

                      /** \desc = "Select output to CLKOS5", \otherValues = "{ENABLED}", \infer = "No" */
                      SEL_OUTF: Boolean = false,

                      /** \desc = "Active HIGH: Enable STOP PMU signal", \otherValues = "{ENABLED}", \infer = "No" */
                      SLEEP: Boolean = false,

                      /** \desc = "Dither enable or disable for SDM", \otherValues = "{ENABLED}", \infer = "No" */
                      SSC_DITHER: Boolean = false,

                      /** \desc = "Down triangle or center triangle control bit in SSC profile generator.", \otherValues = "{CENTER_TRIANGLE}", \infer = "No" */
                      SSC_EN_CENTER_IN: String = "DOWN_TRIANGLE",

                      /** \desc = "Enable or disable SDM", \otherValues = "{ENABLED}", \infer = "No" */
                      SSC_EN_SDM: Boolean = false,

                      /** \desc = "Enable or disabled SSC profile generator", \otherValues = "{ENABLED}", \infer = "No" */
                      SSC_EN_SSC: Boolean = false,

                      /** \desc = "Fractional part of the feedback divider ratio", \otherValues = "{}", \infer = "No" */
                      SSC_F_CODE: String = "0b000000000000000",

                      /** \desc = "Integer part of the feedback divider ratio", \otherValues = "{000000000}", \infer = "No" */
                      SSC_N_CODE: String = "0b000010100",

                      /** \desc = "SDM order control bit; 0 - SDM order = 1; 1 - SDM order = 2", \otherValues = "{SDM_ORDER1}", \infer = "No" */
                      SSC_ORDER: String = "SDM_ORDER2",

                      /** \desc = "PI bypass control bit. 0; PI not bypass, 1; PI bypass; it should be connected to lmmi_fbk_pi_bypass", \otherValues = "{BYPASSED}", \infer = "No" */
                      SSC_PI_BYPASS: String = "NOT_BYPASSED",

                      /** \desc = "Weighting control bit for lmmi_ssc_step_in", \otherValues = "{}", \infer = "No" */
                      SSC_REG_WEIGHTING_SEL: String = "0b000",

                      /** \desc = "Two-point FSK modulation control bit", \otherValues = "{ENABLED}", \infer = "No" */
                      SSC_SQUARE_MODE: Boolean = false,

                      /** \desc = "SSC modulation depth control bit", \otherValues = "{}", \infer = "No" */
                      SSC_STEP_IN: String = "0b0000000",

                      /** \desc = "SSC modulation frequency control. The frequency should be 30-33kHz.", \otherValues = "{}", \infer = "No" */
                      SSC_TBASE: String = "0b000000000000",

                      /** \desc = "Enable STDBY CIB signal", \otherValues = "{ENABLED}", \infer = "No" */
                      STDBY_ATT: Boolean = false,

                      /** \desc = "1b0: bypass CLKOP output trim", \otherValues = "{USED}", \infer = "No" */
                      TRIMOP_BYPASS_N: String = "BYPASSED",

                      /** \desc = "1b0: bypass CLKOS output trim", \otherValues = "{USED}", \infer = "No" */
                      TRIMOS_BYPASS_N: String = "BYPASSED",

                      /** \desc = "1b0: bypass CLKOS2 output trim", \otherValues = "{USED}", \infer = "No" */
                      TRIMOS2_BYPASS_N: String = "BYPASSED",

                      /** \desc = "1b0: bypass CLKOS3 output trim", \otherValues = "{USED}", \infer = "No" */
                      TRIMOS3_BYPASS_N: String = "BYPASSED",

                      /** \desc = "1b0: bypass CLKOS4 output trim", \otherValues = "{USED}", \infer = "No" */
                      TRIMOS4_BYPASS_N: String = "BYPASSED",

                      /** \desc = "1b0: bypass CLKOS5 output trim", \otherValues = "{USED}", \infer = "No" */
                      TRIMOS5_BYPASS_N: String = "BYPASSED",

                      /** \desc = "Mlk choose such as frequency range of ICO from 800-1600MHz over PVT. Mlk larger (more current withdraw from ICO) ICO run slower.", \otherValues = "{40,45,50,55,60,65,70,75,80,90,95,100,105,110,115}", \infer = "No" */
                      V2I_KVCO_SEL: String = "85",

                      /** \desc = "P-path v2i gm control", \otherValues = "{00000}", \infer = "No" */
                      V2I_PP_ICTRL: String = "0b00110",

                      /** \desc = "P-path high frequency pole resistor control", \otherValues = "{11P3K,11K,10P7K,10P3K,9P7K,9P3K,9K}", \infer = "No" */
                      V2I_PP_RES: String = "10K",

                      /** \desc = "Fuses used to determine the feedback selection in the wake up stage, center mux about the clock tree feedback", \otherValues = "{CMUX_CLKOS,CMUX_CLKOS2,CMUX_CLKOS3,CMUX_CLKOS4,CMUX_CLKOS5}", \infer = "No" */
                      CLKMUX_FB: String = "CMUX_CLKOP",

                      /** \desc = "Select feedback clock", \otherValues = "{DIVB,DIVC,DIVD,DIVE,DIVF,RESERVED,FBKCLK0,FBKCLK1,FBKCLK2,FBKCLK3,FBKCLK4,FBKCLK5,FBKCLK6,FBKCLK7,FBKCLK8}", \infer = "No" */
                      SEL_FBK: String = "DIVA",

                      /** \desc = "The internal path delay selection path", \otherValues = "{0000000}", \infer = "No" */
                      DIV_DEL: String = "0b0000001",

                      /** \desc = "The internal phase delay selection path", \otherValues = "{}", \infer = "No" */
                      PHASE_SEL_DEL: String = "0b000",

                      /** \desc = "The internal phase delay selection path1", \otherValues = "{}", \infer = "No" */
                      PHASE_SEL_DEL_P1: String = "0b000",

                      /** \desc = "External divider value for feedback clock", \otherValues = "{}", \infer = "No" */
                      EXTERNAL_DIVIDE_FACTOR: Int = 0,

                      /** \desc = "Associated with the attribute INTFBKDEL_SEL (1'b1 to enable the internal path switching during POR/Sleep/Stdandby modes)", \otherValues = "{ENABLED}", \infer = "No" */
                      INTFBKDEL_SEL: Boolean = false,

                      /** \desc = "Sync with pmu to wait for PLL lock", \otherValues = "{DISABLED}", \infer = "No" */
                      PMU_WAITFORLOCK: Boolean = true,

                      /** \desc = "Select monitoring clock frequency (3.2MHz or 1MHz)", \otherValues = "{1P0}", \infer = "No" */
                      REF_OSC_CTRL: String = "3P2",
                      OUTPUT_CLKS : Seq[PLLOutputClockConfig] = Seq.empty[PLLOutputClockConfig]
                    ) {
  require(OUTPUT_CLKS.size <= 6)

  def Parameters : Map[String, Any] = {
    val rtn = new mutable.HashMap[String, Any]()
    for (f <- this.getClass.getDeclaredFields) {
      f.setAccessible(true)
      val v = f.get(this)
      v match {
        case (b : java.lang.Boolean) => rtn(f.getName) = if (b) "ENABLED" else "DISABLED"
        case (i: java.lang.Integer) => rtn(f.getName) = s"${i}"
        case _ : Seq[PLLOutputClockConfig] =>
        case _ => rtn(f.getName) = v.toString
      }
    }
    val n_to_l = Seq("P", "S", "S2", "S3", "S4", "S5")
    for((c, idx) <- OUTPUT_CLKS.zipWithIndex) {
      val id = ('A' + idx).toChar
      val name = "O" + n_to_l(idx)
      rtn += (
        s"DIV${id}" -> c.DIV.toString,
        s"DEL${id}" -> c.DEL.toString,
        s"PHI${id}" -> c.PHI.toString,
        s"ENCLK_CLK${name}" -> (if (c.ENABLE) "ENABLED" else "DISABLED"),
        s"CLK${name}_TRIM" -> to_bin_string(c.TRIM, 4),
        s"TRIM${name}_BYPASS_N" -> (if(!c.TRIM_BYPASSED) "BYPASSED" else "USED")
      )
    }
    rtn.toMap
  }
}

class PLL(val cfg: PLLConfig) extends BlackBox {

  val io = new Bundle {
    // \desc = "", \pintype = "INTFBK"
    val INTFBKOP = out Bool()
    // \desc = "", \pintype = "INTFBK"
    val INTFBKOS = out Bool()
    // \desc = "", \pintype = "INTFBK"
    val INTFBKOS2 = out Bool()
    // \desc = "", \pintype = "INTFBK"
    val INTFBKOS3 = out Bool()
    // \desc = "", \pintype = "INTFBK"

    // \desc = "", \pintype = "INTFBK"
    val INTFBKOS4 = out Bool()
    // \desc = "", \pintype = "INTFBK"
    val INTFBKOS5 = out Bool()
    // \desc = "Valid if MC1_DYN_SOURCE = 1.  Specify direction that CIB_ROTATE changes VCO phase.  0 - phase rotates to later  phase.  1 - Phase rotates to earlier phase", \pintype = "CONTROL"
    val DIR = in Bool() default (False)
    // \desc = "Valid if MC1_DYN_SOURCE = 1. CIB_DYN_SEL must be stable before initiating a rotation with CIB_LOAD_REG", \pintype = "CONTROL"
    val DIRSEL = in Bits (3 bits) default (0)
    // \desc = "Valid if MC1_DYN_SOURCE = 1.  Initiate a divider output phase shift on negative edge of CIB_LOAD_REG", \pintype = "CONTROL"
    val LOADREG = in Bool() default (False)
    // Valid if MC1_DYN_SOURCE = 1.  Initiate a change from current VCO clock phase to an earlier or later phase on the negative edge of CIB_ROTATE
    val DYNROTATE = in Bool() default (False)
    // LMMI Clock from Fabric
    val LMMICLK = in Bool() default (False)
    // LMMI Reset Signal to reset the state machine if the IP gets locked.
    val LMMIRESET_N = in Bool() default (True)
    // LMMI Request signal from Fabric
    val LMMIREQUEST = in Bool() default (False)
    // LMMI Write High / Read Low from Fabric
    val LMMIWRRD_N = in Bool() default (False)
    // LMMI Offset Address from Fabric, not all bits are required for an IP
    val LMMIOFFSET = in Bits (7 bits) default (0)
    // LMMI Write Data from Fabric, not all bits are required for an IP
    // LMMI Write Data from Fabric, not all bits are required for an IP
    val LMMIWDATA = in Bits (8 bits) default (0)
    // LMMI Read Data to Fabric
    val LMMIRDATA = out Bits (8 bits)
    // LMMI Read Date Valid to Fabric
    val LMMIRDATAVALID = out Bool()
    // LMMI Ready signal to Fabric
    val LMMIREADY = out Bool()
    // Active low to power down PLL from CIB port
    val PLLPOWERDOWN_N = in Bool() default (True)
    // Clock reference
    val REFCK = in Bool()
    // Primary (A) output clock

    val CLKOP = out Bool()
    // Secondary (B) output clock
    val CLKOS = out Bool()
    // Secondary (C) output clock
    val CLKOS2 = out Bool()
    // Secondary (D) output clock
    val CLKOS3 = out Bool()
    // Secondary (E) output clock
    val CLKOS4 = out Bool()
    // Secondary (F) output clock
    val CLKOS5 = out Bool()
    // Active HIGH; Enable A output (CLKOP); PLL CIB Input
    val ENCLKOP = in Bool() default (True)
    // Active HIGH; Enable B output (CLKOS); PLL CIB Input
    val ENCLKOS = in Bool() default (True)
    // Active HIGH; Enable C output (CLKOS2); PLL CIB Input
    val ENCLKOS2 = in Bool() default (True)
    // Active HIGH; Enable D output (CLKOS3); PLL CIB Input
    val ENCLKOS3 = in Bool() default (True)
    // Active HIGH; Enable E output (CLKOS4); PLL CIB Input
    val ENCLKOS4 = in Bool() default (True)
    // Active HIGH; Enable F output (CLKOS5); PLL CIB Input
    val ENCLKOS5 = in Bool() default (True)
    val FBKCK = in Bool() default (
      cfg.SEL_FBK match {
        case "DIVA" => INTFBKOP
        case "DIVB" => INTFBKOS
        case "DIVC" => INTFBKOS2
        case "DIVD" => INTFBKOS3
        case "DIVE" => INTFBKOS4
        case "DIVF" => INTFBKOS5
        case "FBKCLK0" => CLKOP
        case "FBKCLK1" => CLKOS
        case "FBKCLK2" => CLKOS2
        case "FBKCLK3" => CLKOS3
        case "FBKCLK4" => CLKOS4
        case "FBKCLK5" => CLKOS5
      }
    )
    // PLL internal lock indicator; PLL CIB Output
    val INTLOCK = out Bool()
    // PLL Legacy mode signal; Active HIGH to enter the mode. Enabled by lmmi_legacy fuse. PLL CIB input
    val LEGACY = in Bool() default (False)
    val LEGRDYN = out Bool()
    // PLL lock indicator; PLL CIB Output
    val LOCK = out Bool()
    // PFD DN output signal to PLL CIB port
    val PFDDN = out Bool()
    // PFD UP output signal to PLL CIB port
    val PFDUP = out Bool()
    // Active HIGH to reset PLL; PLL CIB Input, Enabled by MC1_PLLRESET; PLL CIB input
    val PLLRESET = in Bool() default(False)
    // PLL STANDBY signal; Active HIGH to put PLL clocks in Low (Not used)
    val STDBY = in Bool() default (False)
    // The output of Reference CLK mux output; PLL CIB Output
    val REFMUXCK = out Bool()
    val REGQA = out Bool()
    // \desc = "", \pintype = "DATA"
    val REGQB = out Bool()
    // \desc = "", \pintype = "DATA"
    val REGQB1 = out Bool()
    // \desc = "", \pintype = "DATA"
    val CLKOUTDL = out Bool()
    // \desc = "", \pintype = "CONTROL"
    val ROTDEL = in Bool() default (False)
    // \desc = "", \pintype = "CONTROL"
    val DIRDEL = in Bool() default (False)
    // \desc = "", \pintype = "CONTROL"
    val ROTDELP1 = in Bool() default (False)
    // \desc = "", \pintype = "CONTROL"
    val GRAYTEST = in Bits (5 bits) default (0)
    // \desc = "", \pintype = "CONTROL"
    val BINTEST = in Bits (2 bits) default (0)
    // \desc = "", \pintype = "CONTROL"
    val DIRDELP1 = in Bool() default (False)
    // \desc = "", \pintype = "CONTROL"
    val GRAYACT = in Bits (5 bits) default (0)
    // \desc = "", \pintype = "CONTROL"
    val BINACT = in Bits (2 bits) default (0)

    val CLKS = cfg.OUTPUT_CLKS.zip(Seq(CLKOP, CLKOS, CLKOS2, CLKOS3, CLKOS4, CLKOS5).filter(_ != null))
  }

  val defParams = PLLConfig().Parameters
  for ((name, value) <- cfg.Parameters.toList.sortBy(_._1)) {
    //if(value != defParams(name)) {
      addGeneric(name, value)
    //}
  }

  val min_ref_clk = ClockDomain.current.frequency.getMin
  val ref_clk = ClockDomain.current.frequency.getValue
  val max_ref_clk = ClockDomain.current.frequency.getMax

  lazy val ClockDomains = io.CLKS.map(cfg_clk => {
    val (cfg, clk) = cfg_clk
    val clkArea = new ClockingArea(new ClockDomain(clk)) {
      val reset = BufferCC(!io.LOCK)
    }

    val scale = HertzNumber(cfg.ACTUAL_FREQ) / ref_clk

    new ClockDomain(clk, reset = clkArea.reset,
      config = ClockDomainConfig(RISING, ASYNC, HIGH),
      frequency = FixedRangeFrequency(min_ref_clk * scale, max_ref_clk * scale))
  })

  noIoPrefix()
  if(ClockDomain.current.reset != null) {
    mapClockDomain(clock = io.REFCK, reset = io.PLLRESET)
  } else {
    mapClockDomain(clock = io.REFCK)
  }

  addPrePopTask( () => {
    Constraints.create_clock(io.REFCK, ClockDomain.current.frequency.getValue)
    io.CLKS.map(cfg_clk => {
      Constraints.create_clock(cfg_clk._2, cfg_clk._1.ACTUAL_FREQ Hz)
    })
    Constraints.add_clock_group(true, io.CLKS.map(_._2):_*)
  })
}

case class IoI2(io: Double, i2: Double, IPP_CTRL: Double, BW_CTL_BIAS: Double, IPP_SEL: Int)
case class NxPllParamPermutation(C1: Double, C2: Double, C3: Double, C4: Double, C5: Double, C6: Double, IPP_CTRL: Double, BW_CTL_BIAS: Double, IPP_SEL: Int, CSET: Double, CRIPPLE: Double, V2I_PP_RES: Int, IPI_CMP: Double)

// Adapted from https://github.com/enjoy-digital/litex/blob/master/litex/soc/cores/clock/lattice_nx.py
object PLLConfig {

  def to_bin_string(x : Int, places : Int): String = {
    s"0b" + x.toBinaryString.reverse.padTo(places, '0').reverse
  }

  val nclkouts_max        = 5
  val clki_div_range      = Array.range( 1, 128+1)

  val clkfb_div_range     = Array.range( 1, 128+1)
  val clkfb_frac_div_range     = Array.range( 16, 128+1)
  val clko_div_range      = Array.range( 1, 128+1)
  val clki_freq_range     = ( 10 MHz,   500 MHz)
  val clko_freq_range     = ( 6.25 MHz, 800 MHz)
  val vco_in_freq_range   = ( 10 MHz,   500 MHz)
  val vco_out_freq_range  = ( 800 MHz,  1600 MHz)

  val idx_to_name = Seq("P", "S", "S2", "S3", "S4", "S5")

  val valid_io_i2_permutations : Iterable[IoI2] = {
    // Valid permutations of IPP_CTRL, BW_CTL_BIAS, IPP_SEL, and IPI_CMP paramters are constrained
    // by the following equation so we can narrow the problem space by calculating the
    // them early in the process.
    // ip = 5.0/3 * ipp_ctrl*bw_ctl_bias*ipp_sel
    // ip/ipi_cmp == 50 +- 1e-4


    def is_valid_io_i2(IPP_CTRL: Double, BW_CTL_BIAS: Double, IPP_SEL: Double, IPI_CMP: Double): Option[(Double, Double)] = {
      val tolerance = 1e-4
      val ip = 5.0/3.0 * IPP_CTRL * BW_CTL_BIAS * IPP_SEL
      val i2 = IPI_CMP
      if (Math.abs(ip/i2-50) < tolerance) {
        Some((ip, i2))
      } else {
        None
      }
    }

    val valid_io_i2_permutations = mutable.Map[Double, IoI2]()

    // List out the valid values of each parameter
    val IPP_CTRL_VALUES = (1 to 4).map(_ * 1e-6)
    val BW_CTL_BIAS_VALUES = 1 to 15
    val IPP_SEL_VALUES = 1 to 4
    val IPI_CMP_VALUES = (1 to 15).map(_ * 0.5e-6)

    for (IPP_CTRL <- IPP_CTRL_VALUES;
         BW_CTL_BIAS <- BW_CTL_BIAS_VALUES;
         IPP_SEL <- IPP_SEL_VALUES;
         IPI_CMP <- IPI_CMP_VALUES) {
      is_valid_io_i2(IPP_CTRL, BW_CTL_BIAS, IPP_SEL, IPI_CMP) match {
        case Some((io, i2)) if !valid_io_i2_permutations.contains(io) =>
          valid_io_i2_permutations(io) = IoI2(io, i2, IPP_CTRL, BW_CTL_BIAS, IPP_SEL)
        case _ =>
      }
    }

    valid_io_i2_permutations.values
  }

  val transfer_func_coefficients = {
    // Take the permutations of the various analog parameters
    // then precalculate the coefficients of the transfer function.
    // During the final calculations sub in the feedback divisor
    // to get the final transfer functions.

    //       (ABF+EC)s^2 + (A(F(G+1)+B) + ED)s + A(G+1)          C1s^s + C2s + C3
    // tf = -------------------------------------------- =  --------------------------
    //               ns^2(CFs^2 + (DF+C)s + D)                ns^2(C4s^2 + C5s + C6)

    // A = i2*g3*ki
    // B = r1*c3
    // C = B*c2
    // D = c2+c3
    // E = io*ki*k1
    // F = r*cs
    // G = k3
    // n = total divisor of the feedback signal (output + N)

    // Constants
    val c3 = 20e-12
    val g3 = 0.2952e-3
    val k1 = 6
    val k3 = 100
    val ki = 508e9
    val r1 = 9.8e6
    val B = r1*c3

    // PLL Parameters
    val CSET_UNITS = 4e-12
    val CSET_VALUES = (2 to 17).map(_ * CSET_UNITS)
    val CRIPPLE_UNITS = 1e-12
    val CRIPPLE_VALUES = List(1, 3, 5, 7, 9, 11, 13, 15).map(_ * CRIPPLE_UNITS)
    val V2I_PP_RES_VALUES = List(9000, 9300, 9700, 10000, 10300, 10700, 11000, 11300)
    val transfer_func_coefficients = new ArrayBuffer[NxPllParamPermutation]()

    // Run through all the permutations and cache it all
    for (io_i2 <- valid_io_i2_permutations) {
      for (CSET <- CSET_VALUES) {
        for (CRIPPLE <- CRIPPLE_VALUES) {
          for (V2I_PP_RES <- V2I_PP_RES_VALUES) {
            val A = io_i2.i2*g3*ki
            val B = r1*c3
            val C = B*CSET
            val D = CSET+c3
            val E = io_i2.io*ki*k1
            val F = V2I_PP_RES*CRIPPLE
            val G = k3

            transfer_func_coefficients += NxPllParamPermutation(
              A*B*F+E*C, // C1
              A*(F*(G+1)+B)+E*D, // C2
              A*(G+1), // C3
              C*F, // C4
              D*F+C, // C5
              D, // C6
              io_i2.IPP_CTRL, io_i2.BW_CTL_BIAS, io_i2.IPP_SEL,
              CSET, CRIPPLE, V2I_PP_RES, io_i2.i2
            )
          }
        }
      }
    }

    transfer_func_coefficients
  }

  def calcTf(n: Double, s: Complex, params: NxPllParamPermutation): Complex = {
    (params.C1 * (s*s) + params.C2 * s + params.C3) / (n * (s*s) * (params.C4 * (s*s) + params.C5 * s + params.C6))
  }

  def closedLoopPeak(fbkdiv: Double, params: NxPllParamPermutation): (Double, Double) = {
    var f = 1e6
    var step = 1.1
    var stepDivs = 0

    var peakValue = -99.0
    var peakF = 0.0

    var lastValue = -99.0

    while (f < 1e9) {
      val s = Complex(0, 2 * Math.PI * f)
      val tfValue = calcTf(fbkdiv, s, params)
      val thisResult = 20 * Math.log10((tfValue/(1+tfValue)).abs)
      if (thisResult > peakValue) {
        peakValue = thisResult
        peakF = f
      }

      if (thisResult < lastValue && stepDivs < 5) {
        f = f / Math.pow(step, 2)
        step = (step - 1) * .5 + 1
        stepDivs += 1
      } else if (thisResult < lastValue && stepDivs == 5) {
        return (peakValue, peakF)
      } else {
        lastValue = thisResult
        f = f * step
      }
    }

    (peakValue, peakF)
  }

  def closedLoop3db(fbkdiv: Double, params: NxPllParamPermutation): Double = {
    var f = 1e6
    var step = 1.1
    var stepDivs = 0

    var lastF = 1.0

    while (f < 1e9) {
      val s = Complex(0, 2 * Math.PI * f)
      val tfValue = calcTf(fbkdiv, s, params)
      val thisResult = 20 * Math.log10((tfValue/(1+tfValue)).abs)

      if ((thisResult+3) < 0 && stepDivs < 5) {
        f = lastF
        step = (step - 1) * .5 + 1
        stepDivs += 1
      } else if ((thisResult+3) < 0 && stepDivs == 5) {
        return lastF
      } else {
        lastF = f
        f = f * step
      }
    }

    lastF
  }
  def openLoopCrossing(fbkdiv: Double, params: NxPllParamPermutation): (Double, Double) = {
    var f = 1e6
    var step = 1.1
    var stepDivs = 0

    var lastF = 1.0
    var lastTf = Complex(0, 0)

    while (f < 1e9) {
      val s = Complex(0, 2 * Math.PI * f)
      val tfValue = calcTf(fbkdiv, s, params)
      val thisResult = 20 * Math.log10((tfValue).abs)

      if (thisResult < 0 && stepDivs < 5) {
        f = lastF
        step = (step - 1) * .5 + 1
        stepDivs += 1
      } else if (thisResult < 0 && stepDivs == 5) {
        return (lastF, (-lastTf).phase*180/Math.PI)
      } else {
        lastF = f
        lastTf = tfValue
        f = f * step
      }
    }

    (lastF, (-lastTf).phase*180/Math.PI)
  }

  // The gist of calculating the analog parameters is to run through all the
  // permutations of the parameters and find the optimum set of values based
  // on the transfer function of the PLL loop filter. There are constraints on
  // on a few specific parameters, the open loop transfer function, and the closed loop
  // transfer function. An optimal solution is chosen based on the bandwidth
  // of the response relative to the input reference frequency of the PLL.

  // Later revs of the Lattice calculator BW_FACTOR is set to 10, may need to change it
  def calcOptimalParams(fref: Double, fbkdiv: Double, M: Int = 1, BW_FACTOR: Int = 5): Option[NxPllParamPermutation] = {
    println(s"Calculating Analog Parameters for a reference frequency of ${fref*1e-6} Mhz, feedback div $fbkdiv, and input div $M.")

    var bestParams: Option[NxPllParamPermutation] = None
    var best3db: Double = 0

    for (params <- transfer_func_coefficients) {
      breakable {
        val (closedLoopPeak, _) = this.closedLoopPeak(fbkdiv, params)
        if (closedLoopPeak < 0.8 || closedLoopPeak > 1.35) {
          break
        }

        val (_, openLoopCrossingPhase) = this.openLoopCrossing(fbkdiv, params)
        if (openLoopCrossingPhase <= 45) {
          break
        }

        val closedLoop3db = this.closedLoop3db(fbkdiv, params)
        val bwFactor = fref * 1e6 / M / closedLoop3db
        if (bwFactor < BW_FACTOR) {
          break
        }

        if (best3db < closedLoop3db) {
          best3db = closedLoop3db
          bestParams = Some(params)
        }
      }
    }

    println(s"Done calculating analog parameters: ${best3db}")
    //val HDLParams = numericalParamsToHDLParams(bestParams.get)
    //println(HDLParams)
    println(bestParams)
    //HDLParams
    bestParams
  }

  def create(inputClock : ClockSpecification, clkConfig : PLLClockConfig) : PLLConfig = {
    val fb_div = clkConfig.CLKFB_DIV
    val fb_div_frac = clkConfig.CLKFB_DIV_FRAC
    val clki_div = clkConfig.CLKI_DIV
    val ref_clk = clkConfig.CLK_REF
    val outputClocks = clkConfig.OUTPUTS

    val params = calcOptimalParams(inputClock.freq.toDouble, fb_div + fb_div_frac / 4096.0, clki_div).get
    val IPP_SEL_LUT = Map(1 -> 1, 2 -> 3, 3 -> 7, 4 -> 15)

    val pp_map = Map(
      11300 -> "11P3K",
      11000 -> "11K",
      10700 -> "10P7K",
      10300 -> "10P3K",
      9700 -> "9P7K",
      9300 -> "9P3K",
      9000 -> "9K"
    )

    val has_spread_spectrum = clkConfig.SPREAD_SPECTRUM
    val has_frac_n = clkConfig.CLKFB_FRAC_MODE

    /*

    localparam SSC_EN_SSC = ((SS_EN == 1) ? "ENABLED" : "DISABLED") ;
    localparam SSC_EN_SDM = ((FRAC_N_EN || SS_EN) ? "ENABLED" : "DISABLED") ;
    localparam SSC_ORDER = ((FRAC_N_EN || SS_EN) ? "SDM_ORDER2" : "SDM_ORDER1") ;
    localparam SSC_DITHER = "DISABLED" ;
    localparam SSC_N_CODE = ((FRAC_N_EN || SS_EN) ? SSC_N_CODE_STR : "0b000000000") ;
    localparam SSC_F_CODE = ((FRAC_N_EN || SS_EN) ? SSC_F_CODE_STR : "0b000000000000000") ;
    localparam SSC_PI_BYPASS = "NOT_BYPASSED" ;
    localparam SSC_SQUARE_MODE = "DISABLED" ;
    localparam SSC_EN_CENTER_IN = ((SSC_PROFILE == "CENTER") ? "CENTER_TRIANGLE" : "DOWN_TRIANGLE") ;
    localparam SSC_TBASE = (SS_EN ? SSC_TBASE_STR : "0b000000000000") ;
    localparam SSC_STEP_IN = (SS_EN ? SSC_STEP_IN_STR : "0b0000000") ;
    localparam SSC_REG_WEIGHTING_SEL = (SS_EN ? SSC_REG_WEIGHTING_SEL_STR : "0b000") ;
     */
    PLLConfig(
      SSC_EN_SSC        = has_spread_spectrum,
      SSC_EN_SDM        = has_spread_spectrum || has_frac_n,
      SSC_ORDER         = if (has_spread_spectrum || has_frac_n) "SDM_ORDER2" else "SDM_ORDER1",

      FBK_MMD_PULS_CTL  = {
        //localparam FBK_MMD_PULS_CTL = (FRAC_N_EN ? "0b0111" : (SS_EN ? "0b0101" : (((FBCLK_DIVIDER_ACTUAL_STR == "1") || (FBCLK_DIVIDER_ACTUAL_STR == "2")) ? "0b0000" : "0b0001"))) ;
        if (has_frac_n) "0b0111" else if (has_spread_spectrum) "0b0101" else if (fb_div <= 2) "0b0000" else "0b0001"
      },
      V2I_PP_ICTRL      = "0b11111", // Hard coded in all reference files
      IPI_CMPN          = "0b0011", // Hard coded in all reference files

      V2I_1V_EN         = true, // Enabled = 1V (Default in references, but not the primitive), Disabled = 0.9V
      V2I_KVCO_SEL      = "60", // if (VOLTAGE == 0.9V) 85 else 60
      KP_VCO            = "0b00011", // if (VOLTAGE == 0.9V) 0b11001 else 0b00011

      PLLPD_N           = "USED",
      PLLRESET_ENA      = true,
      REF_INTEGER_MODE  = true, // Ref manual has a discrepancy so lets always set this value just in case
      REF_MMD_DIG       = clki_div, // Divider for the input clock, ie 'M'

      SEL_FBK           = if(has_frac_n) "DIVA" else s"DIV${('A' + ref_clk.min(0)).toChar}",
      CLKMUX_FB         = s"CMUX_CLKO${idx_to_name(ref_clk.max(0))}",
      DIV_DEL           = if(has_frac_n) "0b0010011" else "0b0000001",
      FBK_INTEGER_MODE  = !has_frac_n,
      FBK_MASK          = if(has_frac_n) "0b00010000" else "0b00000000",
      FBK_MMD_DIG       = fb_div,

      //FBK_MMD_PULS_CTL = (has_frac_n ? "0b0111" : (SS_EN ? "0b0101" : (((FBCLK_DIVIDER_ACTUAL_STR == "1") || (FBCLK_DIVIDER_ACTUAL_STR == "2")) ? "0b0000" : "0b0001"))) ;

      FBK_CUR_BLE = if (has_frac_n) "0b00001000" else "0b00000000",
      FBK_PI_RC = if(has_frac_n) "0b0010" else "0b1100",
      FBK_PR_CC = if(has_frac_n) "0b1000" else "0b0000",
      FBK_PR_IC = if(has_frac_n) "0b1000" else "0b1000",

    //localparam REF_MMD_PULS_CTL = (((CLKI_DIVIDER_ACTUAL_STR == "1") || (CLKI_DIVIDER_ACTUAL_STR == "2")) ? "0b0000" : "0b0001") ;
      REF_MMD_PULS_CTL  = if (clki_div <= 2) "0b0000" else "0b0001",

      CRIPPLE = (params.CRIPPLE / 1e-12).toInt.toString + "P",
      CSET = ((params.CSET / 4e-12)*4).toInt.toString + "P",
      V2I_PP_RES = pp_map(params.V2I_PP_RES),
      IPP_CTRL = "0b%04d".format((params.IPP_CTRL / 1e-6 + 3).round.toBinaryString.toInt),
      IPI_CMP = "0b%04d".format((params.IPI_CMP / .5e-6).round.toBinaryString.toInt),
      BW_CTL_BIAS = "0b%04d".format(params.BW_CTL_BIAS.round.toBinaryString.toInt),
      IPP_SEL = "0b%04d".format(IPP_SEL_LUT(params.IPP_SEL).toBinaryString.toInt),
      OUTPUT_CLKS = outputClocks,

      SSC_N_CODE        = if (has_spread_spectrum || has_frac_n) to_bin_string(fb_div, 9) else "0b000000000",
      SSC_F_CODE        = if (has_spread_spectrum || has_frac_n) to_bin_string(fb_div_frac, 15) else "0b000000000000000",

    )
  }

  def create_clock_config(vco_freq : Rational, vco_tolerance : Double, spec : ClockSpecification): (Double, PLLOutputClockConfig, Boolean) = {
    val lowest_freq = (spec.freq * (1 - spec.tolerance)).max(6.25 MHz)
    val highest_freq = (spec.freq * (1 + spec.tolerance)).min(800 MHz)

    val spec_freq_d = spec.freq.toDouble
    val spec_tol_d = spec.tolerance.toDouble

    val range_start = (vco_freq/highest_freq.toInt).toDouble.floor.toInt.min(128)
    val range_end = (vco_freq/lowest_freq.toInt).toDouble.ceil.toInt.min(128)
    val div_range = Array.range(range_start, range_end + 1)

    val validOptions = div_range.map(d => {
        val phasef = ((1 + spec.phaseOffset / 360) * d)
        val phase = phasef.round.toInt
        val actual_phase = ((phase.toDouble / d) - 1) * 360

        val clk_freq = vco_freq / d
        (
          (clk_freq.toDouble - spec.freq.toInt).abs,
          (actual_phase - spec.phaseOffset).abs,
          PLLOutputClockConfig(
            ENABLE = true,
            DIV = d - 1,
            DEL = phase - 1,
            ACTUAL_FREQ = clk_freq.toDouble,
            ACTUAL_PHASE = actual_phase
          )
        )
      })
      .map(r => (r._1 * 1e-6 + r._2 / 360.0, r._3, r._1 <= (spec_freq_d * (spec_tol_d + vco_tolerance))))
      .sortBy(r => r._1)

    validOptions.head
//    (validOptions.head._1,
//      if(validOptions.head._3) Some(validOptions.head._2) else None)
  }

  def valid_fbclks(inputClock : ClockSpecification): Seq[Rational] = {
    val vco_max = 1600000000
    val vco_min = 800000000
    val max_clki_div = (inputClock.freq / (18 MHz)).toDouble.floor.toInt
    val min_clki_div = (inputClock.freq / (100 MHz)).toDouble.ceil.toInt

    (for {
      clki_div <- Array.range(min_clki_div, max_clki_div);
      clkfb_div <- clkfb_div_range
    } yield Rational((inputClock.freq * clkfb_div).toInt, clki_div))
      .filter(x => x.toDouble <= (800000000) && x.toDouble >= 6250000)
      .distinct.sortBy[Double](-_.toDouble)
  }

  def valid_nonfrac_vcos(inputClock : ClockSpecification) : Seq[Rational] = {
    val fbclks = valid_fbclks(inputClock)

    val vco_max = 1600000000
    val vco_min = 800000000

    def in_range(fbclk : Rational): Seq[Int] = {
      val clko_div_range_start = Math.max((vco_min / fbclk.toDouble).ceil.toInt, clko_div_range.head)
      val clko_div_range_end = Math.min((vco_max / fbclk.toDouble).floor.toInt, clko_div_range.last)
      Array.range(clko_div_range_start, clko_div_range_end + 1)
    }
    // fbclk * clko_div <= 1600000000
    // fbclk * clko_div <=  800000000

    (for {
      fbclk <- fbclks;
      clko_div <- in_range(fbclk)
    } yield fbclk * clko_div).filter(x => x.toDouble <= (1600000000) && x.toDouble >= (800000000)).distinct.sortBy[Double](-_.toDouble)
  }

  def valid_frac_vcos(inputClock : ClockSpecification) : Seq[(Rational, Int, Int, Int)] = {
    val rtn = mutable.ArrayBuffer[(Rational, Int, Int, Int)]()

    val valid_clki_div = clki_div_range
      .filter(clki_div => {
        val phase_clk = (Rational(inputClock.freq.toInt, clki_div).toDouble Hz)
        phase_clk >= (18 MHz) && phase_clk <= (100 MHz)
      })

    breakable {
      for (clki_div <- valid_clki_div) {
        val phase_clk = Rational(inputClock.freq.toInt, clki_div)

        breakable {
          for (clkfb_div <- clkfb_frac_div_range) {
            for (clkfb_frac_value <- 0 until 4096) {
              val vco = phase_clk * Rational(clkfb_frac_value + clkfb_div * 4096, 4096)
              if ((vco.toDouble Hz) > ((1600 MHz))) {
                break
              }
              if ((vco.toDouble Hz) >= ((800 MHz))) {
                rtn.append((vco, clki_div, clkfb_div, clkfb_frac_value))
              }
            }
          }
        }

      }
    }

    rtn.distinct.sortBy[Double](-_._1.toDouble)
  }

  // Clk #, idiv, fbdiv, fractional
  def find_ref_clock(inputClock : ClockSpecification, outputClocks : PLLOutputClockConfig*) : Option[PLLClockConfig] = {
    val inputClockFreq_d = inputClock.freq.toDouble
    def find_valid_settings_for_clk(clk : PLLOutputClockConfig): Option[PLLClockConfig] = {
      breakable {
        for (clki_div <- clki_div_range) {
          val phase_detect_freq = inputClock.freq / clki_div
          if (phase_detect_freq < (18 MHz)) {
            break()
          }
          else if ((phase_detect_freq <= (500 MHz))) {
            val clkfb_div = (clk.ACTUAL_FREQ * clki_div / inputClockFreq_d)
            if (clk.ACTUAL_PHASE == 0 && clkfb_div.round == clkfb_div && clkfb_div < (128)) {
              return Some(PLLClockConfig(CLKI_DIV = clki_div, CLKFB_DIV = clkfb_div.toInt))
            }
          }
        }
      }
      None
    }

    outputClocks.map(find_valid_settings_for_clk).zipWithIndex.map(x => x._1.map(_.copy(CLK_REF=x._2)))
      .find(x => x.nonEmpty).flatten

  }

  def solve_non_fractional(inputClock : ClockSpecification, actualOutputClocks : ClockSpecification*): Option[PLLClockConfig] = {
    var bestError = Double.MaxValue
    var bestIsValid = false
    var best : Option[PLLClockConfig] = None //(HertzNumber, Int, Int, Int, Int, Seq[Option[PLLOutputClockConfig]]) = (0 MHz, 0, 0, 0, 0, Seq.empty)

    val vcos = valid_nonfrac_vcos(inputClock)

    for (vco_freq <- vcos) {
      val configs = actualOutputClocks.map(x => create_clock_config(vco_freq, inputClock.tolerance, x))
      val error = configs.map(x => x._1).sum

      val all_valid = configs.map(_._3).fold(true)((a,b) => a & b)

      if((all_valid) && bestError > error) {

        val refClks = find_ref_clock(inputClock, configs.map(_._2):_*)
        if(refClks.nonEmpty) {
          bestError = error
          best = Some(refClks.get.copy(VCO = vco_freq.toBigDecimal Hz, OUTPUTS = configs.map(_._2)))
          bestIsValid = all_valid
        }
      }
    }

    best
  }
  def find_frac_params(inputClock : ClockSpecification, vco : Rational ) : PLLClockConfig = {
    for (clki_div <- clki_div_range;
         clkfb_div <- clkfb_div_range;
         clkfb_frac_value <- 0 until 4096) {
      val calc_vco = ((Rational(clkfb_frac_value, 4095) + clkfb_div) * inputClock.freq.toInt / clki_div)
      if(calc_vco == vco) {
        return PLLClockConfig(vco.toBigDecimal Hz, clki_div, clkfb_div, clkfb_frac_value, -1)
      }
    }
    null
  }

  def solve_fractional(inputClock : ClockSpecification, actualOutputClocks : ClockSpecification*): Option[PLLClockConfig] = {
    var bestError = Double.MaxValue
    var bestIsValid = false
    var best : PLLClockConfig = null //(HertzNumber, Int, Int, Int, Int, Seq[Option[PLLOutputClockConfig]]) = (0 MHz, 0, 0, 0, 0, Seq.empty)

    val vcos = valid_frac_vcos(inputClock)

    for ((vco_freq, clki_div, clkfb_div, clkfb_frac_value) <- vcos) {
      val configs = actualOutputClocks.map(x => create_clock_config(vco_freq, inputClock.tolerance, x))
      val error = configs.map(x => x._1).sum

      val all_valid = configs.map(_._3).fold(true)((a,b) => a & b)

      if((all_valid || !bestIsValid) && bestError > error) {
        val params = PLLClockConfig(VCO = vco_freq.toBigDecimal Hz, CLKI_DIV = clki_div, CLKFB_DIV = clkfb_div, CLKFB_DIV_FRAC = clkfb_frac_value, CLK_REF = -1, CLKFB_FRAC_MODE = true)
        bestError = error
        best = params.copy(OUTPUTS = configs.map(_._2))
        bestIsValid = all_valid
      }
    }

    if(best != null) {
      for (oc <- actualOutputClocks.zip(best.OUTPUTS)) {
        println(s"Requested ${oc._1} vs ${HertzNumber(oc._2.ACTUAL_FREQ).decompose}")
      }
    }

    if(!bestIsValid) {
      println(s"Could not find a valid PLL solution")
      None
    } else {
      Some(best)
    }
  }

  def createClockConfig(inputClock : ClockSpecification, outputClocks : ClockSpecification*): PLLClockConfig = {
    var config = Map[String, Any]()

    var actualOutputClocks = outputClocks

    // The clock system here works by using the inputClock to establish a VCO between 800 and 1600. Each output clock
    // is tuned according to
    // F_on = VCO / On
    // F_on is the output freq
    // F_i is the input clock freq
    // M is the input divider
    // On is the divider for the Nth clock
    // N is the FB divider
    // Additionally, there must exist a ref clock with no phase shift for which
    // F_o = F_i * N / M
    // VCO / Oref = F_i * N / M
    // VCO = F_i * Oref * N / M
    //
    // Additionally, we want to maximize VCO (to minimize jitter) and also minimize clock / phase error
    // If we only need < 6 of the available 6 clock inputs, we can just say solve around everything else and use
    // unused clock outputs as the VCO
    //
    // Otherwise we can more or less brute force it
    // F_i is known
    // F_on... are to be optimized
    // N, M, On, R are unknowns
    //
    // For fractional mode, the PLL works differently
    // VCO = F_i / N * M
    // And it seems like the ref clock is internally driven

    var solution = solve_non_fractional(inputClock, actualOutputClocks:_*)
    //solution = None
    if(solution.isEmpty) {
      solution = solve_fractional(inputClock, actualOutputClocks: _*)
    }
    if(solution.isEmpty) {
      throw new IllegalArgumentException("No PLL config found")
    }

    solution.get.print(actualOutputClocks)
    solution.get
  }

  def create(inputClock : ClockSpecification, outputClocks : ClockSpecification*): PLLConfig = {
    create(inputClock, createClockConfig(inputClock, outputClocks:_*))
  }

}

object PLL extends App {
  def apply(inputClock : ClockDomain, outputClocks : ClockSpecification*): Seq[ClockDomain] = {
    val pll_input = {
      if(inputClock == null) {
        val osc = new OSCD(OSCDConfig.create(ClockSpecification(60 MHz, tolerance=.1)))
        osc.hf_clk().get
      } else {
        inputClock
      }
    }

    new ClockingArea(clockDomain = pll_input) {
      val pll = new PLL(PLLConfig.create(ClockSpecification.fromClock(pll_input),
        outputClocks:_*
      ))
    }.pll.ClockDomains
  }

  def apply(outputClocks : ClockSpecification*): Seq[ClockDomain] = {
    apply(null, outputClocks:_*)
  }

  Config.spinal.generateVerilog(
    new Component {
      val gen = new TestClockGen(41.6666667, 1)

      val oscd = new OSCD(OSCDConfig.create(lib.misc.ClockSpecification(45 MHz, tolerance = .1), lib.misc.ClockSpecification(60 MHz, tolerance = .1)))

      val clockGenArea = new ClockingArea(new ClockDomain(gen.io.eclk, gen.io.reset)) {
        val dut = new PLL(PLLConfig.create(lib.misc.ClockSpecification(24 MHz),
          lib.misc.ClockSpecification(60.6 MHz),
          lib.misc.ClockSpecification(101 MHz, tolerance = 0),
          lib.misc.ClockSpecification(101 MHz, 90),
          lib.misc.ClockSpecification(202 MHz, 25),
          lib.misc.ClockSpecification(71.3 MHz, 135, .05),
          //ClockSpecification(90 MHz, 0, .05),
        ))

        val counter = Reg(UInt(32 bits)) init(0)
        when(~dut.io.LOCK) {
          counter := 0
        } otherwise {
          counter := counter + 1
        }
      }

      val clockAreas = clockGenArea.dut.ClockDomains.map(cd => {
        new ClockingArea(cd) {
          val counter = Reg(UInt(32 bits)) init(0)
          counter := counter + 1
        }
      })
    }.setDefinitionName("PLLTest")
  )
}
