package spinalextras.lib.clocking

import spinal.core.ClockDomain.{ClockFrequency, FixedFrequency}
import spinal.core._
import spinal.lib._
import spinalextras.lib.Config

import spinalextras.lib.blackbox.lattice.lifcl.{DCS, OSCD, OSCDConfig}
import spinalextras.lib.misc.ClockSpecification

import scala.language.postfixOps

object rst_sync {
  def apply(clk : Bool, rst : Bool): Bool = {
    new ClockingArea(new ClockDomain(clock = clk, config = ClockDomainConfig(resetKind = BOOT))) {
      val reset = AsyncResetSynchronizer()
      reset.io.asyncReset := rst
    }.reset.io.syncReset
  }

  def apply(clockDomain: ClockDomain): ClockDomain = {
    if (clockDomain.config.resetKind == ASYNC) {
      new ClockDomain(clock = clockDomain.readClockWire,
        reset = rst_sync(clockDomain.readClockWire, clockDomain.readResetWire),
        config = clockDomain.config.copy(resetKind = SYNC), frequency = clockDomain.frequency)
    } else {
      clockDomain
    }
  }
}

class fpga_reset() extends Component {
  val io = new Bundle {
    val reset_o = out Bool()
  }

  val reset_i = ClockDomain.readResetWire

  val resetClockDomain = ClockDomain.current.copy(reset = null, config = ClockDomain.current.config.copy(resetKind = BOOT))
  val resetClockArea = new ClockingArea(resetClockDomain) {
    val timeout = Timeout(1 << 9)
    io.reset_o := True
    when(BufferCC(reset_i)) {
      timeout.clear()
    }
    when(timeout) {
      io.reset_o := False
    }
  }

  noIoPrefix()
}

class fpga_reset_bb() extends BlackBox {
  val io = new Bundle {
    val clk = in Bool()
    val reset_n_i = in Bool()
    val reset_n_o = out Bool()
  }
  noIoPrefix()

  setDefinitionName("fpga_reset")
  mapCurrentClockDomain(io.clk, io.reset_n_i, resetActiveLevel = LOW)
}

object fpga_reset {
  def apply(): Bool = {
    (new fpga_reset()).io.reset_o
  }
  def apply(clk : Bool, rst : Bool): Bool = {
    new ClockingArea(new ClockDomain(clock = clk, reset = rst)) {
      val reset = fpga_reset()
    }.reset
  }
}

class ClockSelection(outputClocks: Seq[ClockSpecification], bootstrap : Boolean = false) extends Component {
  val inputClockFrequency = ClockDomain.current.frequency.getValue
  val outputClocksRefIdx = outputClocks.indexWhere(c => c.phaseOffset == 0 && c.freq == inputClockFrequency)
  val refClockIsOutput = outputClocksRefIdx != -1
  val outputClocksWithoutRef = if (outputClocksRefIdx == -1) outputClocks else outputClocks.patch(outputClocksRefIdx, Nil, 1)

  val io = new Bundle {
    val clks = outputClocks.map(_ => out(Bool()))
    val resets = outputClocks.map(_ => out(Bool()))

    val pll_lock = out(Bool())
  }
  noIoPrefix()
  val osc = bootstrap generate OSCD(OSCDConfig.create(ClockSpecification(75 MHz, tolerance = .1)))

  val external_clock_domain = ClockDomain.current

  val reset = ResetCtrl.asyncAssertSyncDeassert(
    input = external_clock_domain.reset,
    clockDomain = external_clock_domain
  )

  var pll = new ClockingArea(clockDomain = external_clock_domain) {
    val pll = PLLs(ClockSpecification.fromClock(external_clock_domain), outputClocksWithoutRef)
    assert(pll != null, "Must be able to generate a pll")
  }.pll
  io.pll_lock := pll.lock

  val dcs_out = bootstrap generate DCS(osc.hf_clk().get, pll.ClockDomains.head, pll.lock)

  val pll_count = Counter(128)
  when(io.pll_lock) {
    pll_count.increment()
  } otherwise {
    pll_count.clear()
  }

  val pll_reset = RegInit(True) clearWhen (pll_count.willOverflow)
  var clockDomains = pll.ClockDomains
  if(refClockIsOutput)
    clockDomains = clockDomains ++ Seq(ClockDomain.current)

  for (out_idx <- outputClocks.indices) {
    val cd = if(out_idx == outputClocksRefIdx) {
      ClockDomain.current
    } else if(out_idx < outputClocksRefIdx || outputClocksRefIdx == -1) {
      pll.ClockDomains(out_idx)
    } else {
      pll.ClockDomains(out_idx + 1)
    }

    io.clks(out_idx) := {
      if (out_idx == 0 && bootstrap) dcs_out.readClockWire else cd.readClockWire
    }

    var name = s"${(cd.frequency.getValue.toDouble / 1e6).round.toInt}mhz"
    io.resets(out_idx) := {
      val rawReset = if(bootstrap) reset else pll_reset
      ClockUtils.createAsyncReset(io.clks(out_idx), rawReset)
    }.setName(s"rst_sync_${name}", true)

    io.clks(out_idx).setName(s"clk_${name}", true)
    io.resets(out_idx).setName(s"clk_${name}_reset", true)
  }

  lazy val ClockDomains = clockDomains
}

object ClockSelection {
  def apply(outputClocks: Seq[ClockSpecification], bootstrap : Boolean = false, requirePLL : Boolean = true) = {
    if(!requirePLL && ClockDomain.current.frequency.getValue == outputClocks.head.freq && outputClocks.size == 1) {
      val reset = ResetCtrl.asyncAssertSyncDeassert(
        input = ClockDomain.current.reset,
        clockDomain = ClockDomain.current
      ).setCompositeName(Component.current, "filteredReset")

      (True, Seq(ClockDomain.current.copy(reset = reset)))
    } else {
      val selection = new ClockSelection(outputClocks, bootstrap)
      (selection.io.pll_lock, selection.ClockDomains)
    }
  }
}