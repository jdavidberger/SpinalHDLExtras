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

case class ClockSelection(inputClock: ClockFrequency, outputClocks: Seq[ClockSpecification], bootstrap : Boolean = false) extends Component {
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
    val pll = PLLs(ClockSpecification.fromClock(external_clock_domain), outputClocks)
    assert(pll != null, "Must be able to generate a pll")
  }.pll
  io.pll_lock := pll.lock

  val dcs_out = bootstrap generate DCS(osc.hf_clk().get, pll.ClockDomains.head, pll.lock)

  val pll_reset = RegInit(True) clearWhen (io.pll_lock)

  for ((clk, idx) <- pll.ClockDomains.zipWithIndex) {
    io.clks(idx) := {
      if (bootstrap) dcs_out.readClockWire else clk.readClockWire
    }

    var name = s"${(clk.frequency.getValue.toDouble / 1e6).round.toInt}mhz"
    io.resets(idx) := {
      val rawReset = if(bootstrap) reset else pll_reset
      ClockUtils.createAsyncReset(io.clks(idx), rawReset)
    }.setName(s"rst_sync_${name}")

    if (pll.outputSpectifications(idx).phaseOffset != 0) {
      name += s"_${pll.outputSpectifications(idx).phaseOffset.round}deg"
    }

    io.clks(idx).setName(s"clk_${name}")
    io.resets(idx).setName(s"clk_${name}_reset")
  }

  lazy val ClockDomains = pll.ClockDomains.zipWithIndex.map(cd_idx => {
    val (cd, idx) = cd_idx
    new ClockDomain(io.clks(idx), reset = io.resets(idx), frequency = cd.frequency, config = ClockDomain.current.config.copy(resetKind = ASYNC))
  })
}

object ClockSelection extends App {
  Config.spinal.generateVerilog(
    ClockSelection(FixedFrequency(24 MHz),
      Seq(ClockSpecification(60 MHz), ClockSpecification(100 MHz), ClockSpecification(50 MHz), ClockSpecification(120 MHz))),
  )
}