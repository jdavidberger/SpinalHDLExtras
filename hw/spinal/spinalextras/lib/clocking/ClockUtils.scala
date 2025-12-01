package spinalextras.lib.clocking

import spinal.core._
import spinal.lib._

/**
 * Turn an asynchronous reset (active-high) into a synchronous reset.
 *
 * @param stages number of synchronizer stages used to synchronize de-assertion (>=1)
 */
case class AsyncResetSynchronizer(stages: Int = 3) extends Component {
  require(stages >= 1, "stages must be >= 1")

  val io = new Bundle {
    val asyncReset = in Bool()   // active-high async reset input
    val syncReset  = out Bool()  // active-high sync reset output (synchronously released)
  }

  // vector of regs used to synchronize release. initialize to 'true' (in reset)
  val staging = Reg(Bits(stages bits)) init( B(1 << (stages - 1)))

  when(BufferCC(io.asyncReset)) {
    // asynchronous assert: keep all stages asserted immediately
    staging.setAll()
  } otherwise {
    staging := (staging << 1).resized
  }

  io.syncReset := staging.msb
}

object ClockUtils {

  def asAsyncReset(clockDomain: ClockDomain): ClockDomain = {
    if (clockDomain.config.resetKind == SYNC) {
      clockDomain.copy(
        reset = BufferCC(clockDomain.isResetActive),
        config = clockDomain.config.copy(resetKind = ASYNC))
    } else {
      clockDomain
    }
  }
  def createBootClock(clk: Bool, freq : IClockDomainFrequency = UnknownFrequency()) : ClockDomain = {
    new ClockDomain(clk, config = ClockDomainConfig(resetKind = BOOT), frequency = freq)
  }

  def asyncAssertSyncDeassertCreateCd(resetCd : ClockDomain,
                                      clockCd : ClockDomain = ClockDomain.current,
                                      bufferDepth : Option[Int] = None) : ClockDomain = {
    clockCd.copy(
      clock = clockCd.clock,
      config = clockCd.config.copy(resetKind = ASYNC),
      reset = ResetCtrl.asyncAssertSyncDeassert(
        input = resetCd.isResetActive,
        clockDomain = clockCd,
        inputPolarity = resetCd.config.resetActiveLevel,
        outputPolarity = clockCd.config.resetActiveLevel,
        bufferDepth = bufferDepth
      ).setCompositeName(resetCd.isResetActive, "synchronized", true)
    )
  }

  def createAsyncClock(clk: Bool, freq : IClockDomainFrequency = UnknownFrequency()): ClockDomain = {
    asyncAssertSyncDeassertCreateCd(ClockDomain.current, createBootClock(clk, freq))
  }

  def createAsyncReset(clk: Bool, reset : Bool) = {
    ResetCtrl.asyncAssertSyncDeassert(
      input = reset,
      clockDomain = createBootClock(clk)
    )
  }

  def makeActiveHighClock(cd: ClockDomain): ClockDomain = {
    cd.config.resetActiveLevel match {
      case HIGH => cd
      case LOW => cd.copy(reset = cd.isResetActive, config = cd.config.copy(resetActiveLevel = HIGH))
    }
  }
}