package spinalextras.lib.misc

import spinal.core._
import spinal.lib._

class AsyncToSyncReset extends Component {
  val io = new Bundle {
    val sync_reset = out Bool()
  }

  assert(ClockDomain.current.config.resetKind == ASYNC)
  io.sync_reset.setAsReg()
  io.sync_reset := BufferCC(ClockDomain.current.readResetWire)
}

object AsyncToSyncReset {
  def apply(clk : Bool, async_reset : Bool): Bool = {
    new ClockingArea(ClockDomain(clk, reset = async_reset, config = ClockDomain.current.config.copy(resetKind = ASYNC, resetActiveLevel = HIGH))) {
      val reset_block = new AsyncToSyncReset()
    }.reset_block.io.sync_reset
  }
}