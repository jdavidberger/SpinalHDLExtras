package spinalextras.lib.misc

import spinal.core.{B, Bundle, Component, False, IntToBuilder, RegNextWhen, TimeNumber, True, when}
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusConfig}
import spinal.lib.{Timeout, master, slave}

case class PipelinedMemoryBusTimeout(config : PipelinedMemoryBusConfig, timeout : TimeNumber = 100 us) extends Component {
  val io = new Bundle {
    val pmb_m = master(PipelinedMemoryBus(config))
    val pmb_s = slave(PipelinedMemoryBus(config))
  }

  io.pmb_m.cmd <> io.pmb_s.cmd
  val timeout_counter = Timeout(timeout)
  val needsRdyCond = io.pmb_s.cmd.valid
  val needsRdy = RegNextWhen(True, needsRdyCond) clearWhen(io.pmb_s.cmd.fire) init(False)
  val needsRespCond = io.pmb_s.cmd.fire && !io.pmb_s.cmd.write
  val needsResp = RegNextWhen(True, needsRespCond) clearWhen(io.pmb_s.rsp.valid) init(False)
  when(needsRespCond || needsRdyCond.rise(False)) {
    timeout_counter.clear()
  }
  when(timeout_counter && needsResp) {
    io.pmb_s.rsp.data := B(0xdeadbeefL, 32 bits).resized
    io.pmb_s.rsp.valid := True
  } elsewhen (timeout_counter && needsRdy) {
    io.pmb_s.cmd.ready := True
    io.pmb_s.rsp.valid := True
    io.pmb_s.rsp.data := B(0xcafed00dL, 32 bits).resized
  } otherwise {
    io.pmb_m.rsp <> io.pmb_s.rsp
  }
}
object PipelinedMemoryBusTimeout {
  def apply(bus : PipelinedMemoryBus) : PipelinedMemoryBus = {
    val dut = PipelinedMemoryBusTimeout(bus.config)
    dut.io.pmb_m <> bus
    dut.io.pmb_s
  }
}