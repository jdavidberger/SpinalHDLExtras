package spinalextras.lib.misc

import spinal.core._
import spinal.lib.bus.simple._
import spinal.lib._

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

case class PipelinedMemoryBusBuffered(cfg : PipelinedMemoryBusConfig, inFlightMax : Int) extends Component {
  val io = new Bundle {
    val config = cfg
    val cmd = slave(Stream(PipelinedMemoryBusCmd(config)))
    val rsp = master(Stream(PipelinedMemoryBusRsp(config)))

    val bus = master(PipelinedMemoryBus(config))

  }
//  val fifo = StreamFifo(io.rsp.payload, inFlightMax)
//  fifo.io.push.payload := io.rsp.payload
//  fifo.io.push.valid := io.rsp.valid
//  assert(~fifo.io.push.isStall, "Bus Buffered Stalled")
//  fifo.io.pop <> io.rsp

  val overflow = Bool()
  io.rsp <> io.bus.rsp.toStream(overflow)
  assert(~overflow, "Bus Buffered Stalled")

  val inFlight = new CounterUpDown(inFlightMax, handleOverflow = false)
  when(io.bus.cmd.fire) { inFlight.increment() }
  when(io.bus.rsp.fire) { inFlight.decrement() }

  io.cmd.takeWhen(io.rsp.ready && ~inFlight.willOverflowIfInc) >> io.bus.cmd
}

object PipelinedMemoryBusBuffered {
  def apply(bus: PipelinedMemoryBus, rspQueue : Int) = {
    val dut = new PipelinedMemoryBusBuffered(bus.config, rspQueue)
    dut.io.bus <> bus
    dut.io
  }

}