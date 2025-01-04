package spinalextras.lib.bus

import spinal.core._
import spinal.lib._
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusCmd, PipelinedMemoryBusConfig}

case class DirectBus(config : PipelinedMemoryBusConfig) extends Bundle with IMasterSlave {
  val cmd = PipelinedMemoryBusCmd(config)
  val rsp = Bits(config.dataWidth bits)

  val valid, ready = Bool()
  override def asMaster(): Unit = {
    out(cmd, valid)
    in(rsp, ready)
  }

  def fire = valid && ready

  def setIdle(): Unit = {
    valid := False
    cmd.assignDontCare()
  }
}

object DirectBus {
  def apply(bus : PipelinedMemoryBus): DirectBus = {
    val busOut = Stream(new DirectBus(bus.config))


    busOut
  }

  def toPipelinedMemoryBus(dBus : DirectBus): PipelinedMemoryBus = new Composite(dBus, "toPipelinedMemoryBus"){
    val bus = PipelinedMemoryBus(dBus.config)

    val expectingRead = RegInit(False) setWhen(bus.readRequestFire) clearWhen(bus.rsp.fire)
    val cmdLatch = RegInit(False) setWhen(dBus.valid && !dBus.ready && !expectingRead) clearWhen(bus.cmd.fire)
    when(cmdLatch) {
      assert(!expectingRead)
      assert(dBus.valid)
    }
    bus.cmd.valid := cmdLatch
    bus.cmd.payload := dBus.cmd

    dBus.rsp := bus.rsp.data
    dBus.ready := bus.rsp.valid || (bus.cmd.fire && bus.cmd.write)

    assert(bus.formalContract.outstandingReads === expectingRead.asUInt)
  }.bus

}