package spinalextras.lib.bus

import spinal.core._

import spinal.lib._
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusCmd, PipelinedMemoryBusConfig}
import spinal.lib.formal.HasFormalAsserts

case class DirectBus(config : PipelinedMemoryBusConfig) extends Bundle with IMasterSlave {
  val cmd = PipelinedMemoryBusCmd(config)
  val rsp = Bits(config.dataWidth bits)

  val valid, ready = Bool()
  override def asMaster(): Unit = {
    out(cmd, valid)
    in(rsp, ready)
  }

  def isStall = valid && !ready
  def fire = valid && ready

  def setIdle(): Unit = {
    valid := False
    cmd.assignDontCare()
  }

  def transactionFlow() : Flow[PipelinedMemoryBusCmd] = {
    val tx = Flow(PipelinedMemoryBusCmd(config))
    tx.address := cmd.address
    tx.data := cmd.write ? cmd.data | rsp
    tx.mask := cmd.mask
    tx.write := cmd.write
    tx.valid := fire
    tx
  }

  def formalIsProducerValid() = {
    val wasStall = RegNext(isStall) init(False)
    val steadyValid = valid || !wasStall
    val lastPayload = RegNextWhen(cmd, valid)
    val payloadInvariant = (lastPayload === cmd) || !wasStall
    steadyValid && payloadInvariant
  }

  def formalIsConsumerValid() = True
}

object DirectBus {
  def apply(bus : PipelinedMemoryBus): DirectBus = {
    val busOut = Stream(new DirectBus(bus.config))


    busOut
  }

  def toPipelinedMemoryBus(dBus : DirectBus): PipelinedMemoryBus = new Composite(dBus, "toPipelinedMemoryBus") with HasFormalAsserts {
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

    override lazy val formalValidInputs = bus.formalIsConsumerValid() && dBus.formalIsProducerValid()

    override protected def formalChecks()(implicit useAssumes: Boolean): Unit = {
      assertOrAssume(bus.formalIsProducerValid())
      assertOrAssume(bus.formalContract.outstandingReads.value === (!cmdLatch).asUInt)
    }
  }.bus

}