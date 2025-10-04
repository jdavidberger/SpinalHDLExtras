package spinalextras.lib.bus

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusCmd, PipelinedMemoryBusConfig}
import spinalextras.lib.formal.fillins.PipelinedMemoryBusFormal.PipelinedMemoryBusFormalExt
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalMasterSlave, FormalProperties, FormalProperty}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

import scala.language.postfixOps

case class DirectBus(config : PipelinedMemoryBusConfig) extends Bundle with IMasterSlave with FormalMasterSlave {
  val cmd = PipelinedMemoryBusCmd(config)
  val rsp = Bits(config.dataWidth bits)

  val valid, ready = Bool()
  override def asMaster(): Unit = {
    out(cmd, valid)
    in(rsp, ready)
  }

  def isStall = valid && !ready
  def fire = valid && ready
  def readFire = fire && !cmd.write

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

  override def formalIsProducerValid() = new FormalProperties(this) {
    val wasStall = RegNext(isStall) init(False)
    val steadyValid = valid || !wasStall
    val lastPayload = RegNextWhen(cmd, valid)
    val payloadInvariant = (lastPayload === cmd) || !wasStall
    addFormalProperty(steadyValid, "Valid was not steady")
    addFormalProperty(payloadInvariant, "Payload was not invariant")
  }

  def write(address: UInt, data: Bits) = {
    cmd.write := True
    cmd.assignWordAddress(address)
    cmd.mask.setAll()
    cmd.data := data
    valid := True

    fire
  }

  def read(address: UInt) = {
    cmd.write := False
    cmd.assignWordAddress(address)
    cmd.mask.setAll()
    cmd.data := 0
    valid := True

    fire
  }
}

case class DirectBusToPipelinedMemoryBus(config : PipelinedMemoryBusConfig) extends ComponentWithFormalProperties {
  val io = new Bundle {
    val db = slave (DirectBus(config))
    val pmb = master (PipelinedMemoryBus(config))
  }

  val expectingRead = RegInit(False) setWhen(io.pmb.readRequestFire) clearWhen(io.pmb.rsp.fire)
  val cmdLatch = RegInit(False) setWhen(io.db.valid && !io.db.ready && !expectingRead) clearWhen(io.pmb.cmd.fire)

  io.pmb.cmd.valid := cmdLatch
  io.pmb.cmd.payload := io.db.cmd

  io.db.rsp := io.pmb.rsp.data
  io.db.ready := io.pmb.rsp.valid || (io.pmb.cmd.fire && io.pmb.cmd.write)

  override def covers(): Seq[FormalProperty] = Seq(
    io.db.isStall && io.db.cmd.write,
    io.db.isStall && !io.db.cmd.write,
    io.db.fire && io.db.cmd.write,
    io.db.fire && !io.db.cmd.write,
  )

  override def formalComponentProperties(): Seq[FormalProperty] = new FormalProperties(this) {
    addFormalProperty(expectingRead.asUInt === io.pmb.contract.outstandingReads)

    when(cmdLatch) {
      addFormalProperty(!expectingRead)
      addFormalProperty(io.db.valid)
    }

    when(expectingRead) {
      addFormalProperty(io.db.valid && (io.db.cmd.write === False))
    }

    addFormalProperty(io.db.readFire === io.pmb.rsp.fire)
  }

}

object DirectBus {
  def apply(bus : PipelinedMemoryBus): DirectBus = {
    val busOut = new DirectBus(bus.config)

    bus.cmd.valid := busOut.valid
    bus.cmd.address := busOut.cmd.address
    bus.cmd.write := busOut.cmd.write
    bus.cmd.data := busOut.cmd.data
    bus.cmd.mask := busOut.cmd.mask

    busOut.rsp := bus.rsp.payload.asBits
    busOut.ready := bus.rsp.valid || (bus.cmd.write && bus.cmd.fire)

    busOut
  }

  def toPipelinedMemoryBus(dBus : DirectBus): PipelinedMemoryBus = {
    val converter = DirectBusToPipelinedMemoryBus(dBus.config)
    converter.io.db <> dBus
    converter.io.pmb
  }
}


class DirectBusToPipelinedMemoryBusFormalTest extends AnyFunSuite with FormalTestSuite {
  formalTests().foreach(t => test(t._1) { t._2() })

  override def defaultDepth(): Int = 20

  override def generateRtl() = Seq(
    ("Basic", () => new GeneralFormalDut(() => new DirectBusToPipelinedMemoryBus(PipelinedMemoryBusConfig(32, 32))))
  )
}