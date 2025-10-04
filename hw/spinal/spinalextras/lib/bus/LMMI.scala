package spinalextras.lib.bus

import spinal.core._
import spinal.lib._
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusConfig}
import spinal.lib.{Flow, IMasterSlave, master, slave, traversableOncePimped}

import scala.language.postfixOps

case class LMMIConfig(addressWidth : Int, dataWidth : Int) {}

case class LMMICmd(config : LMMIConfig) extends Bundle{
  val write = Bool()
  val offset = UInt(config.addressWidth bits)
  val data = Bits(config.dataWidth bits)
}

object LMMI {
  def apply(config: LMMIConfig): LMMI = new LMMI(config)

  def apply(addressWidth: Int, dataWidth: Int): LMMI = LMMI(LMMIConfig(addressWidth, dataWidth))
}

class LMMI(config: LMMIConfig) extends Bundle with IMasterSlave {
  val cmd = Stream(LMMICmd(config))
  val rsp = Flow(Bits(config.dataWidth bits))

  if(!globalData.config.formalAsserts) {
    cmd.valid.setName(s"request")
    cmd.offset.setName(s"offset")
    cmd.write.setName(s"wr_rdn")
    cmd.data.setName(s"wdata")

    cmd.ready.setName(s"ready")

    rsp.valid.setName(s"rdata_valid")
    rsp.payload.setName(s"rdata")
  }

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }

  def toPipelinedMemoryBus() : PipelinedMemoryBus = {
    val pmb = PipelinedMemoryBus(PipelinedMemoryBusConfig(config.addressWidth + log2Up(config.dataWidth / 8), config.dataWidth))
    pmb.cmd.map( c => {
      val cmd = cloneOf(this.cmd.payload)
      cmd.offset := c.wordAddress
      cmd.write := c.write
      cmd.data := c.data
      cmd
    }) >> cmd

    pmb.rsp.valid := rsp.valid
    pmb.rsp.payload.data := rsp.payload

    pmb
  }

  def drivePipelinedMemoryBus() : PipelinedMemoryBus = {
    val pmb = PipelinedMemoryBus(PipelinedMemoryBusConfig(config.addressWidth + log2Up(config.dataWidth / 8), config.dataWidth))

    pmb.cmd << cmd.map( c => {
      val cmd = cloneOf(pmb.cmd.payload)
      cmd.assignWordAddress(c.offset)
      cmd.write := c.write
      cmd.data := c.data
      cmd
    })

    rsp << pmb.rsp.map(_.data)

    pmb
  }
}