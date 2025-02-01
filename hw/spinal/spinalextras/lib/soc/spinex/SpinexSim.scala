package spinalextras.lib.soc.spinex

import spinal.core.{ClockDomain, Component, Device, False, IntToBuilder, RegInit, True}
import spinal.lib.Timeout
import spinalextras.lib.Config
import spinalextras.lib.blackbox.lattice.lifcl.{OSCD, OSCDConfig}
import spinalextras.lib.blackbox.memories.W25Q128JVxIM_quad
import spinalextras.lib.misc.ClockSpecification

case class SpinexSim(memoryFile : String = "") extends Component {

  ClockDomain.push(ClockDomain.current.withBootReset())

  val oscd = new OSCD(OSCDConfig.create(ClockSpecification(80 MHz)))
  ClockDomain.push(oscd.hf_clk().get.withBootReset())

  val resetTimer = Timeout(10)
  val reset = RegInit(True) clearWhen resetTimer
  ClockDomain.push(oscd.hf_clk().get.copy(reset = reset))

  val som = new Spinex()
  val flash = new W25Q128JVxIM_quad(memoryFile)
  flash.io.spiflash_dq <> som.io.spiflash_dq
  flash.io.spiflash_cs_n <> som.io.spiflash_cs_n
  flash.io.spiflash_clk <> som.io.spiflash_clk

  som.io.jtag.tdi := False
  som.io.jtag.tms := False
  som.io.jtag.tck := False

  som.io.uart.rxd := True
  som.io.wb.DAT_MISO.clearAll()
  som.io.wb.ACK := False
  som.io.wb.ERR := False
}

object SpinexSim {
  def main(args: Array[String]) {
    Config.spinal.copy(device = Device("lattice", "lifcl")).generateVerilog(SpinexSim())
  }
}