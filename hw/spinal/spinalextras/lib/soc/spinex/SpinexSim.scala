package spinalextras.lib.soc.spinex

import spinal.core.{ClockDomain, Component, Device, False, IntToBuilder, RegInit, SystemVerilog, True}
import spinal.lib.Timeout
import spinalextras.lib.Config
import spinalextras.lib.blackbox.lattice.lifcl.{GSR, OSCD, OSCDConfig}
import spinalextras.lib.blackbox.memories.W25Q128JVxIM_quad
import spinalextras.lib.misc.ClockSpecification
import spinalextras.lib.soc.peripherals.{UartCtrlPlugin, XipFlashPlugin}
import spinalextras.lib.soc.spinex.plugins.JTagPlugin

case class SpinexSim(memoryFile : String = "") extends Component {

  ClockDomain.push(ClockDomain.current.withBootReset())

  GSR.no_op()

  val oscd = new OSCD(OSCDConfig.create(ClockSpecification(80 MHz)))
  ClockDomain.push(oscd.hf_clk().get.withBootReset())

  val resetTimer = Timeout(10)
  val reset = RegInit(True) clearWhen resetTimer
  ClockDomain.push(oscd.hf_clk().get.copy(reset = reset))

  val som = new Spinex(SpinexConfig.default)
  val flash = new W25Q128JVxIM_quad(memoryFile)

  val flashPlugin = som.getPlugin[XipFlashPlugin].get
  flash.io.spiflash_dq <> flashPlugin.spiflash_dq
  flash.io.spiflash_cs_n <> flashPlugin.spiflash_cs_n
  flash.io.spiflash_clk <> flashPlugin.spiflash_clk

  som.getPlugin[JTagPlugin].foreach(j => {
    j.jtag.tdi := False
    j.jtag.tms := False
    j.jtag.tck := False
  })

  som.getPlugin[UartCtrlPlugin].foreach(_.uart.rxd := True)
//  som.io.wb.DAT_MISO.clearAll()
//  som.io.wb.ACK := False
//  som.io.wb.ERR := False
}

object SpinexSim {
  def main(args: Array[String]) {
    if(args.isEmpty) {
      System.err.println("Usage: SpinexSim <flash-image.hex> [--lattice-lifcl]")
      System.err.println("The flash image hex is a readmemh form of the firmware file.")
      System.err.println("--lattice-lifcl dictates whether this is made as a generic simulation or one specifically for the LIFCL chipset")
      System.exit(-1)
    }
    var config = Config.spinal
    if(args.contains("--lattice-lifcl")) {
      config = config.copy(device = Device("lattice", "lifcl"))
    }
    config.generateSystemVerilog(SpinexSim(args(0)))
  }
}