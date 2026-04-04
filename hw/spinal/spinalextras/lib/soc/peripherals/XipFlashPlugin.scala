package spinalextras.lib.soc.peripherals

import spinal.core._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.com.spi.ddr.{Apb3SpiXdrMasterCtrl, SpiXdrMasterCtrl, SpiXdrParameter}
import spinal.lib.com.spi.ddr.SpiXdrMasterCtrl.{MemoryMappingParameters, XipBus}
import spinalextras.lib.Constraints
import spinalextras.lib.io.TristateBuffer
import spinalextras.lib.misc.GlobalSignals
import spinalextras.lib.soc.spinex.{Spinex, SpinexPlugin}

import scala.language.postfixOps

object XipFlashPlugin {
  val defaultConfig = SpiXdrMasterCtrl.MemoryMappingParameters(
    SpiXdrMasterCtrl.Parameters(8, 12, SpiXdrParameter(
        dataWidth = 4,
        ioRate = 1,
        ssWidth = 1))
      .addFullDuplex(id = 0)
      .addHalfDuplex(id = 1, rate = 1, ddr = false, spiWidth = 4, lateSampling = false),
    cmdFifoDepth = 32,
    rspFifoDepth = 32,
    xipEnableInit = true,

    modInit = 0,
    xipInstructionModInit = 0,
    xipAddressModInit  = 0,
    xipDummyModInit  = 0,
    xipPayloadModInit  = 1,
    xipInstructionDataInit = 0x6B,
    xipDummyDataInit = 0xa5,

    //xipConfigWritable = false,

    xip = SpiXdrMasterCtrl.XipBusParameters(addressWidth = 24, lengthWidth = 5)
  )
}
case class XipFlashPlugin(config: MemoryMappingParameters = XipFlashPlugin.defaultConfig,
                          memoryMapping : SizeMapping = SizeMapping(0x20000000L, 0x01000000),
                          registerMapping : SizeMapping = SizeMapping(0x01000, 1 KiB),
                          clockDomain : ClockDomain = ClockDomain.current,
                          name : String = "spiflash") extends SpinexPlugin {

  lazy val spiflash_clk = out(Bool())
  lazy val spiflash_cs_n = out(Bool())
  lazy val spiflash_dq = inout(Analog(Bits(config.ctrl.spi.dataWidth bits)))

  override def apply(som: Spinex): Unit = {
    val systemClockDomain = ClockDomain.current

    som.io.valCallbackRec(spiflash_clk, s"${name}_clk")
    som.io.valCallbackRec(spiflash_cs_n, s"${name}_cs_n")
    som.io.valCallbackRec(spiflash_dq, s"${name}_dq")

    val clockArea = new ClockingArea(if (clockDomain == null) ClockDomain.current else clockDomain) {
      val ctrl = Apb3SpiXdrMasterCtrl(config)

      som.add_peripheral(ctrl.io.apb, registerMapping)

      val buffers = ctrl.io.spi.data.map(_ => TristateBuffer())

      for (i <- spiflash_dq.bitsRange) {
        val (phy, tristate, xdr) = (spiflash_dq(i), buffers(i), ctrl.io.spi.data(i))
        tristate.io.output_enable := xdr.writeEnable
        tristate.io.input := xdr.write(0)
        xdr.read(0) := RegNext(RegNext(tristate.io.output))
        tristate.io.phy <> phy
      }

      spiflash_clk := ctrl.io.spi.sclk.write(0)
      spiflash_cs_n := ctrl.io.spi.ss(0)

    }
    val xip = if (systemClockDomain == clockArea.clockDomain) clockArea.ctrl.io.xip else {
      val flashClockDomain = clockArea.clockDomain
      val ref_xip = clockArea.ctrl.io.xip
      val cc_xip = new XipBus(ref_xip.p)
      cc_xip.cmd.queue(4, systemClockDomain, flashClockDomain) >> ref_xip.cmd
      ref_xip.rsp.ccToggle(flashClockDomain, systemClockDomain) >> cc_xip.rsp
      cc_xip
    }
    som.add_slave(xip, "xip", memoryMapping, "iBus", "dBus")

    //Constraints.create_clock(spiflash_clk, ClockDomain.current.frequency.getValue)
    //Constraints.set_max_skew(0.1 ns, spiflash_clk, spiflash_cs_n, spiflash_dq)

    def spread(d: Data) = {
      (0 to d.getBitsWidth).map(idx => s"${d.getRtlPath()}[${idx}]").mkString(" ")
    }

//    Constraints.add_verbatim(
//      s"""
//        |create_generated_clock -name ${spiflash_clk.name} -source [get_pins {${source_clock.getRtlPath()}}] -divide_by 2 [get_nets {${spiflash_clk.name}}]
//        |
//        |set_input_delay -clock [get_clocks ${spiflash_clk.name}] -clock_fall -min 1.5 [get_ports {${spread(spiflash_dq)}}]
//        |set_input_delay -clock [get_clocks ${spiflash_clk.name}] -clock_fall -max 6.0 [get_ports {${spread(spiflash_dq)}}]
//        |
//        |set_output_delay -clock [get_clocks ${spiflash_clk.name}] -max 2.0 [get_ports {${spread(spiflash_dq)}}]
//        |set_output_delay -clock [get_clocks ${spiflash_clk.name}] -min -3.0 [get_ports {${spread(spiflash_dq)}}]
//        |set_output_delay -clock [get_clocks ${spiflash_clk.name}] -max 5.0 [get_ports {${spiflash_cs_n.getRtlPath()}}]
//        |set_output_delay -clock [get_clocks ${spiflash_clk.name}] -min -3.0 [get_ports {${spiflash_cs_n.getRtlPath()}}]
//        |""".stripMargin)
  }
}