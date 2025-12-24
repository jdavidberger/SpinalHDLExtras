package spinalextras.lib.soc.peripherals

import spinal.core._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.com.spi.ddr.{Apb3SpiXdrMasterCtrl, SpiXdrMasterCtrl, SpiXdrParameter}
import spinal.lib.com.spi.ddr.SpiXdrMasterCtrl.MemoryMappingParameters
import spinalextras.lib.Constraints
import spinalextras.lib.io.TristateBuffer
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
                          name : String = "spiflash") extends SpinexPlugin {

  lazy val spiflash_clk = out(Bool())
  lazy val spiflash_cs_n = out(Bool())
  lazy val spiflash_dq = inout(Analog(Bits(config.ctrl.spi.dataWidth bits)))

  override def apply(som: Spinex): Unit = {
    val ctrl = Apb3SpiXdrMasterCtrl(config)

    som.system.apbMapping += ctrl.io.apb     -> registerMapping

    som.add_slave(ctrl.io.xip, "xip", memoryMapping, "iBus", "dBus")

    val buffers = ctrl.io.spi.data.map(_ => TristateBuffer())

    for(i <- spiflash_dq.bitsRange) {
      val (phy, tristate, xdr) = (spiflash_dq(i), buffers(i), ctrl.io.spi.data(i))
      tristate.io.output_enable := xdr.writeEnable
      tristate.io.input := xdr.write(0)
      xdr.read(0) := RegNext(RegNext(tristate.io.output))
      tristate.io.phy <> phy
    }

    spiflash_clk := ctrl.io.spi.sclk.write(0)
    spiflash_cs_n := ctrl.io.spi.ss(0)

    som.io.valCallbackRec(spiflash_clk, s"${name}_clk")
    som.io.valCallbackRec(spiflash_cs_n, s"${name}_cs_n")
    som.io.valCallbackRec(spiflash_dq, s"${name}_dq")

    Constraints.create_clock(spiflash_clk, ClockDomain.current.frequency.getValue)
    Constraints.set_max_skew(1 ns, spiflash_clk, spiflash_cs_n, spiflash_dq)
  }
}