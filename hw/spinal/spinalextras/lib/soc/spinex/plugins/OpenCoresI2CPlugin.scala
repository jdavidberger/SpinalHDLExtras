package spinalextras.lib.soc.spinex.plugins

import spinal.core.{Analog, Bool, CombInit, IntToBuilder, inout}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.wishbone.Wishbone
import spinalextras.lib.blackbox.opencores.i2c_master_top
import spinalextras.lib.soc.DeviceTree
import spinalextras.lib.soc.spinex.{Spinex, SpinexRegisterFilePlugin}

import scala.language.postfixOps

case class OpenCoresI2CPlugin(mapping: SizeMapping = SizeMapping(0xe0005000L, 32 Bytes), name: String = "i2c0") extends SpinexRegisterFilePlugin(name, mapping) {
  lazy val scl = inout(Analog(Bool()))
  lazy val sda = inout(Analog(Bool()))

  override val compatible: Seq[String] = Seq("spinex,i2c", "opencores,i2c")

  override def apply(som: Spinex): Unit = {
    val i2cCtrl = new i2c_master_top()
    i2cCtrl.attachi2c(scl, sda)

    som.io.valCallback(scl, s"${name}_scl")
    som.io.valCallback(sda, s"${name}_sda")

    som.system.addInterrupt(CombInit(i2cCtrl.io.wb_inta_o).setName("i2c_int", weak = true), 3)

    val wb32 = new Wishbone(i2cCtrl.io.wb.config.copy(dataWidth = 32))
    wb32.connectTo(i2cCtrl.io.wb, true, true)

    som.add_slave(wb32, name, mapping, "dBus")
  }

  override def appendDeviceTree(dt: DeviceTree): Unit = {
    super.appendDeviceTree(dt)
    Seq(f"clock-frequency = <I2C_BITRATE_STANDARD>;", "rtl-version = <1>;").foreach({
      dt.addEntry(_, baseEntryPath: _*)
    })
  }
}
