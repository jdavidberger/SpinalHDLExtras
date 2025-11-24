package spinalextras.lib.soc.spinex.plugins

import spinal.core.{Analog, Bool, IntToBuilder, inout, when}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.com.i2c.{I2cSlaveGenerics, I2cSlaveIo, I2cSlaveMemoryMappedGenerics}
import spinal.lib.io.{InOutWrapper, ReadableOpenDrain, TriState}
import spinal.lib.{master, slave}
import spinalextras.lib.blackbox.opencores.i2c_master_top
import spinalextras.lib.soc.DeviceTree
import spinalextras.lib.soc.peripherals.{I2cOpencoresCompat, SpinexI2cCtrl}
import spinalextras.lib.soc.spinex.{Spinex, SpinexRegisterFilePlugin}

import scala.language.postfixOps

case class I2CPlugin(mapping: SizeMapping = SizeMapping(0xe0005000L, 32 Bytes), name: String = "i2c0") extends SpinexRegisterFilePlugin(name, mapping) {
//  lazy val scl = inout(Analog(Bool()))
//  lazy val sda = inout(Analog(Bool()))
  lazy val sda = slave (ReadableOpenDrain(Bool()))
  lazy val scl = slave (ReadableOpenDrain(Bool()))

  override val compatible: Seq[String] = Seq("spinex,i2c", "opencores,i2c")

  override def apply(som: Spinex): Unit = {
    val io = new I2cSlaveIo(I2cSlaveGenerics())

    I2cOpencoresCompat.driveI2cSlaveIo(
      io,
      busCtrl = som.interconnect.add_slave_factory(name, mapping, false, false),
      0
    )(I2cSlaveMemoryMappedGenerics(io.g))

    sda <> io.i2c.sda
    scl <> io.i2c.scl

    som.io.valCallback(scl, s"${name}_scl")
    som.io.valCallback(sda, s"${name}_sda")
  }

  override def appendDeviceTree(dt: DeviceTree): Unit = {
    super.appendDeviceTree(dt)
    Seq(f"clock-frequency = <I2C_BITRATE_STANDARD>;", "rtl-version = <1>;").foreach({
      dt.addEntry(_, baseEntryPath: _*)
    })
  }
}
