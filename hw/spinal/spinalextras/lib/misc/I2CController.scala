package spinalextras.lib.misc

import spinal.lib._
import spinal.core._
import spinal.lib.bus.misc.{BusSlaveFactory, SizeMapping}
import spinal.lib.com.i2c.{I2c, I2cCtrl, I2cMasterMemoryMappedGenerics, I2cSlave, I2cSlaveGenerics, I2cSlaveIo, I2cSlaveMemoryMappedGenerics}
import spinal.lib.io.TriState
import spinalextras.lib.bus.GlobalBus
import spinalextras.lib.io.TristateBuffer

case class I2CController[T <: IMasterSlave with Nameable with Bundle](globalBus : GlobalBus[T], baseAddress: BigInt) extends Component {
val io =  new Bundle {
  val sda = master (new TriState(Bool()))
  val scl = master (new TriState(Bool()))
}

  val i2c_slave = new I2cSlave(I2cSlaveGenerics())
  i2c_slave.io.i2c.sda.read := io.sda.read
  //i2c_slave.io.i2c.scl.read := io.scl.read
  io.scl.write := i2c_slave.io.i2c.scl.write
  io.sda.write := i2c_slave.io.i2c.sda.write
  io.sda.writeEnable := i2c_slave.io.i2c.sda.write
  io.scl.writeEnable := i2c_slave.io.i2c.scl.write

  val busCtrl = globalBus.add_slave_factory("i2c", SizeMapping(baseAddress, 1 KiB), "cpu")
  i2c_slave.io.driveFrom(busCtrl, baseAddress)(I2cSlaveMemoryMappedGenerics(I2cSlaveGenerics(), 0, I2cMasterMemoryMappedGenerics(32)))
//  def attach_bus[T <: IMasterSlave with Nameable with Bundle](globalBus : GlobalBus[T], baseAddress: BigInt): Unit = {
//    val busCtrl = globalBus.add_slave_factory("i2c", SizeMapping(baseAddress, 1 KiB), "cpu")
//    io.driveFrom(busCtrl, baseAddress)(I2cSlaveMemoryMappedGenerics(io.g, 0, I2cMasterMemoryMappedGenerics(32)))
//  }

}