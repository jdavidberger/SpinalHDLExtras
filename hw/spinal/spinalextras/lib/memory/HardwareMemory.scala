package spinalextras.lib.memory

import spinal.core._
import spinal.lib._
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusConfig, PipelinedMemoryBusRsp}
import spinalextras.lib.bus.{PipelinedMemoryBusCmdExt, PipelinedMemoryBusConfigExt}
import spinalextras.lib.formal.ComponentWithFormalProperties
import spinalextras.lib.memory.HardwareMemory.{HardwareMemoryReadPort, HardwareMemoryReadWritePort, HardwareMemoryWritePort}
import spinalextras.lib.testing.test_funcs

case class HardwareMemoryReadWriteConfig(addressWidth : Int, dataWidth : Int) {
  def wordAddressShift = log2Up((dataWidth / 8.0).ceil.toInt)
  implicit def toPipelinedMemoryBusConfig: PipelinedMemoryBusConfig = PipelinedMemoryBusConfig(addressWidth + wordAddressShift, dataWidth)
}
object HardwareMemoryReadWriteConfig {
  def apply(cfg: PipelinedMemoryBusConfig): HardwareMemoryReadWriteConfig = {
    HardwareMemoryReadWriteConfig(cfg.addressWidth - cfg.wordAddressShift, cfg.dataWidth)
  }
  implicit def hwReadWriteConfig2PMBConfig(value : HardwareMemoryReadWriteConfig): PipelinedMemoryBusConfig = value.toPipelinedMemoryBusConfig
}

abstract class HardwareMemory[T <: Data]() extends ComponentWithFormalProperties {
  def requirements: MemoryRequirement[T] = ???

  def summary(depth: Int = 0): Unit = {
    println(("\t" * depth) + s"memory_${getClass.getSimpleName}_${requirements.num_elements}x${requirements.dataType.getBitsWidth}")
  }

  def asType[T1 <: Data](dataType: HardType[T1]): HardwareMemory[T1] = {
    new CastHardwareMemory[T, T1](this, dataType)
  }

  lazy val latency: Int = 1
  lazy val cmd_latency: Int = 0

  lazy val num_elements = requirements.num_elements
  lazy val dataType = requirements.dataType

  lazy val actual_num_elements = num_elements

  lazy val config = HardwareMemoryReadWriteConfig(log2Up(num_elements), dataWidth = requirements.dataType.getBitsWidth)

  def bitsWidth = requirements.dataType.getBitsWidth

  var self = this

  lazy val io = new Bundle {
    val readWritePorts = Array.fill(requirements.numReadWritePorts)(slave((HardwareMemoryReadWritePort(config))))
    val readPorts = Array.fill(requirements.numReadPorts)(slave((HardwareMemoryReadPort(config))))
    val writePorts = Array.fill(requirements.numWritePorts)(slave((HardwareMemoryWritePort(config))))

    val readWritePortsOutstanding = readWritePorts.map(p => new Composite(p, s"readWrite") {
      val counter = new CounterUpDown(latency + 1, handleOverflow = false)
      test_funcs.assertCounter(counter)
      when(p.cmd.fire && !p.cmd.write) {
        counter.increment()
      }
      when(p.rsp.fire) {
        counter.decrement()
      }
      val v = out(cloneOf(counter.value))
      v := counter.value
    }.v)

    val readPortsOutstanding = readPorts.map(p => new Composite(p, "readPort") {
      val counter = new CounterUpDown(latency + 1, handleOverflow = false)
      test_funcs.assertCounter(counter)
      when(p.cmd.fire) {
        counter.increment()
      }
      when(p.rsp.fire) {
        counter.decrement()
      }
      val v = out(cloneOf(counter.value))
      v := counter.value
    }.v)
  }

  def rsps = {
    io.readWritePorts.map(_.rsp) ++ io.readPorts.map(_.rsp)
  }

  def setIdle(): Unit = {
    io.readWritePorts.foreach(_.setIdle())
    io.readPorts.foreach(_.setIdle())
    io.writePorts.foreach(_.setIdle())
  }

  private lazy val _pmbs = {
    io.readPorts.zip(io.writePorts).map(read_write => {
      val (read, write) = read_write
      val pmb = PipelinedMemoryBus(config)
      pmb.cmd.ready := True

      read.cmd.valid := pmb.cmd.valid && !pmb.cmd.write
      read.cmd.payload := pmb.cmd.payload.wordAddress

      pmb.rsp.valid := read.rsp.valid
      pmb.rsp.data := read.rsp.data

      write.cmd.valid := pmb.cmd.valid && pmb.cmd.write
      write.cmd.address := pmb.cmd.payload.wordAddress
      if (write.cmd.mask != null)
        write.cmd.mask := pmb.cmd.mask
      write.cmd.payload.data := pmb.cmd.data

      pmb
    }) ++
      io.readWritePorts.map(read_write => {
        val pmb = PipelinedMemoryBus(config)
        pmb.cmd.ready := True

        pmb.rsp.valid := read_write.rsp.valid
        pmb.rsp.data := read_write.rsp.data

        read_write.cmd.write := pmb.cmd.write
        read_write.cmd.valid := pmb.cmd.valid
        read_write.cmd.address := pmb.cmd.payload.wordAddress
        if (read_write.cmd.mask != null)
          read_write.cmd.mask := pmb.cmd.mask
        read_write.cmd.payload.data := pmb.cmd.data

        pmb
      })
  }

  def pmbs(): Seq[PipelinedMemoryBus] = _pmbs
}

object HardwareMemory {

  case class HardwareMemoryReadWriteCmd(config: HardwareMemoryReadWriteConfig) extends Bundle {
    val write = Bool()
    val address = UInt(config.addressWidth bits)
    val data = Bits(config.dataWidth bits)
    val mask = (config.dataWidth % 8 == 0) generate (Bits(config.dataWidth / 8 bit))
  }

  case class HardwareMemoryReadWritePort(config: HardwareMemoryReadWriteConfig) extends Bundle with IMasterSlave {
    val cmd = Flow(HardwareMemoryReadWriteCmd(config))
    val rsp = Flow(PipelinedMemoryBusRsp(config))

    def readFire = cmd.fire && !cmd.write

    def setIdle(): Unit = {
      cmd.setIdle()
    }

    override def asMaster(): Unit = {
      master(cmd)
      slave(rsp)
    }

    def <<(rw: MemReadWrite): Unit = {
      cmd.write.assignFrom(rw.writeEnable)
      cmd.valid := rw.clockDomain.isClockEnableActive
      cmd.address.assignFrom(rw.address)
      cmd.payload.assignFrom(rw.data)
      cmd.mask.assignFrom(rw.mask)

      rsp.valid := RegNext(!cmd.write && cmd.fire, init = False)
      rsp.data.assignFrom(rw)
    }
  }

  case class HardwareMemoryReadPort(config: HardwareMemoryReadWriteConfig) extends Bundle with IMasterSlave {
    val cmd = Flow(UInt(config.addressWidth bits))
    val rsp = Flow(PipelinedMemoryBusRsp(config))

    def setIdle(): Unit = {
      cmd.setIdle()
    }

    override def asMaster(): Unit = {
      master(cmd)
      slave(rsp)
    }
  }

  case class HardwareMemoryWriteCmd(config: HardwareMemoryReadWriteConfig) extends Bundle {
    val address = UInt(config.addressWidth bits)
    val data = Bits(config.dataWidth bits)
    val mask = (config.dataWidth % 8 == 0) generate (Bits(config.dataWidth / 8 bit))
  }

  case class HardwareMemoryWritePort(config: HardwareMemoryReadWriteConfig) extends Bundle with IMasterSlave {
    val cmd = Flow(HardwareMemoryWriteCmd(config))

    def setIdle(): Unit = {
      cmd.setIdle()
    }

    override def asMaster(): Unit = {
      master(cmd)
    }
  }

}