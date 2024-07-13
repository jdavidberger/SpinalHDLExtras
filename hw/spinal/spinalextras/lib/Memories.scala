package spinalextras.lib

import spinal.core._
import spinal.core.fiber.Handle.initImplicit
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusCmd, PipelinedMemoryBusConfig, PipelinedMemoryBusRsp}
import spinalextras.lib.HardwareMemory._
import spinalextras.lib.blackbox.lattice.lifcl.{DPSC512K_Mem, PDPSC512K_Mem}
import spinalextras.lib.impl.ImplementationSpecificFactory
import spinalextras.lib.misc.ComponentWithKnownLatency

import scala.language.postfixOps

object HardwareMemory {
  type HardwareMemoryReadWriteConfig = PipelinedMemoryBusConfig

  case class HardwareMemoryReadWriteCmd(config : HardwareMemoryReadWriteConfig) extends Bundle{
    val write = Bool()
    val address = UInt(config.addressWidth bits)
    val data = Bits(config.dataWidth bits)
    val mask = (config.dataWidth % 8 == 0) generate (Bits(config.dataWidth / 8 bit))
  }

  case class HardwareMemoryReadWritePort(config : HardwareMemoryReadWriteConfig) extends Bundle with IMasterSlave {
    val cmd = Flow(HardwareMemoryReadWriteCmd(config))
    val rsp = Flow(PipelinedMemoryBusRsp(config))

    def setIdle(): Unit = {
      cmd.setIdle()
    }

    override def asMaster(): Unit = {
      master(cmd)
      slave(rsp)
    }
  }

  case class HardwareMemoryReadPort(config : HardwareMemoryReadWriteConfig) extends Bundle with IMasterSlave {
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

  case class HardwareMemoryWriteCmd(config : HardwareMemoryReadWriteConfig) extends Bundle{
    val address = UInt(config.addressWidth bits)
    val data = Bits(config.dataWidth bits)
    val mask = (config.dataWidth % 8 == 0) generate (Bits(config.dataWidth / 8 bit))
  }

  case class HardwareMemoryWritePort(config : HardwareMemoryReadWriteConfig) extends Bundle with IMasterSlave {
    val cmd = Flow(HardwareMemoryWriteCmd(config))
    def setIdle(): Unit = {
      cmd.setIdle()
    }
    override def asMaster(): Unit = {
      master(cmd)
    }
  }

}

case class MemoryRequirement[T <: Data](dataType : HardType[T], num_elements : BigInt, numReadWritePorts : Int, numReadPorts : Int, numWritePorts : Int) {
  lazy val allocationSize = dataType.getBitsWidth * num_elements
  lazy val numPorts = numReadWritePorts + numReadPorts + numWritePorts

  override def toString: String = {
    if(dataType.globalData != null) {
      s"${dataType.getBitsWidth}bits_${num_elements}d_${numReadWritePorts}rw_${numReadPorts}r_${numWritePorts}"
    } else {
      s"${num_elements}d_${numReadWritePorts}rw_${numReadPorts}r_${numWritePorts}"
    }

  }
}

class MemoryRequirementBits(dataWidth : Int, num_elements : BigInt, numReadWritePorts : Int, numReadPorts : Int, numWritePorts : Int) extends
  MemoryRequirement(Bits(dataWidth bits), num_elements = num_elements, numReadWritePorts = numReadWritePorts, numReadPorts = numReadPorts, numWritePorts = numWritePorts) {

  override def toString: String =
    s"${dataWidth}bits_${num_elements}d_${numReadWritePorts}rw_${numReadPorts}r_${numWritePorts}"
}

abstract class HardwareMemory[T <: Data]() extends Component {
  def requirements : MemoryRequirement[T] = ???
  lazy val num_elements = requirements.num_elements
  lazy val dataType = requirements.dataType

  lazy val config = PipelinedMemoryBusConfig(log2Up(num_elements), dataWidth = requirements.dataType.getBitsWidth)
  def bitsWidth = requirements.dataType.getBitsWidth

  lazy val io = new Bundle {
    val readWritePorts = Array.fill(requirements.numReadWritePorts)(slave((HardwareMemoryReadWritePort(config))))
    val readPorts = Array.fill(requirements.numReadPorts)(slave((HardwareMemoryReadPort(config))))
    val writePorts = Array.fill(requirements.numWritePorts)(slave((HardwareMemoryWritePort(config))))
  }

  def setIdle(): Unit = {
    io.readWritePorts.foreach(_.setIdle())
    io.readPorts.foreach(_.setIdle())
    io.writePorts.foreach(_.setIdle())
  }

  def pmbs(): Seq[PipelinedMemoryBus] = {
    io.readPorts.zip(io.writePorts).map(read_write => {
      val (read, write) = read_write
      val pmb = PipelinedMemoryBus(config)
      pmb.cmd.ready := True

      read.cmd.valid := pmb.cmd.valid && !pmb.cmd.write
      read.cmd.payload := pmb.cmd.address

      pmb.rsp.valid := read.rsp.valid
      pmb.rsp.data := read.rsp.data

      write.cmd.valid := pmb.cmd.valid && pmb.cmd.write
      write.cmd.address := pmb.cmd.address
      if(write.cmd.mask != null)
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
        read_write.cmd.address := pmb.cmd.address
        if(read_write.cmd.mask != null)
          read_write.cmd.mask := pmb.cmd.mask
        read_write.cmd.payload.data := pmb.cmd.data

        pmb
      })
  }
}

class WideHardwareMemory[T <: Data] (reqs : MemoryRequirement[T], direct_factory: () => HardwareMemory[Bits]) extends HardwareMemory[T]() {
  val prototype = direct_factory()

  override def requirements = reqs.copy(num_elements = prototype.num_elements)

  val needed_cols = (dataType.getBitsWidth + prototype.bitsWidth - 1)/ prototype.bitsWidth
  var memories = Array.fill(needed_cols - 1)(direct_factory()) ++ Seq(prototype)
  memories.zipWithIndex.map(x => x._1.setName(s"memories_col_${x._2}"))
  //memories.foreach(_.setIdle())

  io.readPorts.zipWithIndex.foreach(port_idx => {
    val (port, idx) = port_idx

    port.rsp.payload.data.assignFromBits(Vec(memories.map(mem => {
      mem.io.readPorts(idx).cmd << port.cmd
      mem.io.readPorts(idx).rsp.payload.data
    }).toSeq).asBits.resized)

    port.rsp.valid := memories.head.io.readPorts(idx).rsp.valid
  })

  io.writePorts.zipWithIndex.foreach(port_idx => {
    val (port, idx) = port_idx

    val datas = memories.map(mem => {
      mem.io.writePorts(idx).cmd.valid := port.cmd.valid
      mem.io.writePorts(idx).cmd.address := port.cmd.address
      (mem.io.writePorts(idx).cmd.data, mem.io.writePorts(idx).cmd.mask)
    })

    datas.map(_._1).zip(port.cmd.data.subdivideIn(memories.size slices, strict = false)).foreach(d => {
      d._1 := d._2.resized
    })
    if(port.cmd.mask != null) {
      datas.map(_._2).zip(port.cmd.mask.subdivideIn(memories.size slices, strict = false)).foreach(d => {
        d._1 := d._2
      })
    } else {
      datas.map(_._2).foreach(d => {
        d.setAll()
      })
    }
  })

  io.readWritePorts.zipWithIndex.foreach(port_idx => {
    val (port, idx) = port_idx

    val datas = memories.map(mem => {
      mem.io.readWritePorts(idx).cmd.valid := port.cmd.valid
      mem.io.readWritePorts(idx).cmd.write := port.cmd.write
      mem.io.readWritePorts(idx).cmd.address := port.cmd.address

      (mem.io.readWritePorts(idx).cmd.data, mem.io.readWritePorts(idx).rsp.data, mem.io.readWritePorts(idx).cmd.mask)
    })

    port.rsp.valid := memories.head.io.readWritePorts(idx).rsp.valid
    port.rsp.payload.data.assignFromBits(Vec(datas.map(_._2)).asBits.resized)
    datas.map(_._1).zip(port.cmd.data.subdivideIn(memories.size slices, strict = false)).foreach(d => {
      d._1 := d._2.resized
    })


    if(port.cmd.mask != null) {
      datas.map(_._3).zip(port.cmd.mask.subdivideIn(memories.size slices, strict = false)).foreach(d => {
        d._1 := d._2
      })
    } else {
      datas.map(_._3).foreach(d => {
        d.setAll()
      })
    }
  })

}

class StackedHardwareMemory[T <: Data](reqs : MemoryRequirement[T], direct_factory: () => HardwareMemory[Bits]) extends HardwareMemory[T]() {
  override def requirements = reqs
  val wide_factory = () => new WideHardwareMemory(requirements, direct_factory)
  val prototype = wide_factory()
  require(prototype.bitsWidth == dataType.getBitsWidth)
  require(isPow2(prototype.num_elements))

  val needed_rows = ((num_elements + prototype.num_elements - 1) / prototype.num_elements).toInt

  var memories = Array.fill(needed_rows - 1)(wide_factory()) ++ Seq(prototype)
  var mappings = Array.range(0, needed_rows).map(idx => SizeMapping(prototype.num_elements * idx, prototype.num_elements))

  memories.zipWithIndex.map(x => x._1.setName(s"memories_row_${x._2}"))
  memories.foreach(_.setIdle())

  def memories_hits(addr: UInt): (UInt, UInt) = {
//    val hits = Vec(Bool(), mappings.size)
//    for ((memorySpace, hit) <- (mappings, hits).zipped) yield {
//      hit := memorySpace.hit(addr)
//    }
//    OHToUInt(hits)
    val addrWidth = log2Up(prototype.num_elements)
    (addr.resize(addrWidth bits), addr >> (addrWidth))
  }

  io.readPorts.zipWithIndex.foreach(port_idx => {
    val (port, idx) = port_idx
    val (addr, hit_idx) = memories_hits(port.cmd.payload)
    val mem_readPorts = Vec(memories.map(_.io.readPorts(idx)))

    mem_readPorts(hit_idx).cmd.valid := port.cmd.valid
    mem_readPorts(hit_idx).cmd.payload := addr

    port.rsp.valid := False
    port.rsp.payload.assignDontCare()

    memories.foreach(mem => {
      when(mem.io.readPorts(idx).rsp.valid) {
        port.rsp.valid := True
        port.rsp.payload := mem.io.readPorts(idx).rsp.payload
      }
    })
  })

  io.readWritePorts.zipWithIndex.foreach(port_idx => {
    val (port, idx) = port_idx
    val (addr, hit_idx) = memories_hits(port.cmd.payload.address)
    val mem_readWritePorts = Vec(memories.map(_.io.readWritePorts(idx)))

    mem_readWritePorts(hit_idx).cmd.valid := port.cmd.valid
    mem_readWritePorts(hit_idx).cmd.payload.address := addr
    mem_readWritePorts(hit_idx).cmd.payload.write := port.cmd.write
    if(port.cmd.mask != null)
      mem_readWritePorts(hit_idx).cmd.payload.mask := port.cmd.mask

    port.rsp.valid := False
    port.rsp.payload.assignDontCare()

    memories.foreach(mem => {
      when(mem.io.readWritePorts(idx).rsp.valid) {
        port.rsp.valid := True
        port.rsp.payload := mem.io.readWritePorts(idx).rsp.payload
      }
    })
  })

  io.writePorts.zipWithIndex.foreach(port_idx => {
    val (port, idx) = port_idx
    val (addr, hit_idx) = memories_hits(port.cmd.payload.address)
    val mem_writePorts = Vec(memories.map(_.io.writePorts(idx)))

    mem_writePorts(hit_idx).cmd.valid := port.cmd.valid
    mem_writePorts(hit_idx).cmd.payload.address := addr
    if(port.cmd.mask != null)
      mem_writePorts(hit_idx).cmd.payload.mask := port.cmd.mask

  })
}

case class MemBackedHardwardMemory[T <: Data](override val requirements : MemoryRequirement[T]) extends
  HardwareMemory[T]() {
  val mem = Mem[T](dataType, num_elements)
  io.readWritePorts.foreach(port => {
    val mask_width = if(port.cmd.mask != null) port.cmd.mask.getWidth else -1
    val mem_port = mem.readWriteSyncPort(maskWidth = mask_width)
    if(port.cmd.mask != null)
      mem_port.mask := port.cmd.mask
    mem_port.address := port.cmd.address
    mem_port.enable := port.cmd.valid
    mem_port.write := port.cmd.write
    mem_port.wdata.assignFromBits(port.cmd.data)

    port.rsp.valid := RegNext(port.cmd.valid && !port.cmd.write)
    port.rsp.data := mem_port.rdata.asBits
  })

  io.readPorts.foreach(port => {
    port.rsp.data := mem.readSync(
      address = port.cmd.payload,
      enable = port.cmd.valid,
    ).asBits
    port.rsp.valid := RegNext(port.cmd.valid)
  })

  io.writePorts.foreach(port => {
    mem.write(address = port.cmd.address,
      data = port.cmd.data.as(dataType),
      enable = port.cmd.valid,
      mask = port.cmd.mask
    )
  })
}
//
//case class lram(numReadWritePorts : Int, numReadPorts : Int, numWritePorts : Int) extends
//  HardwareMemory[Bits](Bits(32 bits), 128000, numReadWritePorts, numReadPorts, numWritePorts) {
//
//}

object LatticeMemories {
  def find_lram[T <: Data](requirements : MemoryRequirement[T]): Option[()=>HardwareMemory[Bits]] = {
    if(requirements.numPorts > 2) return None

    (requirements.numReadPorts, requirements.numWritePorts, requirements.numReadWritePorts) match {
      case (1, 1, 0) => Some(() => new PDPSC512K_Mem())
      case (0, 0, 1) => Some(() => new DPSC512K_Mem())
      case (0, 0, 2) => Some(() => new DPSC512K_Mem())
    }
  }

  def apply[T <: Data](memKind : MemTechnologyKind)(requirements : MemoryRequirement[T]): HardwareMemory[T] = {
    val allocationSize = requirements.allocationSize

    val shouldUseLRam = memKind.technologyKind.toLowerCase == "lram" || allocationSize > (3 KiB)
    val lram_factory = find_lram(requirements)

    if(false && shouldUseLRam && lram_factory.isDefined) {
      new StackedHardwareMemory(requirements, lram_factory.get)
    } else {
      new MemBackedHardwardMemory[T](requirements)
    }
  }
}

object Memories {
  def factory[T <: Data] = new ImplementationSpecificFactory[HardwareMemory[T], (MemoryRequirement[T], MemTechnologyKind)] {
    simulationHandler = {
      case _ => args => { MemBackedHardwardMemory[T](args._1)
      }
    }
    AddHandler { case Device("lattice", "lifcl", name, resetKind) => { args => LatticeMemories[T](args._2)(args._1) } }
    AddHandler { case _ => args => MemBackedHardwardMemory[T](args._1) }
  }

  def apply[T <: Data](reqs : MemoryRequirement[T], technologyKind: MemTechnologyKind = auto) = factory(reqs, technologyKind)
}

case class PipelinedMemoryBusMemory[T <: Data](reqs : MemoryRequirement[T], technologyKind: MemTechnologyKind = auto,
                                               factory : (MemoryRequirement[T], MemTechnologyKind) => HardwareMemory[T] = Memories.apply _ ) extends Component {
  val mem = factory(reqs, technologyKind)
  val mem_bus = mem.pmbs().head
  val io = new Bundle {
    val bus = slave(cloneOf(mem_bus))
  }
  mem_bus <> io.bus
}