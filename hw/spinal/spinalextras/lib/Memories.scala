package spinalextras.lib

import spinal.core._
import spinal.core.fiber.Handle.initImplicit
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusCmd, PipelinedMemoryBusConfig, PipelinedMemoryBusRsp}
import spinalextras.lib.HardwareMemory._
import spinalextras.lib.blackbox.lattice.lifcl.{DPSC512K_Mem, PDPSC16K_Mem, PDPSC512K_Mem}
import spinalextras.lib.impl.ImplementationSpecificFactory
import spinalextras.lib.misc.ComponentWithKnownLatency

import scala.collection.mutable
import scala.language.postfixOps
import scala.reflect.ClassTag

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

case class MemoryRequirement[T <: Data](dataType : HardType[T], num_elements : BigInt, numReadWritePorts : Int,
                                        numReadPorts : Int, numWritePorts : Int,
                                        needsMask : Boolean = false,
                                        latencyRange : (Int, Int) = (1, 3)) {
  lazy val allocationSize = dataType.getBitsWidth * num_elements
  lazy val numPorts = numReadWritePorts + numReadPorts + numWritePorts

  override def toString: String = {
    if(dataType.globalData != null) {
      s"${dataType.getBitsWidth}bits_${num_elements}d_${numReadWritePorts}rw_${numReadPorts}r_${numWritePorts}"
    } else {
      s"${num_elements}d_${numReadWritePorts}rw_${numReadPorts}r_${numWritePorts}"
    }
  }

  def asBits = new MemoryRequirement[Bits](Bits(dataType.getBitsWidth bits), num_elements, numReadWritePorts, numReadPorts, numWritePorts, needsMask, latencyRange)
}

class MemoryRequirementBits(dataWidth : Int, num_elements : BigInt, numReadWritePorts : Int, numReadPorts : Int, numWritePorts : Int) extends
  MemoryRequirement(Bits(dataWidth bits), num_elements = num_elements, numReadWritePorts = numReadWritePorts, numReadPorts = numReadPorts, numWritePorts = numWritePorts) {

  override def toString: String =
    s"${dataWidth}bits_${num_elements}d_${numReadWritePorts}rw_${numReadPorts}r_${numWritePorts}"
}

abstract class HardwareMemory[T <: Data]() extends Component {
  def requirements : MemoryRequirement[T] = ???
  lazy val latency : Int = 1
  lazy val cmd_latency : Int = 0

  lazy val num_elements = requirements.num_elements
  lazy val dataType = requirements.dataType

  lazy val actual_num_elements = num_elements

  lazy val config = PipelinedMemoryBusConfig(log2Up(num_elements), dataWidth = requirements.dataType.getBitsWidth)
  def bitsWidth = requirements.dataType.getBitsWidth

  lazy val io = new Bundle {
    val readWritePorts = Array.fill(requirements.numReadWritePorts)(slave((HardwareMemoryReadWritePort(config))))
    val readPorts = Array.fill(requirements.numReadPorts)(slave((HardwareMemoryReadPort(config))))
    val writePorts = Array.fill(requirements.numWritePorts)(slave((HardwareMemoryWritePort(config))))
  }

  def rsps = {
    io.readWritePorts.map(_.rsp) ++ io.readPorts.map(_.rsp)
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

  override lazy val latency : Int = prototype.latency
  override def requirements = reqs

  val mod_width = (dataType.getBitsWidth) % prototype.bitsWidth
  val needed_cols = dataType.getBitsWidth/ prototype.bitsWidth
  lazy val mod_memory = if(mod_width != 0 && (dataType.getBitsWidth > prototype.bitsWidth)) Seq(Memories(reqs.copy(dataType = Bits(mod_width bits), latencyRange = (prototype.latency, prototype.latency) ))) else Seq()
  var memories = Array.fill(needed_cols - 1)(direct_factory()) ++ Seq(prototype) ++ mod_memory
  memories.zipWithIndex.map(x => x._1.setName(s"memories_col_${x._2}_${x._1.actual_num_elements}x${x._1.bitsWidth}_${x._1.getClass.getSimpleName}"))

  override lazy val actual_num_elements = memories.map(_.num_elements).min

  rsps.zipWithIndex.foreach{case (rsp, idx) => {
    rsp.payload.data.assignFromBits(Vec(memories.map(mem => {
      mem.rsps(idx).payload.data
    }).toSeq).asBits.resized)


    rsp.valid := memories.head.rsps(idx).valid
    assert(Vec(memories.map(_.rsps(idx).valid).map(x => x === rsp.valid)).andR, "Read responses not synced")
  }}

  io.readPorts.zipWithIndex.foreach(port_idx => {
    val (port, idx) = port_idx
    memories.map(mem => {
      mem.io.readPorts(idx).cmd.valid := port.cmd.valid
      mem.io.readPorts(idx).cmd.payload := port.cmd.payload.resized
    })
  })

  io.writePorts.zipWithIndex.foreach(port_idx => {
    val (port, idx) = port_idx

    val datas = memories.map(mem => {
      mem.io.writePorts(idx).cmd.valid := port.cmd.valid
      mem.io.writePorts(idx).cmd.address := port.cmd.address.resized
      (mem.io.writePorts(idx).cmd.data, mem.io.writePorts(idx).cmd.mask)
    })

    datas.map(_._1).zip(port.cmd.data.subdivideIn(prototype.bitsWidth bits, strict = false)).foreach(d => {
      d._1 := d._2.resized
    })
    if(port.cmd.mask != null) {
      datas.map(_._2).zip(port.cmd.mask.subdivideIn(prototype.bitsWidth/8 bits, strict = false)).foreach(d => {
        d._1 := d._2
      })
    } else {
      datas.map(_._2).foreach(d => {
        if(d != null)
          d.setAll()
      })
    }
  })

  io.readWritePorts.zipWithIndex.foreach(port_idx => {
    val (port, idx) = port_idx

    val datas = memories.map(mem => {
      mem.io.readWritePorts(idx).cmd.valid := port.cmd.valid
      mem.io.readWritePorts(idx).cmd.write := port.cmd.write
      mem.io.readWritePorts(idx).cmd.address := port.cmd.address.resized

      (mem.io.readWritePorts(idx).cmd.data, mem.io.readWritePorts(idx).rsp.data, mem.io.readWritePorts(idx).cmd.mask)
    })

    datas.map(_._1).zip(port.cmd.data.subdivideIn(prototype.bitsWidth bits, strict = false)).foreach(d => {
      d._1 := d._2.resized
    })


    if(port.cmd.mask != null) {
      datas.map(_._3).zip(port.cmd.mask.subdivideIn((prototype.bitsWidth/8) bits, strict = false)).foreach(d => {
        d._1 := d._2
      })
    } else {
      datas.map(_._3).foreach(d => {
        if(d != null)
          d.setAll()
      })
    }
  })

}

class StackedHardwareMemory[T <: Data](reqs : MemoryRequirement[T], direct_factory: () => HardwareMemory[Bits]) extends HardwareMemory[T]() {
  override def requirements = reqs
  val wide_factory = () => new WideHardwareMemory(requirements, direct_factory)
  val prototype = wide_factory()

  override lazy val latency : Int = prototype.latency
  require(prototype.bitsWidth == dataType.getBitsWidth)
  require(isPow2(prototype.num_elements) || prototype.num_elements == reqs.num_elements)

  val needed_rows = ((num_elements + prototype.num_elements - 1) / prototype.num_elements).toInt
  var memories = Array.fill(needed_rows - 1)(wide_factory()) ++ Seq(prototype)

  var mappings = Array.range(0, needed_rows).map(idx => SizeMapping(prototype.num_elements * idx, prototype.num_elements))

  override lazy val actual_num_elements = memories.map(_.actual_num_elements).sum

  if(actual_num_elements > num_elements) {
    SpinalWarning(s"Memory type underutilized -- total space is ${actual_num_elements}x${bitsWidth}; ${num_elements}x${bitsWidth} are used.")
  }

  memories.zipWithIndex.map(x => x._1.setName(s"memories_row_${x._2}_${x._1.actual_num_elements}x${x._1.bitsWidth}"))
  memories.foreach(x => println(s"${x.name}"))
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

  rsps.zipWithIndex.foreach{case (rsp, idx) => {
    rsp.valid := False
    rsp.payload.assignDontCare()

    memories.foreach(mem => {
      when(mem.rsps(idx).valid) {
        rsp.valid := True
        rsp.payload := mem.rsps(idx).payload
      }
    })
  }}

  io.readPorts.zipWithIndex.foreach(port_idx => {
    val (port, idx) = port_idx
    val (addr, hit_idx) = memories_hits(port.cmd.payload)
    val mem_readPorts = Vec(memories.map(_.io.readPorts(idx)))

    mem_readPorts(hit_idx).cmd.valid := port.cmd.valid
    mem_readPorts(hit_idx).cmd.payload := addr
  })

  io.readWritePorts.zipWithIndex.foreach(port_idx => {
    val (port, idx) = port_idx
    val (addr, hit_idx) = memories_hits(port.cmd.payload.address)
    val mem_readWritePorts = Vec(memories.map(_.io.readWritePorts(idx)))

    mem_readWritePorts(hit_idx).cmd.valid := port.cmd.valid
    mem_readWritePorts(hit_idx).cmd.payload.address := addr
    mem_readWritePorts(hit_idx).cmd.payload.data := port.cmd.data
    mem_readWritePorts(hit_idx).cmd.payload.write := port.cmd.write
    if(port.cmd.mask != null)
      mem_readWritePorts(hit_idx).cmd.payload.mask := port.cmd.mask
  })

  io.writePorts.zipWithIndex.foreach(port_idx => {
    val (port, idx) = port_idx
    val (addr, hit_idx) = memories_hits(port.cmd.payload.address)
    val mem_writePorts = Vec(memories.map(_.io.writePorts(idx)))

    mem_writePorts(hit_idx).cmd.valid := port.cmd.valid
    mem_writePorts(hit_idx).cmd.payload.address := addr
    mem_writePorts(hit_idx).cmd.payload.data := port.cmd.data
    if(port.cmd.mask != null)
      mem_writePorts(hit_idx).cmd.payload.mask := port.cmd.mask

  })
}

case class MemBackedHardwardMemory[T <: Data](override val requirements : MemoryRequirement[T]) extends
  HardwareMemory[T]() {
  val mem = Mem[T](dataType, num_elements)

  override lazy val latency = requirements.latencyRange._2
  val extra_latency = latency - 1

  if (globalData.config.flags.contains(GenerationFlags.simulation)) {
    //mem.randBoot()
    mem.init((0 until num_elements.toInt).map(idx => B(0).as(dataType) ))
  }

  io.readWritePorts.foreach(port => {
    val mask_width = if(port.cmd.mask != null) port.cmd.mask.getWidth else -1
    val mem_port = mem.readWriteSyncPort(maskWidth = mask_width)
    if(port.cmd.mask != null)
      mem_port.mask := port.cmd.mask
    mem_port.address := port.cmd.address

    val invalid_address = port.cmd.address.resize(log2Up(num_elements + 1) bits) < num_elements
    when(~invalid_address) {
      report(Seq(s"Invalid access in range 0x${num_elements.toString(16)} ", port.cmd.address.resize(log2Up(num_elements + 1) bits)))
      assert(~invalid_address, "address overrun")
    }
    mem_port.enable := port.cmd.valid
    mem_port.write := port.cmd.write
    mem_port.wdata.assignFromBits(port.cmd.data)

    var rspFlow = cloneOf(port.rsp)
    rspFlow.valid := RegNext(port.cmd.valid && !port.cmd.write)
    rspFlow.data := mem_port.rdata.asBits
    for(i <- 0 until extra_latency)
      rspFlow = rspFlow.stage()

    port.rsp <> rspFlow
  })

  io.readPorts.foreach(port => {
    var rspFlow = cloneOf(port.rsp)
    //assert(port.cmd.payload < num_elements)
    rspFlow.data := mem.readSync(
      address = port.cmd.payload,
      enable = port.cmd.valid,
    ).asBits
    rspFlow.valid := RegNext(port.cmd.valid)

    for(i <- 0 until extra_latency)
      rspFlow = rspFlow.stage()

    port.rsp <> rspFlow
  })

  io.writePorts.foreach(port => {
//    assert(port.cmd.address < num_elements)
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
  var lram_available_maps = new mutable.HashMap[Component, Int]()
  def lram_available = lram_available_maps.getOrElse(Component.toplevel, 4)

  def find_lram[T <: Data](requirements : MemoryRequirement[T]): Option[()=>HardwareMemory[Bits]] = {
    if(requirements.numPorts > 2)
      return None

    Some(
      () => {
        if(lram_available <= 0)
          MemBackedHardwardMemory(requirements.copy(dataType = Bits(32 bits)))
        else {
          lram_available_maps(Component.toplevel) = lram_available - 1
          val latency = requirements.latencyRange._2.min(2)
          (requirements.numReadPorts, requirements.numWritePorts, requirements.numReadWritePorts) match {
            case (1, 1, 0) => new PDPSC512K_Mem(target_latency = latency)
            case (0, 0, 1) => new DPSC512K_Mem()
            case (0, 0, 2) => new DPSC512K_Mem()
          }
        }
      }
    )
  }

  def find_ebr[T <: Data](requirements : MemoryRequirement[T]): ()=>HardwareMemory[Bits] = {
    () => {
      val latency = requirements.latencyRange._2.min(2)
      (requirements.numReadPorts, requirements.numWritePorts, requirements.numReadWritePorts) match {
        case (1, 1, 0) => new PDPSC16K_Mem(data_pins = requirements.dataType.getBitsWidth,target_latency = latency)
        case _ => new MemBackedHardwardMemory[Bits](requirements.asBits)
      }
    }
  }

  def apply[T <: Data](memKind : MemTechnologyKind)(requirements : MemoryRequirement[T]): HardwareMemory[T] = {
    val allocationSize = (requirements.dataType.getBitsWidth.min(32) * requirements.num_elements) / 8

    val shouldUseLRam = lram_available > 0 && (memKind.technologyKind.toLowerCase == "lram" || allocationSize > (6 KiB))
    val lram_factory = find_lram(requirements)

    if(shouldUseLRam && lram_factory.isDefined) {
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

  def apply[T <: Data](reqs : MemoryRequirement[T], technologyKind: MemTechnologyKind = auto): HardwareMemory[T] = factory(reqs, technologyKind)

  def applyAuto[T <: Data](reqs: MemoryRequirement[T]): HardwareMemory[T] = apply(reqs, auto)
}

case class PipelinedMemoryBusMemory[T <: Data](reqs : MemoryRequirement[T], technologyKind: MemTechnologyKind = auto,
                                               factory : (MemoryRequirement[T], MemTechnologyKind) => HardwareMemory[T] = Memories.apply _ ) extends Component {
  val mem = factory(reqs, technologyKind)
  val mem_bus = mem.pmbs().head
  val io = new Bundle {
    val bus = slave(cloneOf(mem_bus))
  }
  mem_bus <> io.bus

  def latency = mem.latency
}