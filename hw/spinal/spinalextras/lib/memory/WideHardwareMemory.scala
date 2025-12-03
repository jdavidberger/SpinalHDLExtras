package spinalextras.lib.memory

import spinal.core.{Bits, Data, HardType, IntToBuilder, Vec, assert}
import spinal.lib.traversableOnceBoolPimped

class CastHardwareMemory[TFrom <: Data, T <: Data](mem : HardwareMemory[TFrom], dataType: HardType[T]) extends HardwareMemory[T] {
  override def requirements: MemoryRequirement[T] = mem.requirements.asType[T](dataType)

  override lazy val actual_num_elements: BigInt = mem.actual_num_elements

  io.readPorts.zip(mem.io.readPorts).foreach { case (r1, r2) =>
    r1 <> r2
  }

  io.readPorts.zip(mem.io.readPorts).foreach { case (r1, r2) =>
    r1 <> r2
  }

  io.readWritePorts.zip(mem.io.readWritePorts).foreach { case (r1, r2) =>
    r1 <> r2
  }

  io.writePorts.zip(mem.io.writePorts).foreach { case (r1, r2) =>
    r1 <> r2
  }

}

class WideHardwareMemory[T <: Data](reqs: MemoryRequirement[T], direct_factory: () => HardwareMemory[Bits]) extends HardwareMemory[T]() {
  val prototype = direct_factory()

  override def summary(depth: Int): Unit = {
    super.summary(depth)
    memories.foreach(m => m.summary(depth + 1))
  }

  override lazy val latency: Int = prototype.latency

  override def requirements: MemoryRequirement[T] = reqs.copy(num_elements = prototype.num_elements)

  val mod_width = (dataType.getBitsWidth) % prototype.bitsWidth
  val needed_cols = dataType.getBitsWidth / prototype.bitsWidth
  lazy val mod_memory = if (mod_width != 0 && (dataType.getBitsWidth > prototype.bitsWidth)) Seq(Memories(reqs.copy(dataType = Bits(mod_width bits), latencyRange = (prototype.latency, prototype.latency)))) else Seq()
  var memories = Array.fill(needed_cols - 1)(direct_factory()) ++ Seq(prototype) ++ mod_memory
  memories.zipWithIndex.map(x => x._1.setName(s"memories_col_${x._2}_${x._1.actual_num_elements}x${x._1.bitsWidth}_${x._1.getClass.getSimpleName}"))

  override lazy val actual_num_elements = memories.map(_.num_elements).min

  rsps.zipWithIndex.foreach { case (rsp, idx) => {
    rsp.payload.data.assignFromBits(Vec(memories.map(mem => {
      mem.rsps(idx).payload.data
    }).toSeq).asBits.resized)


    rsp.valid := memories.head.rsps(idx).valid
    assert(Vec(memories.map(_.rsps(idx).valid).map(x => x === rsp.valid)).andR, "Read responses not synced")
  }
  }

  memories.foreach(mem => {
    require(io.readPorts.length == mem.io.readPorts.length)
    require(io.readWritePorts.length == mem.io.readWritePorts.length)
  })


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
    if (port.cmd.mask != null) {
      datas.map(_._2).zip(port.cmd.mask.subdivideIn(prototype.bitsWidth / 8 bits, strict = false)).foreach(d => {
        d._1 := d._2
      })
    } else {
      datas.map(_._2).foreach(d => {
        if (d != null)
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


    if (port.cmd.mask != null) {
      datas.map(_._3).zip(port.cmd.mask.subdivideIn((prototype.bitsWidth / 8) bits, strict = false)).foreach(d => {
        d._1 := d._2
      })
    } else {
      datas.map(_._3).foreach(d => {
        if (d != null)
          d.setAll()
      })
    }
  })

}
