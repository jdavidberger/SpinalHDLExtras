package spinalextras.lib.memory

import spinal.core._
import spinal.lib.bus.misc.SizeMapping

class StackedHardwareMemory[T <: Data](reqs: MemoryRequirement[T], direct_factory: () => HardwareMemory[Bits]) extends HardwareMemory[T]() {
  override def requirements: MemoryRequirement[T] = reqs

  val wide_factory = () => new WideHardwareMemory(requirements, direct_factory)
  val prototype = wide_factory()

  override def summary(depth: Int): Unit = {
    super.summary(depth)
    memories.foreach(m => m.summary(depth + 1))
  }

  override lazy val latency: Int = prototype.latency
  require(prototype.bitsWidth == dataType.getBitsWidth)
  require(isPow2(prototype.num_elements) || prototype.num_elements == reqs.num_elements)

  val needed_rows = ((num_elements + prototype.num_elements - 1) / prototype.num_elements).toInt
  var memories = Array.fill(needed_rows - 1)(wide_factory()) ++ Seq(prototype)

  var mappings = Array.range(0, needed_rows).map(idx => SizeMapping(prototype.num_elements * idx, prototype.num_elements))

  override lazy val actual_num_elements = memories.map(_.actual_num_elements).sum

  if (actual_num_elements > num_elements) {
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

  rsps.zipWithIndex.foreach { case (rsp, idx) => {
    rsp.valid := False
    rsp.payload.assignDontCare()

    memories.foreach(mem => {
      when(mem.rsps(idx).valid) {
        rsp.valid := True
        rsp.payload := mem.rsps(idx).payload
      }
    })
  }
  }

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
    assert(mem_readWritePorts(hit_idx).cmd.payload.address.getBitsWidth == addr.getBitsWidth)

    mem_readWritePorts(hit_idx).cmd.payload.address := addr
    mem_readWritePorts(hit_idx).cmd.payload.data := port.cmd.data
    mem_readWritePorts(hit_idx).cmd.payload.write := port.cmd.write
    if (port.cmd.mask != null)
      mem_readWritePorts(hit_idx).cmd.payload.mask := port.cmd.mask
  })

  io.writePorts.zipWithIndex.foreach(port_idx => {
    val (port, idx) = port_idx
    val (addr, hit_idx) = memories_hits(port.cmd.payload.address)
    val mem_writePorts = Vec(memories.map(_.io.writePorts(idx)))

    mem_writePorts(hit_idx).cmd.valid := port.cmd.valid
    mem_writePorts(hit_idx).cmd.payload.address := addr
    mem_writePorts(hit_idx).cmd.payload.data := port.cmd.data
    if (port.cmd.mask != null)
      mem_writePorts(hit_idx).cmd.payload.mask := port.cmd.mask

  })
}
