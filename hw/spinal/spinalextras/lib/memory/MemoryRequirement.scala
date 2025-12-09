package spinalextras.lib.memory

import spinal.core._
import spinal.core.fiber.Handle.initImplicit
import spinal.core.internals.MemTopology

case class MemoryRequirement[T <: Data](dataType: HardType[T], num_elements: BigInt, numReadWritePorts: Int,
                                        numReadPorts: Int = 0, numWritePorts: Int = 0,
                                        needsMask: Boolean = false,
                                        latencyRange: (Int, Int) = (1, 3), label: String = "", initialContent: Seq[BigInt] = Seq()) {
  lazy val allocationSizeInBits = dataType.getBitsWidth * num_elements
  lazy val allocationSizeInBytes = dataType.getBitsWidth * num_elements / 8

  lazy val numPorts = numReadWritePorts + numReadPorts + numWritePorts

  override def toString: String = {
    val prefix = if(label.nonEmpty) f"${label} - " else ""
    if (dataType.globalData != null) {
      s"${prefix}${dataType.getBitsWidth}bits_${num_elements}d_${numReadWritePorts}rw_${numReadPorts}r_${numWritePorts}"
    } else {
      s"${prefix}${num_elements}d_${numReadWritePorts}rw_${numReadPorts}r_${numWritePorts}"
    }
  }

  def asBits = new MemoryRequirement[Bits](Bits(dataType.getBitsWidth bits), num_elements, numReadWritePorts, numReadPorts, numWritePorts, needsMask, latencyRange)
  def asType[T1 <: Data](dataType: HardType[T1]) = {
    assert(dataType.getBitsWidth == this.dataType.getBitsWidth)
    new MemoryRequirement[T1](dataType, num_elements, numReadWritePorts, numReadPorts, numWritePorts, needsMask, latencyRange)
  }
}

object MemoryRequirement {
  def apply(topo: MemTopology): MemoryRequirement[Bits] = {
    MemoryRequirement[Bits](Bits(topo.mem.wordType.getBitsWidth bits), topo.mem.wordCount, topo.readWriteSync.size, topo.readsSync.size, topo.writes.size,
      needsMask = true, latencyRange = (1, 1), initialContent = topo.mem.initialContent)
  }
}