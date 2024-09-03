package spinalextras.lib.memory

import spinal.core.{Bundle, Component, Data, False, HardType, IntToBuilder, MemTechnologyKind, auto, in, log2Up, out}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.{master, slave}
import spinalextras.lib.{HardwareMemory, Memories, MemoryRequirement}

case class MemoryFifo[T <: Data](dataType: HardType[T], depth: Int,
                                 technologyKind: MemTechnologyKind = auto,
                                 mem_factory: (MemoryRequirement[T], MemTechnologyKind) => HardwareMemory[T] = Memories.apply[T] _) extends Component {
  val io = new Bundle {
    val push = slave Stream (dataType)
    val pop = master Stream (dataType)
    val empty = out Bool()

    val flush = in Bool() default (False)
    val occupancy = out UInt (log2Up(depth + 1) bits)
    val availability = out UInt (log2Up(depth + 1) bits)
  }

  val mem = mem_factory(MemoryRequirement(dataType, depth, 0, 1, 1), technologyKind)
  val fifo = PipelinedMemoryBusFIFO(dataType, SizeMapping(0, depth))
  fifo.bus <> mem.pmbs().head
  fifo.io.push <> io.push
  fifo.io.pop <> io.pop
  fifo.io.empty <> io.empty
  fifo.io.flush <> io.flush
  fifo.io.occupancy <> io.occupancy
  fifo.io.availability <> io.availability
}
