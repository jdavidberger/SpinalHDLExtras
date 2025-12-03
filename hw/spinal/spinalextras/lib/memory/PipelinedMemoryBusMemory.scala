package spinalextras.lib.memory

import spinal.core.{Bundle, Component, Data, MemTechnologyKind, auto, cloneOf}
import spinal.lib.slave

case class PipelinedMemoryBusMemory[T <: Data](reqs: MemoryRequirement[T], technologyKind: MemTechnologyKind = auto,
                                               factory: (MemoryRequirement[T], MemTechnologyKind) => HardwareMemory[T] = Memories.apply[T] _) extends Component {
  val mem = factory(reqs, technologyKind)
  val mem_bus = mem.pmbs().head
  val io = new Bundle {
    val bus = slave(cloneOf(mem_bus))
  }
  mem_bus <> io.bus

  def latency = mem.latency
}
