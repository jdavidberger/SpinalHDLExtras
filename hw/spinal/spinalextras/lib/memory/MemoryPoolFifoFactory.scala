package spinalextras.lib.memory

import spinal.core.{Bits, Component, Data, HardType, IntToBuilder, MemTechnologyKind, auto}
import spinal.lib.{StreamFifo, master}
import spinalextras.lib.misc.GlobalSignals

import scala.collection.mutable

object FifoFactory {
  type create_fifo_fn[T <: Data] = (Int, HardType[T]) => FifoInterface[T]

  def default[T <: Data](depth: Int, dataType: HardType[T]): FifoInterface[T] = {
    val rtn = FifoInterface(dataType, depth)
    val fifo = StreamFifo(dataType = dataType, depth = depth)
    rtn.push >> fifo.io.push
    rtn.pop << fifo.io.pop.stage()
    rtn.availability := fifo.io.availability
    rtn.occupancy := fifo.io.occupancy
    fifo.io.flush := rtn.flush
    rtn
  }
}

trait FifoFactory[T <: Data] {
  def create_fifo(depth: Int): FifoInterface[T]
}

class MemoryPoolFifoFactory[T <: Data] {
  val interfaces = new mutable.ArrayBuffer[FifoInterface[T]]()
  val component = if(Component.current == null) null else Component.current.parent

  def apply(depth: Int, dataType: HardType[T]) : FifoInterface[T] = {
    val rtn = master(FifoInterface(dataType, depth))
    interfaces += GlobalSignals.externalize(rtn, component)
    rtn
  }

  def build(technologyKind: MemTechnologyKind = auto,
            mem_factory: (MemoryRequirement[T], MemTechnologyKind) => HardwareMemory[T] = Memories.apply[T] _,
            localFifoDepth: Int = 0): MemoryPoolFIFOs[T] = {
    val fifos = interfaces
    val sizes = interfaces.map(_.depth)
    if(interfaces.isEmpty)
      return null

    val dataType = interfaces.map(_.dataType).maxBy(_.getBitsWidth)
    val pool = new MemoryPoolFIFOs(dataType, sizes, technologyKind, mem_factory, localFifoDepth)
    pool.io.fifos.zip(fifos).map(x => {
      x._1 <> x._2
    })
    pool
  }
}
