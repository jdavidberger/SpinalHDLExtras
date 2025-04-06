package spinalextras.lib.memory

import spinal.core.{Area, Bool, BooleanPimped, Bundle, CombInit, Component, Data, False, HardType, IntToBuilder, Reg, RegNextWhen, True, U, UInt, assert, cloneOf, in, isPow2, log2Up, out, when}
import spinal.lib.bus.simple.PipelinedMemoryBusConfig
import spinalextras.lib.formal.ComponentWithFormalProperties
import spinal.lib.{KeepAttribute, Stream, StreamFifoInterface, master, slave}
import spinalextras.lib.HardwareMemory.HardwareMemoryReadWriteCmd
import spinalextras.lib.bus.{PipelineMemoryGlobalBus, PipelinedMemoryBusCmdExt}
import spinalextras.lib.testing.test_funcs
import spinalextras.lib.{HardwareMemory, Memories, MemoryRequirement}

class MemoryBackedFifo[T <: Data](val dataType: HardType[T],
                                  val depth: Int,
                                  val mem_factory: ((MemoryRequirement[T]) => HardwareMemory[T]) = Memories.applyAuto[T] _,
                                  val withAsserts : Boolean = true,
                                  latencyRange : (Int, Int) = (1, 3)
                                 ) extends ComponentWithFormalProperties {
  val mem = mem_factory(MemoryRequirement(dataType, depth, 2, 0, 0, latencyRange = latencyRange))
  val io = slave(new FifoInterface[T](dataType, depth))

  val sysBus = PipelineMemoryGlobalBus(mem.config.toPipelinedMemoryBusConfig)

  val fifo = PipelinedMemoryBusFIFO(dataType, (0, depth << mem.config.wordAddressShift), Some(sysBus), localPushDepth = mem.cmd_latency, localPopDepth = mem.latency + 1)

  io.push <> fifo.io.push
  io.pop <> fifo.io.pop
  io.availability <> fifo.io.availability
  io.occupancy <> fifo.io.occupancy
  io.flush <> fifo.io.flush

  require(sysBus.masters.size == 2)
  require(sysBus.slaves.isEmpty)

  mem.io.readWritePorts.zipWithIndex.foreach({case (rw, idx) => {
    val memBus = sysBus.masters(idx)._1
    //test_funcs.assertPMBContract(memBus)

    when(memBus.cmd.valid) {
      assert(memBus.cmd.payload.wordAddress <= (depth - 1), Seq("Mapping higher bound check ", idx.toString, " ", memBus.cmd.address, " ", (depth).toHexString))
    }

    memBus.cmd.map(x => {
      val rtn = HardwareMemoryReadWriteCmd(mem.config)
      rtn.address := x.wordAddress

      rtn.data := x.data
      rtn.write := x.write
      if (rtn.mask != null)
        rtn.mask := x.mask
      rtn
    }).toFlow <> rw.cmd

    rw.rsp >> memBus.rsp
  }})
  sysBus.cancel()
//
//  if(withAsserts) {
//    this.formalAsserts()
//  }
}
