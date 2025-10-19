package spinalextras.lib.memory

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.{Area, Bits, Bool, BooleanPimped, Bundle, CombInit, Component, Data, False, HardType, IntToBuilder, Reg, RegNextWhen, True, U, UInt, assert, cloneOf, in, isPow2, log2Up, out, when}
import spinal.lib.bus.simple.PipelinedMemoryBusConfig
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties, FormalProperty}
import spinal.lib.{KeepAttribute, Stream, StreamFifoInterface, master, slave}
import spinalextras.lib.HardwareMemory.HardwareMemoryReadWriteCmd
import spinalextras.lib.bus.{PipelineMemoryGlobalBus, PipelinedMemoryBusCmdExt}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut, test_funcs}
import spinalextras.lib.{HardwareMemory, Memories, MemoryRequirement}

class MemoryBackedFifo[T <: Data](val dataType: HardType[T],
                                  val depth: Int,
                                  val mem_factory: ((MemoryRequirement[T]) => HardwareMemory[T]) = Memories.applyAuto[T] _,
                                  val withAsserts : Boolean = true,
                                  latencyRange : (Int, Int) = (1, 3),
                                  val memory_label : String = ""
                                 ) extends ComponentWithFormalProperties {
  val mem = mem_factory(MemoryRequirement(dataType, depth, 2, 0, 0, latencyRange = latencyRange, label=memory_label))
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

  override def formalComponentInputProperties(): Seq[FormalProperty] = new FormalProperties(this) {
    when(io.flush) {
      addFormalProperty(io.pop.ready, "Flush requires a ready on the pop")
    }
  }

}



class MemoryBackedFifoFormalTester extends AnyFunSuite with FormalTestSuite {
  override def defaultDepth() = 10

  formalTests().foreach(t => test(t._1) {
    t._2()
  })

  override def generateRtl() = {
    Seq(
      (s"Basic", () =>
        GeneralFormalDut(() => new MemoryBackedFifo(Bits(95 bits), 16000)))
    )
  }
}





