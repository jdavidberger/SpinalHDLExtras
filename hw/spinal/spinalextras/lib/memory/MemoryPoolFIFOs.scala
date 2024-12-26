package spinalextras.lib.memory

import spinal.core._
import spinal.lib.bus.misc.{DefaultMapping, MaskMapping, SizeMapping}
import spinal.lib.bus.simple.{PipelinedMemoryBusArbiter, PipelinedMemoryBusCmd, PipelinedMemoryBusConfig}
import spinal.lib._
import spinal.lib.bus.regif.BusIf
import spinalextras.lib.HardwareMemory.{HardwareMemoryReadWriteCmd, HardwareMemoryWriteCmd}
import spinalextras.lib.bus.PipelineMemoryGlobalBus
import spinalextras.lib.memory.MemoryPoolFIFOs.splitReadWrite
import spinalextras.lib.testing.test_funcs
import spinalextras.lib.{HardwareMemory, Memories, MemoryRequirement}

object MemoryPoolFIFOs {
  def splitReadWrite(cmd : Stream[PipelinedMemoryBusCmd]) : (Stream[PipelinedMemoryBusCmd], Stream[PipelinedMemoryBusCmd]) = {
    val readCmd = cloneOf(cmd)
    val writeCmd = cloneOf(cmd)

    readCmd.payload.mask.setAll()
    readCmd.data.assignDontCare()
    readCmd.address := cmd.address
    readCmd.write := False
    readCmd.valid := cmd.write === False && cmd.valid

    writeCmd.mask := cmd.mask
    writeCmd.data := cmd.data
    writeCmd.address := cmd.address
    writeCmd.write := True
    writeCmd.valid := cmd.write === True && cmd.valid

    cmd.ready := cmd.write ? writeCmd.ready | readCmd.ready

    (readCmd, writeCmd)
  }
}

case class MemoryPoolFIFOs[T <: Data](dataType: HardType[T],
                                      sizes: Seq[BigInt],
                                      technologyKind: MemTechnologyKind = auto,
                                      mem_factory: (MemoryRequirement[T], MemTechnologyKind) => HardwareMemory[T] = Memories.apply[T] _,
                                      localFifoDepth: Int = 0) extends Component {

  val io = new Bundle {
    val fifos = sizes.map(sm => slave(new FifoInterface[T](dataType, sm)))
  }

  val totalDepth = sizes.sum
  val mem = mem_factory(MemoryRequirement(dataType, totalDepth, 2, 0, 0), technologyKind)
  val sysBus = PipelineMemoryGlobalBus(PipelinedMemoryBusConfig(log2Up(totalDepth), dataType.getBitsWidth))

  var base = 0
  val fifos = sizes.zip(io.fifos).map(sm_fifo => {
    val fifo = new PipelinedMemoryBusFIFO(dataType, (base, sm_fifo._1.toInt), Some(sysBus), localPopDepth = mem.latency)
    base = base + sm_fifo._1.toInt
    fifo.io.push <> sm_fifo._2.push
    fifo.io.pop <> sm_fifo._2.pop
    fifo.io.flush := sm_fifo._2.flush

    sm_fifo._2.occupancy := fifo.io.occupancy

    sm_fifo._2.availability := fifo.io.availability

    fifo
  })

  val groupedMasters = sysBus.masters.map(_._1).grouped(2).toSeq

  val arbitratedBusses =
    Seq(PipelinedMemoryBusArbiter(groupedMasters.map(_(0)), 8, rspRouteQueue = true, transactionLock = false),
      PipelinedMemoryBusArbiter(groupedMasters.map(_(1)), 8, rspRouteQueue = true, transactionLock = false))

  mem.io.readWritePorts.zipWithIndex.foreach({case (rw, idx) => {
    val memBus = arbitratedBusses(idx)
    memBus.cmd.map(x => {
      val rtn = HardwareMemoryReadWriteCmd(mem.config)
      rtn.address := x.address
      rtn.data := x.data
      rtn.write := x.write
      if (rtn.mask != null)
        rtn.mask := x.mask
      rtn
    }).toFlow <> rw.cmd

    assert(test_funcs.assertPMBContract(memBus).outstanding_cnt.value === mem.io.readWritePortsOutstanding(idx))

    rw.rsp >> memBus.rsp
  }})

  sysBus.cancel()

  def attach_bus(busSlaveFactory: BusIf): Unit = {
    fifos.foreach(_.attach_bus(busSlaveFactory))
  }
}
