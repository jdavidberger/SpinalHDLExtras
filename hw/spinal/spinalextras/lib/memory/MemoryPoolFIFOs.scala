package spinalextras.lib.memory

import spinal.core._
import spinal.lib._
import spinal.lib.bus.regif.BusIf
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusArbiter, PipelinedMemoryBusCmd}
import spinalextras.lib.bus.{PipelineMemoryGlobalBus, PipelinedMemoryBusCmdExt, PipelinedMemoryBusConfigExt}
import spinalextras.lib.formal.fillins.PipelinedMemoryBusFormal.PipelinedMemoryBusFormalExt
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties, FormalProperty}
import spinalextras.lib.memory.HardwareMemory.HardwareMemoryReadWriteCmd

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
                                      localFifoDepth: Int = 0) extends ComponentWithFormalProperties {

  val io = new Bundle {
    val fifos = sizes.map(sm => slave(new FifoInterface[T](dataType, sm)))
  }

  val totalDepth = sizes.sum
  val totalDepthInBytes = totalDepth << (dataType.getBitsWidth / 8.0).ceil.toInt
  val mem = mem_factory(MemoryRequirement(dataType, totalDepth, 2, 0, 0), technologyKind)
  val sysBus = PipelineMemoryGlobalBus(mem.config.toPipelinedMemoryBusConfig)

  var base = 0
  val fifos = sizes.zip(io.fifos).map(sm_fifo => {
    val fifo = new PipelinedMemoryBusFIFO(dataType, (base, sm_fifo._1.toInt << sysBus.config.wordAddressShift), Some(sysBus), localPopDepth = mem.latency)
    base = base + (sm_fifo._1.toInt << sysBus.config.wordAddressShift)
    fifo.io.push << sm_fifo._2.push
    fifo.io.pop.stage() >> sm_fifo._2.pop
    fifo.io.flush := sm_fifo._2.flush

    sm_fifo._2.occupancy := fifo.io.occupancy

    sm_fifo._2.availability := fifo.io.availability

    fifo
  })

  val groupedMasters = sysBus.masters.map(_._1).grouped(2).toSeq

  withAutoPull()
  def create_arbiters(inputs : Seq[PipelinedMemoryBus], pendingRspMax : Int, rspRouteQueue : Boolean, transactionLock : Boolean): PipelinedMemoryBus = {
    val c = PipelinedMemoryBusArbiter(inputs.head.config, inputs.size, pendingRspMax, rspRouteQueue, transactionLock)
    (inputs, c.io.inputs).zipped.foreach {case (in, in_port) => {
      //assert(in.formalContract.outstandingReads.value === in_port.formalContract.outstandingReads.value)
      (in <> in_port)
    }}
    c.io.output
  }

  val arbitratedBusses =
    Seq(create_arbiters(groupedMasters.map(_(0)), 8, rspRouteQueue = true, transactionLock = false),
      create_arbiters(groupedMasters.map(_(1)), 1, rspRouteQueue = false, transactionLock = false))

  mem.io.readWritePorts.zipWithIndex.foreach({case (rw, idx) => {
    val memBus = arbitratedBusses(idx)
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

  def attach_bus(busSlaveFactory: BusIf): Unit = {
    fifos.foreach(_.attach_bus(busSlaveFactory))
  }

  override def formalComponentProperties(): Seq[FormalProperty] = new FormalProperties(this) {
    mem.io.readWritePorts.zipWithIndex.foreach({case (rw, idx) => {
      val memBus = arbitratedBusses(idx)
      addFormalProperty(memBus.contract.outstandingReads.value === mem.io.readWritePortsOutstanding(idx), s"Match read port outstanding counts ${idx}")
    }})
  }
}
