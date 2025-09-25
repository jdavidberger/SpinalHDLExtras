package spinalextras.lib.memory

import spinal.core._
import spinal.lib._
import spinal.lib.bus.simple.PipelinedMemoryBusRsp
import spinalextras.lib._
import spinalextras.lib.formal.FormalProperties

case class MemBackedHardwardMemory[T <: Data](override val requirements : MemoryRequirement[T]) extends
  HardwareMemory[T]() {
  val mem = Mem[T](dataType, num_elements)

  override lazy val latency = requirements.latencyRange._2
  val extra_latency = latency - 1

  if (globalData.config.flags.contains(GenerationFlags.simulation)) {
    //mem.randBoot()
    mem.init((0 until num_elements.toInt).map(idx => B(0).as(dataType) ))
  }

  def mapReadLatency(immediateRspFlow : Flow[PipelinedMemoryBusRsp], outstanding : UInt): Flow[PipelinedMemoryBusRsp] = {
    var rspFlow = immediateRspFlow
    var inPipeline = UInt(0 bits)
    for(i <- 0 until extra_latency) {
      inPipeline = inPipeline +^ rspFlow.valid.asUInt
      rspFlow = rspFlow.stage().setName(s"rspFlowStage_${i}")
    }
    inPipeline = inPipeline +^ rspFlow.valid.asUInt
    assert(inPipeline === outstanding)
    //inPipeline.setName(s"${outstanding.name}_inPipeline")
    rspFlow
  }

  val readWritePortArea = io.readWritePorts.zipWithIndex.map(port_idx => new Area {
    val (port, idx) = port_idx
    val mask_width = if(port.cmd.mask != null) port.cmd.mask.getWidth else -1
    val mem_port = mem.readWriteSyncPort(maskWidth = mask_width)
    if(port.cmd.mask != null)
      mem_port.mask := port.cmd.mask
    mem_port.address := port.cmd.address

    mem_port.enable := port.cmd.valid
    mem_port.write := port.cmd.write
    mem_port.wdata.assignFromBits(port.cmd.data)

    var rspFlow = cloneOf(port.rsp).setName("rspFlowInit")
    rspFlow.valid := RegNext(port.cmd.valid && !port.cmd.write, init = False)
    rspFlow.data := mem_port.rdata.asBits

    port.rsp <> mapReadLatency(rspFlow, io.readWritePortsOutstanding(idx))
  })

  val readPortArea = io.readPorts.zipWithIndex.map(port_idx => new Area {
    val (port, idx) = port_idx
    var rspFlow = cloneOf(port.rsp)
    //assert(port.cmd.payload < num_elements)
    rspFlow.data := mem.readSync(
      address = port.cmd.payload,
      enable = port.cmd.valid,
    ).asBits
    rspFlow.valid := RegNext(port.cmd.valid) init(False)

    port.rsp <> mapReadLatency(rspFlow, io.readPortsOutstanding(idx))
  })

  io.writePorts.foreach(port => {
    //    assert(port.cmd.address < num_elements)
    mem.write(address = port.cmd.address,
      data = port.cmd.data.as(dataType),
      enable = port.cmd.valid,
      mask = port.cmd.mask
    )
  })

  override protected def formalInputProperties() = new FormalProperties(this) {
    io.readWritePorts.zipWithIndex.foreach(port_idx => new Area {
      val (port, idx) = port_idx

      val valid_address = port.cmd.address.resize(log2Up(num_elements + 1) bits) < num_elements
      when(port.cmd.valid) {
        addFormalProperty(valid_address, Seq(s"Validate access ${idx} in range 0x${num_elements.toString(16)} ", port.cmd.address.resize(log2Up(num_elements + 1) bits)))
      }
    })


    io.readPorts.zipWithIndex.foreach(port_idx => new Area {
      val (port, idx) = port_idx
      val valid_address = port.cmd.payload.resize(log2Up(num_elements + 1) bits) < num_elements
      when(port.cmd.valid) {
        addFormalProperty(valid_address, Seq(s"Validate access ${idx} in range 0x${num_elements.toString(16)} ", port.cmd.payload.resize(log2Up(num_elements + 1) bits)))
      }
    })
  }
}
