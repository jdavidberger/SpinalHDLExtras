package spinalextras.lib.logging

import spinal.core._
import spinal.lib._
import spinal.lib.bus._
import spinal.lib.bus.regif.AccessType.{RO, RW}
import spinal.lib.bus.regif.BusIf
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusCmd}
import spinalextras.lib.Constraints
import spinalextras.lib.misc.RegisterTools

object PipelinedMemoryBusLogger {
  def flows(data_width : Int, busses: PipelinedMemoryBus*): Seq[(Data, Flow[Bits])] = {
    busses.flatMap(bus => {
      FlowLogger.flows(bus.cmd.toFlowFire.map(x => {
        val c = PipelinedMemoryBusCmd(bus.config.copy(dataWidth = data_width))
        c.data := x.data.resized
        c.address := x.address
        c.write := x.write
        c.mask := x.mask.resized
        c
      }).setName(s"${bus.name}_cmd")) ++
        FlowLogger.flows(bus.rsp.setName(s"${bus.name}_rsp"))
    })
  }
  def flows(busses: PipelinedMemoryBus*): Seq[(Data, Flow[Bits])] = {
    flows(32, busses:_*)
  }

  def attach_debug_registers(busSlaveFactory: BusIf, busses: PipelinedMemoryBus*): Unit = {

    busses.foreach(bus => {
      RegisterTools.Counter(busSlaveFactory, f"${bus.name}_write_count", bus.cmd.write && bus.cmd.fire, bus.cmd.valid.clockDomain)
      RegisterTools.Counter(busSlaveFactory, f"${bus.name}_read_count", ~bus.cmd.write && bus.cmd.fire, bus.cmd.valid.clockDomain)
      RegisterTools.Counter(busSlaveFactory, f"${bus.name}_rsp_count", bus.rsp.fire, bus.cmd.valid.clockDomain)
    })
  }
}
