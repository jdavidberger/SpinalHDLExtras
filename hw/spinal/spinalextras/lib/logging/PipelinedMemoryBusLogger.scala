package spinalextras.lib.logging

import spinal.core._
import spinal.lib._
import spinal.lib.bus._
import spinal.lib.bus.regif.AccessType.{RO, RW}
import spinal.lib.bus.regif.BusIf
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusCmd}
import spinalextras.lib.Constraints

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
      def create_counter(reg: UInt, cond : Bool): Unit = {
        if(reg.clockDomain != bus.cmd.valid.clockDomain) {
          val r = new ClockingArea(bus.cmd.valid.clockDomain) {
            val r = Reg(cloneOf(reg)) init(0)
            when(RegNext(cond)) {
              r := RegNext(r + 1)
            }
          }.r

          val cc = StreamCCByToggle(cloneOf(r), bus.cmd.valid.clockDomain, reg.clockDomain)
          cc.io.input.payload := r
          cc.io.input.valid := True
          cc.io.output.ready := True

          reg.setAsReg() init(0)
          when(cc.io.output.fire) {
            reg := cc.io.output.payload
          }
        } else {
          reg.setAsReg() init(0)
          when(cond) {
            reg := RegNext(reg + 1)
          }
        }
      }

      val write_count_reg = busSlaveFactory.newReg(f"${bus.name}_write_count")
      val write_count = write_count_reg.field(UInt(32 bits), RO, s"${bus.name}_write_count}")
      create_counter(write_count, bus.cmd.write && bus.cmd.fire)

      val read_count_reg = busSlaveFactory.newReg(f"${bus.name}_read_count")
      val read_count = read_count_reg.field(UInt(32 bits), RO, s"${bus.name}_read_count}")
      create_counter(read_count, !bus.cmd.write && bus.cmd.fire)

      val rsp_count_reg = busSlaveFactory.newReg(f"${bus.name}_rsp_count")
      val rsp_count = rsp_count_reg.field(UInt(32 bits), RO, s"${bus.name}_rsp_count}")
      create_counter(rsp_count, bus.rsp.fire)
    })
  }
}
