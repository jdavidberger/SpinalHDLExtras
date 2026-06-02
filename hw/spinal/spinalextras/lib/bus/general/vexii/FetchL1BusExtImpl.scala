package spinalextras.lib.bus.general.vexii

import spinal.core._
import spinal.lib._
import spinalextras.lib.bus.bus.XipBusExt
import spinalextras.lib.bus.general.{GeneralBusFlowRspInterface, GeneralBusStreamRspInterface}
import spinalextras.lib.bus.{MultiInterconnectConnectFactory, PipelinedMemoryBusMultiBus}
import spinalextras.lib.misc.StreamFragmentWidthAdapterWithOccupancy
import vexiiriscv.fetch.{CachelessCmd, CachelessRsp, FetchL1Bus, FetchL1BusParam, FetchL1Cmd, FetchL1Rsp}

class FetchL1BusExtImpl(p: FetchL1BusParam) extends GeneralBusStreamRspInterface[FetchL1Bus] {
  override def data_width: Int = p.dataWidth

  override def RspStream(b: FetchL1Bus): Stream[RSP] = b.rsp

  override def decodeMissTarget() = {
    val bus = FetchL1Bus(p)
    bus.cmd.ready := True
    bus.rsp.valid := True
    bus.rsp.error := True
    bus.rsp.data := 0xDEADBEEFL
    bus.rsp.id.setAll()
    bus
  }

  override type CMD = FetchL1Cmd
  override type RSP = FetchL1Rsp
  override val dataType: HardType[FetchL1Bus] = FetchL1Bus(p)

  override def address(cmd: CMD): UInt = cmd.address

  override def cmd_requires_response(cmd: CMD): Bool = True

  override def rsp_required_count(bus: FetchL1Bus): UInt = 1

  override def cmd(bus: FetchL1Bus): Stream[CMD] = bus.cmd

  override def map_rsp_read_error(input: FetchL1Bus): Unit = {
    input.rsp.valid := True
    input.rsp.error := True
    input.rsp.id.setAll()
    input.rsp.payload.assignDontCare()
  }
}

object FetchL1BusExtImpl {
  MultiInterconnectConnectFactory.AddHandler { case (bus: FetchL1Bus, s: XipBusExt) => new Composite(bus, "to_xip") {
    val data_width = bus.rsp.payload.data.getBitsWidth
    bus.cmd.map(c => {
      val cmd = cloneOf(s.bus.cmd.payload)
      cmd.address := c.address.resized
      cmd.length := data_width / 8 - 1
      cmd
    }) >> s.bus.cmd

    val output = Stream(Fragment(Bits(data_width bits)))
    val adapter = StreamFragmentWidthAdapterWithOccupancy(s.bus.rsp, output)

    output.map(r => {
      val rsp = FetchL1Rsp(bus.p.dataWidth, bus.p.refillCount)
      rsp.error := False
      rsp.data := r.fragment
      rsp.id := 0
      rsp
    }) >> bus.rsp
  }
  }

  MultiInterconnectConnectFactory.AddHandler { case (bus: FetchL1Bus, s: PipelinedMemoryBusMultiBus) => new Composite(bus, "to_pmb") {
    bus.cmd.map(c => {
      val cmd = cloneOf(s.bus.cmd.payload)
      cmd.address := c.address.resized
      cmd.write := False
      cmd.data.assignDontCare()
      cmd.mask.assignDontCare()
      cmd
    }) >> s.bus.cmd

    s.bus.rsp.map(r => {
      val rsp = FetchL1Rsp(bus.p.dataWidth, bus.p.refillCount)
      rsp.error := False
      rsp.data := r.data
      rsp.id.setAll()
      rsp
    }).toStream >> bus.rsp
  }
  }

  def apply(p: FetchL1BusParam): FetchL1BusExtImpl = {
    new FetchL1BusExtImpl(p)
  }
}