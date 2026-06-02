package spinalextras.lib.bus.general.vexii

import spinal.core._
import spinal.lib._
import spinalextras.lib.bus.bus.XipBusExt
import spinalextras.lib.bus.general.GeneralBusFlowRspInterface
import spinalextras.lib.bus.{MultiInterconnectConnectFactory, PipelinedMemoryBusMultiBus}
import spinalextras.lib.misc.StreamFragmentWidthAdapterWithOccupancy
import vexiiriscv.fetch.{CachelessBus, CachelessBusParam, CachelessCmd, CachelessRsp}

class CachelessBusExtImpl(p: CachelessBusParam) extends GeneralBusFlowRspInterface[CachelessBus] {
  override def data_width: Int = p.dataWidth

  override def RspStream(b: CachelessBus): Flow[RSP] = b.rsp

  override def decodeMissTarget() = {
    val bus = CachelessBus(p)
    bus.cmd.ready := True
    bus.rsp.valid := True
    bus.rsp.error := True
    bus.rsp.word := 0xDEADBEEFL
    bus.rsp.id.setAll()
    bus
  }

  override type CMD = CachelessCmd
  override type RSP = CachelessRsp
  override val dataType: HardType[CachelessBus] = CachelessBus(p)

  override def address(cmd: CMD): UInt = cmd.address

  override def cmd_requires_response(cmd: CMD): Bool = True

  override def rsp_required_count(bus: CachelessBus): UInt = 1

  override def cmd(bus: CachelessBus): Stream[CMD] = bus.cmd

  override def map_rsp_read_error(input: CachelessBus): Unit = {
    input.rsp.valid := True
    input.rsp.error := True
    input.rsp.id.setAll()
    input.rsp.payload.assignDontCare()
  }
}

object CachelessBusExtImpl {
  MultiInterconnectConnectFactory.AddHandler { case (bus: CachelessBus, s: XipBusExt) => new Composite(bus, "to_xip") {
    val data_width = bus.rsp.payload.word.getBitsWidth
    bus.cmd.map(c => {
      val cmd = cloneOf(s.bus.cmd.payload)
      cmd.address := c.address.resized
      cmd.length := data_width / 8 - 1
      cmd
    }) >> s.bus.cmd

    val output = Stream(Fragment(Bits(data_width bits)))
    val adapter = StreamFragmentWidthAdapterWithOccupancy(s.bus.rsp, output)

    output.map(r => {
      val rsp = CachelessRsp(bus.p)
      rsp.error := False
      rsp.word := r.fragment
      rsp.id := 0
      rsp
    }).toFlow >> bus.rsp
  }
  }

  MultiInterconnectConnectFactory.AddHandler { case (bus: CachelessBus, s: PipelinedMemoryBusMultiBus) => new Composite(bus, "to_pmb") {
    bus.cmd.map(c => {
      val cmd = cloneOf(s.bus.cmd.payload)
      cmd.address := c.address.resized
      cmd.write := False
      cmd.data.assignDontCare()
      cmd.mask.assignDontCare()
      cmd
    }) >> s.bus.cmd

    s.bus.rsp.map(r => {
      val rsp = CachelessRsp(bus.p)
      rsp.error := False
      rsp.word := r.data
      rsp.id := 1
      rsp
    }) >> bus.rsp
  }
  }

  def apply(p: CachelessBusParam): CachelessBusExtImpl = {
    new CachelessBusExtImpl(p)
  }
}