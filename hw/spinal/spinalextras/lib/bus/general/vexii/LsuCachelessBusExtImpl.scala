package spinalextras.lib.bus.general.vexii

import spinal.core._
import spinal.lib._
import spinalextras.lib.bus.bus.XipBusExt
import spinalextras.lib.bus.general.GeneralBusFlowRspInterface
import spinalextras.lib.bus.{MultiInterconnectConnectFactory, PipelinedMemoryBusMultiBus}
import spinalextras.lib.misc.StreamFragmentWidthAdapterWithOccupancy
import vexiiriscv.execute.lsu.{LsuCachelessBus, LsuCachelessBusParam, LsuCachelessCmd, LsuCachelessRsp}

class LsuCachelessBusExtImpl(p: LsuCachelessBusParam) extends GeneralBusFlowRspInterface[LsuCachelessBus] {
  override def data_width: Int = p.dataWidth

  override def RspStream(b: LsuCachelessBus): Flow[LsuCachelessRsp] = b.rsp

  override def decodeMissTarget() = {
    val bus = LsuCachelessBus(p)
    bus.cmd.ready := True
    bus.rsp.valid := True
    bus.rsp.error := True
    bus.rsp.data := 0xDEADBEEFL
    bus
  }

  override type CMD = LsuCachelessCmd
  override type RSP = LsuCachelessRsp
  override val dataType: HardType[LsuCachelessBus] = LsuCachelessBus(p)

  override def address(cmd: CMD): UInt = cmd.address

  override def cmd_requires_response(cmd: CMD): Bool = True

  override def rsp_required_count(bus: LsuCachelessBus): UInt = 1

  override def cmd(bus: LsuCachelessBus): Stream[CMD] = bus.cmd

  override def map_rsp_read_error(input: LsuCachelessBus): Unit = {
    input.rsp.valid := True
    input.rsp.error := True
    input.rsp.payload.assignDontCare()
  }
}

object LsuCachelessBusExtImpl {
  def apply(p: LsuCachelessBusParam): LsuCachelessBusExtImpl = {
    new LsuCachelessBusExtImpl(p)
  }

  MultiInterconnectConnectFactory.AddHandler { case (bus: LsuCachelessBus, s: XipBusExt) => new Composite(bus, "to_xip") {
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
      val rsp = LsuCachelessRsp(bus.p)
      rsp.error := False
      rsp.data := r.fragment
      rsp.id := 0
      rsp
    }).toFlow >> bus.rsp
  }
  }

  MultiInterconnectConnectFactory.AddHandler { case (bus: LsuCachelessBus, s: PipelinedMemoryBusMultiBus) => new Composite(bus, "to_pmb") {
    bus.cmd.map(c => {
      val cmd = cloneOf(s.bus.cmd.payload)
      cmd.address := c.address.resized
      cmd.write := False
      cmd.data.assignDontCare()
      cmd.mask.assignDontCare()
      cmd
    }) >> s.bus.cmd

    s.bus.rsp.map(r => {
      val rsp = LsuCachelessRsp(bus.p)
      rsp.error := False
      rsp.data := r.data
      rsp.id := 1
      rsp
    }) >> bus.rsp
  }
  }
}