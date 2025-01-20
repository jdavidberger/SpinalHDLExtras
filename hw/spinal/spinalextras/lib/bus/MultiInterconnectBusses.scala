package spinalextras.lib.bus

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbArbiter, BmbDecoder, BmbDownSizerBridge, BmbParameter, BmbUpSizerBridge}
import spinal.lib.bus.misc.AddressMapping
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusArbiter, PipelinedMemoryBusConnectors, PipelinedMemoryBusDecoder, PipelinedMemoryBusRsp}
import spinal.lib.bus.wishbone.Wishbone
import spinal.lib.com.spi.ddr.SpiXdrMasterCtrl.XipBus

import scala.language.postfixOps

case class PipelinedMemoryBusMultiBus(bus : PipelinedMemoryBus,
                                      pendingMax : Int = 3,
                                      pendingRspMax : Int = 1, rspRouteQueue : Boolean = false, transactionLock : Boolean = true) extends MultiBusInterface {
  override def create_decoder(mappings:  Seq[AddressMapping]) = new Composite(bus, "") {
    val decoder = new PipelinedMemoryBusDecoder(bus.config, mappings, pendingMax)
    decoder.io.input <> bus
    val outputs = decoder.io.outputs.map(outputBus => copy(bus = outputBus))
  }.outputs

  override def create_arbiter(size:  Int)  = new Composite(bus, "") {
    val arbiter = new PipelinedMemoryBusArbiter(bus.config, size, pendingRspMax, rspRouteQueue, transactionLock)
    arbiter.io.output <> bus
    val inputs = arbiter.io.inputs.map(inputBus => copy(bus = inputBus))
  }.inputs
}

object PipelinedMemoryBusMultiBus {
  MultiInterconnectConnectFactory.AddHandler { case (m: PipelinedMemoryBusMultiBus, s: PipelinedMemoryBusMultiBus) => PipelinedMemoryBusConnectors.direct(m.bus, s.bus)}
}


import vexriscv.plugin.IBusSimpleBus
import vexriscv.ip.InstructionCacheMemBus
import vexriscv.plugin.DBusSimpleBus

package object bus {
  implicit class ISimpleBusExt(bus: IBusSimpleBus) extends PipelinedMemoryBusMultiBus(bus.toPipelinedMemoryBus()) {

  }

  implicit class InstructionCacheMemBusExt(val bus: InstructionCacheMemBus) extends MultiBusInterface {
    import spinalextras.lib.bus.bus_traits._

    override def create_decoder(mappings: Seq[AddressMapping]): Seq[InstructionCacheMemBusExt] = new Composite(bus) {
      val decoder = new GeneralBusDecoder(new InstructionCacheMemBusExtImpl(bus), mappings)
      decoder.io.input <> bus
      val outputs = decoder.io.outputs.map(outputBus => InstructionCacheMemBusExt(bus = outputBus))
    }.outputs

    override def create_arbiter(size:  Int): Seq[InstructionCacheMemBusExt] = new Composite(bus) {
      val arbiter = new GeneralBusArbiter(new InstructionCacheMemBusExtImpl(bus), size)
      arbiter.io.output <> bus
      val inputs = arbiter.io.inputs.map(inputBus => InstructionCacheMemBusExt(bus = inputBus))
    }.inputs
  }

  implicit class DBusSimpleBusExt(bus: DBusSimpleBus) extends PipelinedMemoryBusMultiBus(bus.toPipelinedMemoryBus()) {

  }

  implicit class BMBBusExt(val bus: Bmb) extends MultiBusInterface {
    override def create_decoder(mappings: Seq[AddressMapping]): Seq[BMBBusExt]   = new Composite(bus) {
      val decoder = BmbDecoder(bus.p, mappings, capabilities = mappings.map(_ => bus.p))
      decoder.io.input <> bus
      val outputs = decoder.io.outputs.map(BMBBusExt)
    }.outputs

    override def create_arbiter(size:  Int): Seq[BMBBusExt]  = new Composite(bus) {
      val arbiter = BmbArbiter(Array.fill(size)(bus.p), bus.p, lowerFirstPriority = false, 0)
      arbiter.io.output <> bus
      val inputs = arbiter.io.inputs.map(BMBBusExt)
    }.inputs
  }

  def xipParam(bus: XipBus) = BmbParameter(24, 8, 1, 1, 2)

  implicit class XipBusExt(val bus: XipBus) extends MultiBusInterface {

    override def create_decoder(mappings:  Seq[AddressMapping]): Seq[MultiBusInterface] = ???
    override def create_arbiter(size:  Int): Seq[MultiBusInterface] = new Composite(bus, "arbiter") {
      import spinalextras.lib.bus.bus_traits._
      val arbiter = new GeneralBusArbiter(new XipBusMemBusExtImpl(bus), size)
      arbiter.io.output <> bus
      val inputs = arbiter.io.inputs.map(inputBus => XipBusExt(bus = inputBus))
    }.inputs
}


  implicit class WishboneExt(val bus: Wishbone) extends MultiBusInterface {
    override def create_decoder(mappings:  Seq[AddressMapping]): Seq[MultiBusInterface] = ???
    override def create_arbiter(size:  Int): Seq[MultiBusInterface] = ???
  }
  MultiInterconnectConnectFactory.AddHandler { case (m: PipelinedMemoryBusMultiBus, s: BMBBusExt) => {
    val pmb = m.bus

    val bmb =
      if(s.bus.p.access.dataWidth != pmb.config.dataWidth) {
        val mappedP = BmbUpSizerBridge.outputParameterFrom(s.bus.p.access, pmb.config.dataWidth)

        val pmb_bmb = Bmb(mappedP)
        val bridge = BmbDownSizerBridge(s.bus.p.copy(access = mappedP), s.bus.p)
        bridge.io.input <> pmb_bmb
        bridge.io.output <> s.bus
        pmb_bmb
      } else {
        s.bus
      }

    pmb.cmd.map(c => {
      val cmd = cloneOf(bmb.cmd.payload)
      cmd.address := c.address.resized
      cmd.data := c.data
      cmd.length := 0
      cmd.last := True
      cmd.mask := c.mask
      cmd.source := 0
      cmd
    }) >> bmb.cmd

    bmb.rsp.map(r => {
      val rsp = cloneOf(pmb.rsp.payload)
      rsp.data := r.data
      rsp
    }).toFlow >> pmb.rsp
  }}

  MultiInterconnectConnectFactory.AddHandler { case (m: PipelinedMemoryBusMultiBus, s: XipBusExt) => new Composite(m.bus, "to_xip") {
    m.bus.cmd.map(c => {
      val cmd = cloneOf(s.bus.cmd.payload)
      cmd.address := c.address.resized//(c.address << log2Up(m.bus.config.dataWidth / 8)).resized
      cmd.length := m.bus.config.dataWidth / 8 - 1
      cmd
    }) >> s.bus.cmd

    val output = Stream(Fragment(Bits(m.bus.config.dataWidth bits)))
    StreamFragmentWidthAdapter(s.bus.rsp, output)

    output.map(r => {
      val rsp = cloneOf(m.bus.rsp.payload)
      rsp.data := r.fragment
      rsp
    }).toFlow >> m.bus.rsp
  }}

  MultiInterconnectConnectFactory.AddHandler { case (m: InstructionCacheMemBusExt, s: XipBusExt) => new Composite(m.bus, "to_xip"){
    m.bus.cmd.map(c => {
      val cmd = cloneOf(s.bus.cmd.payload)
      cmd.address := c.address.resized//(c.address << log2Up(m.bus.p.memDataWidth / 8)).resized
      cmd.length := (m.bus.cmd.p.burstSize << log2Up(m.bus.p.memDataWidth / 8)) - 1
      cmd
    }) >> s.bus.cmd

    val output = Stream(Fragment(Bits(m.bus.p.memDataWidth bits)))
    StreamFragmentWidthAdapter(s.bus.rsp, output)

    output.map(r => {
      val rsp = cloneOf(m.bus.rsp.payload)
      rsp.data := r.fragment
      rsp.error := False
      rsp
    }).toFlow >> m.bus.rsp
  }}

  MultiInterconnectConnectFactory.AddHandler { case (m: InstructionCacheMemBusExt, s: PipelinedMemoryBusMultiBus) => {
    m.bus.toPipelinedMemoryBus() <> s.bus
  }}

  MultiInterconnectConnectFactory.AddHandler { case (m: PipelinedMemoryBusMultiBus, s: WishboneExt) => {
    PipelinedMemoryBusToWishbone(m.bus, s.bus.config) <> s.bus
  }}
}