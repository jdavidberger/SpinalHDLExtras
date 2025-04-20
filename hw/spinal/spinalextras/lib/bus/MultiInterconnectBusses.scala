package spinalextras.lib.bus

import spinal.core._
import spinal.lib._
import spinal.lib.bus.bmb.{Bmb, BmbArbiter, BmbDecoder, BmbDownSizerBridge, BmbParameter, BmbUpSizerBridge}
import spinal.lib.bus.misc.AddressMapping
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusArbiter, PipelinedMemoryBusConnectors, PipelinedMemoryBusDecoder, PipelinedMemoryBusRsp}
import spinal.lib.bus.wishbone.Wishbone
import spinal.lib.com.spi.ddr.SpiXdrMasterCtrl.{XipBus, XipBusParameters}

import scala.language.postfixOps
import spinalextras.lib.bus.general._
import spinalextras.lib.formal.FormalMasterSlave
import spinalextras.lib.formal.StreamFormal.StreamExt
import spinalextras.lib.formal._
import spinalextras.lib.formal.fillins.Wishbone._
import spinalextras.lib.formal.fillins.PipelinedMemoryBusFormal._

import scala.collection.mutable
import scala.reflect.{ClassTag, classTag}

case class PipelinedMemoryBusMultiBus(bus : PipelinedMemoryBus,
                                      pendingMax : Int = 3,
                                      pendingRspMax : Int = 1, rspRouteQueue : Boolean = false, transactionLock : Boolean = true) extends MultiBusInterface {
  override def address_width = bus.config.addressWidth
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

  override def isValidProducer: Bool = PipelinedMemoryBusFormalExt(bus).formalIsProducerValid().map(_.condition).andR

  override def isValidConsumer: Bool = PipelinedMemoryBusFormalExt(bus).formalIsConsumerValid().map(_.condition).andR
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
    override def toString = bus.toString()

    override def address_width = bus.p.addressWidth
    override def create_decoder(mappings: Seq[AddressMapping]): Seq[InstructionCacheMemBusExt] = new Composite(bus) {
      val decoder = new GeneralBusDecoder(InstructionCacheMemBusInterfaceExtImpl(bus), mappings)
      decoder.setWeakName(s"${bus.name}_decoder")
      decoder.io.input <> bus
      val outputs = decoder.io.outputs.map(outputBus => InstructionCacheMemBusExt(bus = outputBus))
    }.outputs

    override def create_arbiter(size:  Int): Seq[InstructionCacheMemBusExt] = new Composite(bus) {
      val arbiter = new GeneralBusArbiter(InstructionCacheMemBusInterfaceExtImpl(bus), size)
      arbiter.io.output <> bus
      val inputs = arbiter.io.inputs.map(inputBus => InstructionCacheMemBusExt(bus = inputBus))
    }.inputs

    override def isValidProducer: Bool = InstructionCacheMemBusInterfaceExtImpl(bus).isProducerValid(bus)

    override def isValidConsumer: Bool = InstructionCacheMemBusInterfaceExtImpl(bus).isConsumerValid(bus)
  }

  implicit class DBusSimpleBusExt(val bus: DBusSimpleBus) extends MultiBusInterface {
    import spinalextras.lib.bus.general._
    override def toString = bus.toString()

    override def address_width = bus.cmd.address.getWidth
    override def create_decoder(mappings: Seq[AddressMapping]): Seq[DBusSimpleBusExt] = new Composite(bus) {
      val decoder = new GeneralBusDecoder(DSimpleBusInterfaceExtImpl(bus), mappings)
      decoder.setWeakName(s"${bus.name}_decoder")
      decoder.io.input <> bus
      val outputs = decoder.io.outputs.map(outputBus => DBusSimpleBusExt(bus = outputBus))
    }.outputs

    override def create_arbiter(size:  Int): Seq[DBusSimpleBusExt] = new Composite(bus) {
      val arbiter = new GeneralBusArbiter(DSimpleBusInterfaceExtImpl(bus), size)
      arbiter.io.output <> bus
      val inputs = arbiter.io.inputs.map(inputBus => DBusSimpleBusExt(bus = inputBus))
    }.inputs

    override def isValidProducer: Bool = DSimpleBusInterfaceExtImpl(bus).isProducerValid(bus)

    override def isValidConsumer: Bool = DSimpleBusInterfaceExtImpl(bus).isConsumerValid(bus)
  }

  val dbusSimpleContracts = new mutable.WeakHashMap[DBusSimpleBus, DBusSimpleContract]()

  class DBusSimpleContract(bus : DBusSimpleBus) extends FormalProperties(bus) {
    import bus._

    val outstandingReads = Reg(SInt(32 bits)) init(0)

    val toAdd = Mux(cmd.fire, U(1), U(0))
    outstandingReads := (outstandingReads +^ toAdd.intoSInt -^ rsp.ready.asUInt.intoSInt).resized

    addFormalProperty(outstandingReads > 0 || !rsp.ready)
    addFormalProperties(cmd.formalIsConsumerValid())
    addFormalProperty(outstandingReads >= 0, "XIP outstanding reads should be above 0")
  }

  class DBusSimpleFormal(val bus : DBusSimpleBus) extends FormalMasterSlave {
    def contract = dbusSimpleContracts.getOrElseUpdate(bus, new DBusSimpleContract(bus))
    override def asIMasterSlave = bus
    override def formalIsProducerValid(): Seq[FormalProperty] = bus.cmd.formalIsProducerValid()
    override def formalIsConsumerValid(): Seq[FormalProperty] = contract
  }
  fillins.AddHandler { case bus: DBusSimpleBus => new DBusSimpleFormal(bus) }


  implicit class BMBBusExt(val bus: Bmb) extends MultiBusInterface {
    override def address_width = bus.p.access.addressWidth
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


    override def isValidProducer: Bool = ???

    override def isValidConsumer: Bool = ???
  }

  def xipParam(bus: XipBus) = BmbParameter(24, 8, 1, 1, 2)

  val xipContracts = new mutable.WeakHashMap[XipBus, XipBusContract]()

  class XipBusContract(bus : XipBus) extends FormalProperties(bus) {
    import bus._

    val outstandingReads = Reg(SInt(32 bits)) init(0)

    val toAdd = Mux(cmd.fire, cmd.length + 1, U(0))
    outstandingReads := (outstandingReads +^ toAdd.intoSInt -^ rsp.fire.asUInt.intoSInt).resized

    addFormalProperty(outstandingReads > 0 || !rsp.fire)
    addFormalProperties(rsp.formalIsProducerValid())
    addFormalProperties(cmd.formalIsConsumerValid())
    addFormalProperty(outstandingReads >= 0, "XIP outstanding reads should be above 0")
  }

  class XipBusFormal(bus : XipBus) extends FormalMasterSlave with FormalDataWithEquivalnce[XipBusFormal] {
    override type Self = XipBusFormal

    override def clone(): Bundle = new XipBus(bus.p)

    def contract = xipContracts.getOrElseUpdate(bus, new XipBusContract(bus))

    override def formalIsConsumerValid(): Seq[FormalProperty] = contract

    override def formalIsProducerValid(): Seq[FormalProperty] = bus.cmd.formalIsProducerValid() ++ bus.rsp.formalIsConsumerValid()

    override def asIMasterSlave: XipBus = bus

    override def selfClassTag: ClassTag[XipBusFormal] = classTag[XipBusFormal]

    def formalAssertEquivalence(that: XipBusFormal): Unit = {
      ???
    }
  }
  fillins.AddHandler { case bus: XipBus => new XipBusFormal(bus) }

  implicit class XipBusExt(val bus: XipBus) extends MultiBusInterface {
    override def toString = bus.toString()

    override def address_width = bus.p.addressWidth
    override def create_decoder(mappings:  Seq[AddressMapping]): Seq[MultiBusInterface] = ???
    override def create_arbiter(size:  Int): Seq[MultiBusInterface] = new Composite(bus, "arbiter") {
      val arbiter = new GeneralBusArbiter(new XipBusMemBusInterfaceExtImpl(bus.p), size)
      arbiter.io.output <> bus
      val inputs = arbiter.io.inputs.map(inputBus => XipBusExt(bus = inputBus))
    }.inputs

    override def isValidProducer: Bool = XipBusMemBusInterfaceExtImpl(bus.p).isProducerValid(bus)

    override def isValidConsumer: Bool = XipBusMemBusInterfaceExtImpl(bus.p).isConsumerValid(bus)
  }


  implicit class WishboneMultiBusInterface(val bus: Wishbone) extends MultiBusInterface {
    override def toString = bus.toString()

    override def address_width = bus.config.addressWidth
    override def create_decoder(mappings:  Seq[AddressMapping]): Seq[MultiBusInterface] = ???
    override def create_arbiter(size:  Int): Seq[MultiBusInterface] = ???

    override def isValidProducer: Bool = bus.formalIsProducerValid().map(_.condition).andR

    override def isValidConsumer: Bool = bus.formalIsConsumerValid().map(_.condition).andR
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

  MultiInterconnectConnectFactory.AddHandler { case (m: PipelinedMemoryBusMultiBus, s: XipBusExt) => new Composite(m.bus, "to_xip") with HasFormalProperties {
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

    /**
     * @return The formal properties which should all be true if the formalInputProperties are true too. These are the main
     *         assertions we are concerned with defining and verifying in formal testing
     *
     *         For complicated properties, consider using the helper class `FormalProperties`
     */
    override protected def formalProperties(): Seq[FormalProperty] = {
      FormalProperty.toFormalProperties(PipelinedMemoryBusFormalExt(m.bus).contract.outstandingReads.value.asSInt ===
        new XipBusFormal(s.bus).contract.outstandingReads)
    }
  }}


  MultiInterconnectConnectFactory.AddHandler { case (m: DBusSimpleBusExt, s: XipBusExt) => new Composite(m.bus, "to_xip") {
    m.bus.cmd.map(c => {
      val wordShift = log2Up(c.address.getWidth / 8)
      val cmd = cloneOf(s.bus.cmd.payload)
      cmd.address := ((c.address >> wordShift) << wordShift).resized //(c.address << log2Up(m.bus.config.dataWidth / 8)).resized
      cmd.length := m.bus.cmd.data.getWidth / 8 - 1
      cmd
    }) >> s.bus.cmd

    val output = Stream(Fragment(Bits(m.bus.cmd.data.getWidth bits)))
    StreamFragmentWidthAdapter(s.bus.rsp, output)

    output.ready := True
    m.bus.rsp.ready := output.valid
    m.bus.rsp.data := output.payload.fragment
    m.bus.rsp.error := False
  }}

  MultiInterconnectConnectFactory.AddHandler { case (m: InstructionCacheMemBusExt, s: InstructionCacheMemBusExt) => new Composite(m.bus) {
    m.bus <> s.bus
  }
  }

  MultiInterconnectConnectFactory.AddHandler { case (m: InstructionCacheMemBusExt, s: XipBusExt) => new Composite(m.bus, "to_xip"){
    m.bus.cmd.map(c => {
      val wordShift = log2Up(c.address.getWidth / 8)
      val cmd = cloneOf(s.bus.cmd.payload)
      cmd.address := ((c.address >> wordShift) << wordShift).resized //(c.address << log2Up(m.bus.config.dataWidth / 8)).resized
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
    m.bus.toPipelinedMemoryBus() >> s.bus
  }}

  MultiInterconnectConnectFactory.AddHandler { case (m: PipelinedMemoryBusMultiBus, s: WishboneMultiBusInterface) => {
    val wb = PipelinedMemoryBusToWishbone(m.bus, s.bus.config)
    wb <> s.bus
  }}


  MultiInterconnectConnectFactory.AddHandler { case (m: DBusSimpleBusExt, s: WishboneMultiBusInterface) => new Composite(m.bus, "dbus_to_wb") with HasFormalProperties {
    val dbus_as_wb = m.bus.toWishbone()
    dbus_as_wb.connectToGranularity(s.bus, allowAddressResize = true, allowDataResize = true)
    if(dbus_as_wb.ERR != null && s.bus.ERR == null) {
      dbus_as_wb.ERR := False
    }

    override protected def formalProperties(): Seq[FormalProperty] = new DBusSimpleFormal(m.bus).contract.outstandingReads === 0
  }}

  MultiInterconnectConnectFactory.AddHandler { case (m: DBusSimpleBusExt, s: PipelinedMemoryBusMultiBus) => new Composite(m.bus, "dbus_to_pmb") with HasFormalProperties {
    m.bus.toPipelinedMemoryBus() >> s.bus
    m.bus.rsp.error := False

    override protected def formalProperties(): Seq[FormalProperty] = new DBusSimpleFormal(m.bus).contract.outstandingReads === s.bus.contract.outstandingReads.value.intoSInt
  }}
}