package spinalextras.lib.noc.protocols

import spinal.core.{B, Bits, Data, HardType, IntToBuilder}
import spinal.lib.bus.regif.AccessType.RW
import spinal.lib.bus.regif.BusIf
import spinal.lib.{Fragment, Stream}
import spinalextras.lib.misc.{RegisterTools, StreamTools}
import spinalextras.lib.misc.StreamTools.CreateFragment
import spinalextras.lib.noc.{NoCBuilder, NodeSlot}
import spinalextras.lib.soc.{BusIfDeviceTreeProvider, DeviceTree}
import spinalextras.lib.soc.spinex.plugins.BusType.PipelinedMemoryBus

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class DataStreamSpecification[T <: Data](datatype: HardType[T], builder: NoCBuilder) extends ProtocolSpecification(builder) {
  protected case class Source(hdr: Bits, address: NodeSlot, stream: Stream[Fragment[T]])
  protected case class Sink(address: NodeSlot, stream: Stream[Fragment[T]])

  protected val sources = new ArrayBuffer[Source]()
  protected val sinks = new ArrayBuffer[Sink]()

  /** @return the source's driveable stream, and the NodeSlot it injects from -- read
    *         `.resolvedAddress` only after `NoCBuilder.build()` (e.g. to address a sink whose own
    *         address was auto-assigned). */
  def addSource(hdr: Bits, address: Int): Stream[Fragment[T]] = {
    val rtn = Stream(Fragment(datatype))
    val slot = builder.createInputSlot(address)
    sources.append(Source(hdr, slot, rtn))
    rtn
  }
  def addSource(hdr: Bits): Stream[Fragment[T]] = addSource(hdr, -1)
  def sourceSlot(s : Stream[Fragment[T]]) = sources.find(_.stream == s).map(_.address)

  /** @return the sink's readable stream, and the NodeSlot packets must be addressed to in order
    *         to reach it -- read `.resolvedAddress` only after `NoCBuilder.build()`. */
  def addSink(address: Int = -1): Stream[Fragment[T]] = {
    val rtn = Stream(Fragment(datatype))
    val slot = builder.createOutputSlot(address)
    sinks.append(Sink(slot, rtn))
    rtn
  }
  def sinkSlot(s : Stream[Fragment[T]]) = sinks.find(_.stream == s).map(_.address)

  // A source's destination is baked into the header bits the caller supplies, entirely opaque to
  // this specification, so any source could in principle address any sink -- assume full
  // connectivity between every source and every sink by default, the same way
  // PipelinedMemoryBusSpecification does for masters and slaves.
  override def registerRoutes(): Unit = {
    for (s <- sources; k <- sinks) {
      builder.requireRoute(s.address, k.address)
    }
  }

  override def build(): Unit = {
    for (s <- sources) {
      builder.addInput(s.stream.map(x => CreateFragment(x.fragment.asBits, x.last)).insertHeader(s.hdr.resized).setName(s"${s.stream.name}Packet"), s.address.resolvedAddress)
    }
    for (k <- sinks) {
      val o = Stream(Fragment(Bits(k.stream.fragment.getBitsWidth bits)))
      // The fabric delivers the routing header flit as an ordinary leading fragment, same as it
      // does for every other NoC consumer (see PipelinedMemoryNocSlave/Master) -- strip it here so
      // the caller-visible sink stream only ever sees real payload beats.
      val (_, payload) = StreamTools.takeHead(o)
      payload.map(x => CreateFragment(x.fragment.as(datatype), x.last)) >> k.stream
      builder.addOutput(o.setName(k.stream.name), k.address.resolvedAddress)
    }
  }
}

class DataStreamSpecificationWithRegisters[T <: Data](val datatype: HardType[T], builder: NoCBuilder, busIf : BusIf) extends DataStreamSpecification(datatype, builder) {
  val initRoutes = new mutable.HashMap[NodeSlot, NodeSlot]()
  val sourceRegisters = new mutable.HashMap[NodeSlot, Bits]()

  def setInitRoute(src : Stream[Fragment[T]], dst: Stream[Fragment[T]]): Unit = {
    initRoutes.update(sourceSlot(src).get, sinkSlot(dst).get)
  }
  def addSource(name : String): Stream[Fragment[T]] = {
    this.addSource(name, -1)
  }
  def addSource(name : String, address: Int): Stream[Fragment[T]] = {
    val header_reg = RegisterTools.Register(busIf, f"${name}_hdr", B(0, 32 bits))
    val stream = this.addSource(header_reg, address)
    sourceRegisters.update(this.sourceSlot(stream).get, header_reg)
    stream.setName(name)
  }

  def addSourceWithInit(name : String, dst : Stream[Fragment[T]]): Stream[Fragment[T]] = {
    addSourceWithInit(name, dst, -1)
  }

  def addSourceWithInit(name : String, dst : Stream[Fragment[T]], address : Int): Stream[Fragment[T]] = {
    val src = addSource(name, address)
    if(dst != null)
      setInitRoute(src, dst)
    src
  }

  def addSink(name : String, address: Int): Stream[Fragment[T]] = super.addSink(address).setName(name)
  def addSink(name : String): Stream[Fragment[T]] = addSink(name, -1).setName(name)

  override def build(): Unit = {
    super.build()

    for (elem <- initRoutes) {
      println(f"Initial route for ${builder.cfg.topology.addressName(elem._1.resolvedAddress)} -> ${builder.cfg.topology.addressName(elem._2.resolvedAddress)}")
      sourceRegisters(elem._1).init(builder.cfg.topology.routeableAddressToAddress(elem._2.resolvedAddress))
    }

    val deviceTreeProvider = new BusIfDeviceTreeProvider(busIf) {
      override def compatible: Seq[String] = Seq("spinex,data-stream-noc")

      override def appendDeviceTree(dt: DeviceTree): Unit = {
        super.appendDeviceTree(dt)
        dt.addEntry(s"sink-names = ${sinks.map(m => '"' + m.stream.name + '"').mkString(",\n            ")};", baseEntryPath:_*)
        dt.addEntry(s"sink-addresses = < ${sinks.map(m => '"' + builder.cfg.topology.routeableAddressToAddress(m.address.resolvedAddress) + '"').mkString("\n            ")} > ;", baseEntryPath:_*)
      }
    }
  }
}