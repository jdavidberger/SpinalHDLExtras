package spinalextras.lib.noc.protocols

import spinal.core.{Bits, Data, HardType, IntToBuilder}
import spinal.lib.{Fragment, Stream}
import spinalextras.lib.misc.StreamTools
import spinalextras.lib.misc.StreamTools.CreateFragment
import spinalextras.lib.soc.spinex.plugins.BusType.PipelinedMemoryBus

import scala.collection.mutable.ArrayBuffer

class DataStreamSpecification[T <: Data](datatype: HardType[T], builder: NoCBuilder) extends ProtocolSpecification(builder) {
  private case class Source(hdr: Bits, address: NodeSlot, stream: Stream[Fragment[T]])
  private case class Sink(address: NodeSlot, stream: Stream[Fragment[T]])

  private val sources = new ArrayBuffer[Source]()
  private val sinks = new ArrayBuffer[Sink]()

  /** @return the source's driveable stream, and the NodeSlot it injects from -- read
    *         `.resolvedAddress` only after `NoCBuilder.build()` (e.g. to address a sink whose own
    *         address was auto-assigned). */
  def addSource(hdr: Bits, address: Int = -1): (Stream[Fragment[T]], NodeSlot) = {
    val rtn = Stream(Fragment(datatype))
    val slot = builder.createInputSlot(address)
    sources.append(Source(hdr, slot, rtn))
    (rtn, slot)
  }

  /** @return the sink's readable stream, and the NodeSlot packets must be addressed to in order
    *         to reach it -- read `.resolvedAddress` only after `NoCBuilder.build()`. */
  def addSink(address: Int = -1): (Stream[Fragment[T]], NodeSlot) = {
    val rtn = Stream(Fragment(datatype))
    val slot = builder.createOutputSlot(address)
    sinks.append(Sink(slot, rtn))
    (rtn, slot)
  }

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
      builder.addInput(s.stream.map(x => CreateFragment(x.fragment.asBits, x.last)).insertHeader(s.hdr), s.address.resolvedAddress)
    }
    for (k <- sinks) {
      val o = Stream(Fragment(Bits(k.stream.fragment.getBitsWidth bits)))
      // The fabric delivers the routing header flit as an ordinary leading fragment, same as it
      // does for every other NoC consumer (see PipelinedMemoryNocSlave/Master) -- strip it here so
      // the caller-visible sink stream only ever sees real payload beats.
      val (_, payload) = StreamTools.takeHead(o)
      payload.map(x => CreateFragment(x.fragment.as(datatype), x.last)) >> k.stream
      builder.addOutput(o, k.address.resolvedAddress)
    }
  }
}
