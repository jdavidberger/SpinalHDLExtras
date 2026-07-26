package spinalextras.lib.noc.protocols

import spinal.core.{Bits, Data, HardType, IntToBuilder}
import spinal.lib.{Fragment, Stream}
import spinalextras.lib.misc.StreamTools.CreateFragment
import spinalextras.lib.soc.spinex.plugins.BusType.PipelinedMemoryBus

import scala.collection.mutable.ArrayBuffer

class DataStreamSpecification[T <: Data](datatype: HardType[T], builder: NoCBuilder) extends ProtocolSpecification(builder) {
  private case class Source(hdr: Bits, address: NodeSlot, stream: Stream[Fragment[T]])
  private case class Sink(address: NodeSlot, stream: Stream[Fragment[T]])

  private val sources = new ArrayBuffer[Source]()
  private val sinks = new ArrayBuffer[Sink]()

  def addSource(hdr: Bits, address: Int = -1): Stream[Fragment[T]] = {
    val rtn = Stream(Fragment(datatype))
    sources.append(Source(hdr, builder.createInputSlot(address), rtn))
    rtn
  }

  def addSink(address: Int = -1): Stream[Fragment[T]] = {
    val rtn = Stream(Fragment(datatype))
    sinks.append(Sink(builder.createOutputSlot(address), rtn))
    rtn
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
      o.map(x => CreateFragment(x.fragment.as(datatype), x.last)) >> k.stream
      builder.addOutput(o, k.address.resolvedAddress)
    }
  }
}
