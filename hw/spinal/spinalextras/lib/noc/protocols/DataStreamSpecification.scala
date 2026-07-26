package spinalextras.lib.noc.protocols

import spinal.core.{Bits, Data, HardType, IntToBuilder}
import spinal.lib.{Fragment, Stream}
import spinalextras.lib.misc.StreamTools.CreateFragment
import spinalextras.lib.soc.spinex.plugins.BusType.PipelinedMemoryBus

import scala.collection.mutable.ArrayBuffer

class DataStreamSpecification[T <: Data](datatype: HardType[T], builder: NoCBuilder) extends ProtocolSpecification(builder) {
  val sources = new ArrayBuffer[(Bits, Int, Stream[Fragment[T]])]()
  val sinks = new ArrayBuffer[(Int, Stream[Fragment[T]])]()

  def addSource(hdr: Bits, address: Int = -1) = {
    val rtn = Stream(Fragment(datatype))
    sources.append((hdr, address, rtn))
    rtn
  }

  def addSink(address: Int) = {
    val rtn = Stream(Fragment(datatype))
    sinks.append((address, rtn))
    rtn
  }

  override def build(): Unit = {
    for (s <- sources) {
      builder.addInput(s._3.map(x => CreateFragment(x.fragment.asBits, x.last)).insertHeader(s._1), s._2)
    }
    for (s <- sinks) {
      val o = Stream(Fragment(Bits(s._2.fragment.getBitsWidth bits)))
      o.map(x => CreateFragment(x.fragment.as(datatype), x.last)) >> s._2
      builder.addOutput(o, s._1)
    }
  }
}
