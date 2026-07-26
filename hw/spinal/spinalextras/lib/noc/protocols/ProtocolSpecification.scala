package spinalextras.lib.noc.protocols


import spinal.core._
import spinal.lib._
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusInterconnect}
import spinal.lib.misc.aia.APlicGenParam.test
import spinalextras.lib.Config
import spinalextras.lib.misc.StreamTools.CreateFragment
import spinalextras.lib.noc.{Header, NoC, NocConfig}
import spinalextras.lib.misc.{StreamFragmentWidthAdapterEncoding, StreamTools}
import spinalextras.lib.noc.topology.Mesh

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class ProtocolSpecification(builder : NoCBuilder) {
  builder.addSpecification(this)

  /** Called once, for every specification sharing a builder, before any address is auto-assigned --
    * override to declare `builder.requireRoute` constraints between slots this specification has
    * already created (e.g. via `createInputSlot`/`createOutputSlot`), so auto-addressing can take
    * them into account. */
  def registerRoutes(): Unit = {}

  def build() = {}
}





class TestDesign extends Component {
  val cfg = NocConfig(new Mesh(2, 2))
  val builder = new NoCBuilder(cfg)

  val spec1 = new DataStreamSpecification(SInt(24 bit), builder)
  val spec2 = new DataStreamSpecification(SInt(20 bit), builder)

  val sink = spec1.addSink(0)
  val header = B(0, 24 bits)
  val src = spec1.addSource(header, 1)

  val sink2 = spec2.addSink(2)
  val header2 = B(2, 20 bits)
  val src2 = spec2.addSource(header2, 3)

  sink.map(x => CreateFragment(x.fragment.resize(20 bits), x.last)) >> src2

  src.payload.fragment := 123
  src.valid := True
  src.last := True

  sink2.ready := True

  val noc = builder.build()
}


object TestDesign extends App {
  val report = Config.spinal.generateVerilog(new TestDesign())
}

