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
