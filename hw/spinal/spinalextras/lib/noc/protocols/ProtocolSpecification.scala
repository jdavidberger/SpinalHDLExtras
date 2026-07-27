package spinalextras.lib.noc.protocols


import spinalextras.lib.noc.NoCBuilder

import scala.language.postfixOps

class ProtocolSpecification(builder : NoCBuilder) {
  builder.addSpecification(this)

  /** Called once, for every specification sharing a builder, before any address is auto-assigned --
    * override to declare `builder.requireRoute` constraints between slots this specification has
    * already created (e.g. via `createInputSlot`/`createOutputSlot`), so auto-addressing can take
    * them into account. */
  def registerRoutes(): Unit = {}

  def build(): Unit = {}
}
