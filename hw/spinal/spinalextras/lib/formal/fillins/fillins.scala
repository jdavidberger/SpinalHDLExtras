package spinalextras.lib.formal

import spinal.core.Data
import spinal.lib.Stream
import spinal.lib.bus.simple.PipelinedMemoryBus
import spinal.lib.bus.wishbone.Wishbone

import scala.collection.mutable


package object fillins {
  type HandlerFunction = PartialFunction[Data, Any]

  private var handlers = mutable.ArrayBuffer[HandlerFunction]()

  def AddHandler(f: HandlerFunction): this.type = {
    handlers.append(f)
    this
  }

  def findFillin(data : Data): Any = {
    for (handler <- handlers) {
      val factoryFunc = handler.lift(data)
      if (factoryFunc.nonEmpty) {
        return factoryFunc.get
      }
    }

    data
  }

  fillins.AddHandler { case bus: Wishbone => Wishbone.WishboneFormalExt(bus) }
  fillins.AddHandler { case stream: Stream[Data] => StreamFormal.StreamExt(stream) }
  fillins.AddHandler { case bus: PipelinedMemoryBus => PipelinedMemoryBusFormal.PipelinedMemoryBusFormalExt(bus) }
}