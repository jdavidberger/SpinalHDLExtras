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

  private var fillinInstances : mutable.WeakHashMap[Data, Any] = mutable.WeakHashMap.empty
  def findFillin(data : Data): Any = {
    if (fillinInstances.contains(data)) {
      return fillinInstances(data)
    }

    for (handler <- handlers) {
      val factoryFunc = handler.lift(data)
      if (factoryFunc.nonEmpty) {
        println(s"Turning ${data} -> ${factoryFunc.get}")
        fillinInstances(data) = factoryFunc.get
        return factoryFunc.get
      }
    }

    data
  }

  fillins.AddHandler { case bus: Wishbone => Wishbone.WishboneFormalExt(bus) }
  fillins.AddHandler { case stream: Stream[Data] => StreamFormal.StreamExt(stream) }
  fillins.AddHandler { case bus: PipelinedMemoryBus => PipelinedMemoryBusFormal.PipelinedMemoryBusFormalExt(bus) }
}