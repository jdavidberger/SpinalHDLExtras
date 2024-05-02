package spinalextras.lib.impl

import spinal.core.{Device, GenerationFlags}

import scala.collection.mutable

class ImplementationSpecificFactory[T >: Null, Args] {
  type HandlerFunction = PartialFunction[Device, Args => T]

  private var handlers = mutable.ArrayBuffer[HandlerFunction]()
  var simulationHandler : HandlerFunction = Map.empty

  def AddHandler(f: HandlerFunction): this.type = {
    handlers.append(f)
    this
  }

  def apply(args: Args): T = {
    val checkHandlers =
      if (spinal.core.GlobalData.get.config.flags.contains(GenerationFlags.simulation))
        handlers += simulationHandler
      else
        handlers

    for (handler <- checkHandlers.reverse) {
      val factoryFunc = handler.lift(spinal.core.GlobalData.get.config.device)
      if (factoryFunc.nonEmpty) {
        return (factoryFunc.get)(args)
      }
    }
    null
  }
}
