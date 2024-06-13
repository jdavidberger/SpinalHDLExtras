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
    val checkHandlers : Seq[HandlerFunction] =
      if (spinal.core.GlobalData.get.config.flags.contains(GenerationFlags.simulation))
        Array(simulationHandler) ++ handlers
      else
        handlers

    for (handler <- checkHandlers) {
      val factoryFunc = handler.lift(spinal.core.GlobalData.get.config.device)
      if (factoryFunc.nonEmpty) {
        return (factoryFunc.get)(args)
      }
    }

    null
  }
}
