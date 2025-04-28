package spinalextras.lib.formal.fillins

import spinal.core._

import scala.collection.mutable

object EquivalenceRegistry {
  type EquivalenceFunction = PartialFunction[(Data, Data), Bool]
  private var handlers = mutable.ArrayBuffer[EquivalenceFunction]()

  def AddEquivalenceHandler(f: EquivalenceFunction): this.type = {
    handlers.append(f)
    this
  }

  def Check[T <: Data](a : T, b : T) : Bool = {
    for (handler <- handlers) {
      val factoryFunc = handler.lift(a, b)
      if(factoryFunc.isDefined){
        return factoryFunc.get
      }
    }

    a === b
  }

}