package spinalextras.lib.formal.fillins

import spinal.core._

import scala.collection.mutable


trait HasDefinedEquivalence {
  type internal_type = this.type

  def IsEquivalent(b : this.type ): Bool
}

object EquivalenceRegistry {
  type EquivalenceFunction = PartialFunction[(Data, Data), Bool]
  private var handlers = mutable.ArrayBuffer[EquivalenceFunction]()

  def AddEquivalenceHandler(f: EquivalenceFunction): this.type = {
    handlers.append(f)
    this
  }

  def Check[T <: Data](a : T, b : T) : Bool = {
    for ((v1, v2) <- Seq((a, b), (findFillin(a), findFillin(b)))) {
      v1 match {
        case eq1 : HasDefinedEquivalence =>
          return eq1.IsEquivalent(v2.asInstanceOf[eq1.internal_type])
        case _ =>
      }
    }

    for (handler <- handlers) {
      val factoryFunc = handler.lift(a, b)
      if(factoryFunc.isDefined){
        return factoryFunc.get
      }
    }

    for ((v1, v2) <- Seq((a, b), (findFillin(a), findFillin(b)))) {
      v1 match {
        case b : Bundle => {
          val b2 = v2.asInstanceOf[Bundle]
          return b.elements.map(x => {
            val el = b2.elements.find(y => y._1 == x._1)
            EquivalenceRegistry.Check(x._2, el.get._2)
          }).fold(True)((z1,z2) => z1 && z2)
        }
        case _ =>
      }
    }
    a === b
  }

}