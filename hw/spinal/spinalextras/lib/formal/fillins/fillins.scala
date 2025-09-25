package spinalextras.lib.formal

import spinal.core.{Area, Component, Data}
import spinal.lib.bus.amba4.axi.Axi4
import spinal.lib.{Counter, Fragment, Stream}
import spinal.lib.bus.simple.PipelinedMemoryBus
import spinal.lib.bus.wishbone.Wishbone

import scala.collection.mutable


package object fillins {
  type HandlerFunction = PartialFunction[Any, Any]

  private var handlers = mutable.ArrayBuffer[HandlerFunction]()

  def AddHandler(f: HandlerFunction): this.type = {
    handlers.append(f)
    this
  }

  def findInternalFormalProperties(c : Component): Set[HasFormalProperties] = {
    val rtn = new mutable.HashSet[HasFormalProperties]()
    c.dslBody.walkStatements {
      case d: Data => {
        val t = findFillin(d.refOwner, null)
        if (t != null && t.isInstanceOf[HasFormalProperties]) {
          rtn += t.asInstanceOf[HasFormalProperties]
        }
      }
      case _ => {}
    }

    rtn.toSet
  }

  private var fillinInstances : mutable.WeakHashMap[Any, Any] = mutable.WeakHashMap.empty
  def findFillin(data : Any, orElse : Any): Any = {
    if (fillinInstances.contains(data)) {
      return fillinInstances(data)
    }

    for (handler <- handlers) {
      val factoryFunc = handler.lift(data)
      if (factoryFunc.nonEmpty) {
        //println(s"Turning ${data} -> ${factoryFunc.get}")
        fillinInstances(data) = factoryFunc.get
        return factoryFunc.get
      }
    }

    orElse
  }

  def findFillin(data : Any) : Any = findFillin(data, data)

  fillins.AddHandler { case bus: Wishbone => Wishbone.WishboneFormalExt(bus) }
  fillins.AddHandler { case stream: Stream[Data] => StreamFormal.StreamExt(stream) }
  fillins.AddHandler { case fragment: Fragment[Data] => FragmentFormal.FragmentExt(fragment) }
  fillins.AddHandler { case bus: PipelinedMemoryBus => PipelinedMemoryBusFormal.PipelinedMemoryBusFormalExt(bus) }
  fillins.AddHandler { case counter: Counter => CounterFormalExt(counter) }
  fillins.AddHandler { case bus: Axi4 => Axi4Formal.Axi4FormalExt(bus) }
}