package spinalextras.lib.formal

import spinal.core.internals.DataAssignmentStatement
import spinal.core.{Area, Bundle, Component, Data, TupleBundle2}
import spinal.lib.bus.amba4.axi.{Axi4, Axi4ReadOnly, Axi4Shared, Axi4WriteOnly}
import spinal.lib.{Counter, Fragment, IMasterSlave, Stream, StreamFork, StreamForkArea}
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusArbiter}
import spinal.lib.bus.wishbone.Wishbone
import spinal.lib.fsm.{StateFsm, StateMachine}

import scala.collection.mutable


package object fillins {
  type HandlerFunction = PartialFunction[Any, Any]

  private var handlers = mutable.ArrayBuffer[HandlerFunction]()

  def AddHandler(f: HandlerFunction): this.type = {
    handlers.append(f)
    this
  }
  def findInternalFormalProperties(a : Area): Set[HasFormalProperties] = {
    val fillin = findFillin(a, a)
    if (fillin != null && fillin.isInstanceOf[HasFormalProperties]) {
      return Set(fillin.asInstanceOf[HasFormalProperties])
    }


    val rtn = new mutable.HashSet[HasFormalProperties]()
    a.foreachReflectableNameables {
      case inner_a: Area => {
        rtn ++= findInternalFormalProperties(inner_a)
      }
      case _ => {
      }
    }

    rtn.toSet
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
      case _ => {
      }
    }

    c.children.foreach(ch => {
      val t = findFillin(ch, ch)
      if (t != null && t.isInstanceOf[HasFormalProperties]) {
        rtn += t.asInstanceOf[HasFormalProperties]
      }

      rtn ++= findInternalFormalProperties(ch)
    })

    c.foreachReflectableNameables {
      case a: Area => {
        rtn ++= findInternalFormalProperties(a)
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

    if(!data.isInstanceOf[FormalData]) {
      data match {
        case ms : Bundle with IMasterSlave =>
          return new IMasterSlaveExt(ms)
        case bundle: Bundle =>
          return new BundleExt(bundle)
        case _ =>
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
  fillins.AddHandler { case bus: Axi4Shared => Axi4Formal.Axi4SharedFormalExt(bus) }
  fillins.AddHandler { case bus: Axi4ReadOnly => Axi4Formal.Axi4ReadOnlyExt(bus) }
  fillins.AddHandler { case bus: Axi4WriteOnly => Axi4Formal.Axi4WriteOnlyExt(bus) }
  fillins.AddHandler { case fsm: StateMachine => new StateMachineFormal(fsm) }
  fillins.AddHandler { case fork: StreamFork[Data] => new StreamForkFormal[Data](fork) }
  fillins.AddHandler { case fsm: StateFsm[_] => new StateFsmFormal(fsm) }
}