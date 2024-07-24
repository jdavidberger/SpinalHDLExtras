package spinalextras.lib.misc

import spinal.core.{Analog, BaseType, Bundle, ClockDomain, ClockingArea, Component, Data, cloneOf, in, inout, out}
import spinal.lib.{IMasterSlave, growableAnyPimped}

import scala.collection.mutable

object AutoInterconnect {
  def buildInterconnect(components: Iterable[Component], io : Bundle, renames: Map[String, String] = Map.empty): Unit = {
    val unhandled_ios = new mutable.HashMap[String, Data]()
    for (c <- components) {

      def hasAssignments(io: Data): Boolean = {
        if(io.isInstanceOf[Bundle]) {
          val assigned = io.asInstanceOf[Bundle].elements.map(_._2).filter(x => x.isInput || x.isInstanceOf[Bundle]).map(hasAssignments)
          assigned.headOption.getOrElse(false)
        } else if(io.isInput) {
          io match {
            case baseType: BaseType => !baseType.dlcIsEmpty
            case _ => false
          }
        } else {
          false
        }
      }

      val c_ios = c.getGroupedIO(true).filter(io => !hasAssignments(io) && io.parent != null && (io.parent.name == "" || io.parent.name == "io"))
      for (io <- c_ios) {
        val name = renames.getOrElse(io.name, io.name)
        if (unhandled_ios.contains(name)) {
          //println(s"Connecting ${name}")
          io <> unhandled_ios(name)
          unhandled_ios.remove(name)
        } else {
          //println(s"Queueing ${name}")
          require(name != null && io != null)
          unhandled_ios.addRet(name, io)
        }
      }
    }

    for (unhandled_io <- unhandled_ios.toList.sortBy(_._1).map(_._2)) {
      val signal: Data = unhandled_io.getDirection match {
        case spinal.core.in => in(unhandled_io.clone())
        case spinal.core.out => out(unhandled_io.clone())
        case spinal.core.inout => inout(Analog(unhandled_io.clone()))
        case null =>
          val masterSlave = unhandled_io.asInstanceOf[IMasterSlave]
          if (masterSlave != null) {
            if (masterSlave.isMasterInterface) {
              cloneOf(unhandled_io).asInstanceOf[IMasterSlave].intoMaster().asInstanceOf[Data]
            } else {
              unhandled_io.clone().asInstanceOf[IMasterSlave].intoSlave().asInstanceOf[Data]
            }
          } else {
            null
          }
      }

      if (unhandled_io.isAnalog)
        signal.setAsAnalog()

      signal.parent = io
      signal.setName(unhandled_io.name) <> unhandled_io
      signal.flatten.zip(unhandled_io.flatten).foreach(
        x => x._1.setName(x._2.getName())
      )
    }

  }
  def apply(name: String, contents: () => Iterator[Component],
            renames: Map[String, String] = Map.empty, clockDomain : => ClockDomain = ClockDomain.current): Component = {
    val componentName = name
    lazy val customClockDomain = clockDomain
    new Component {
      setName(componentName)
      val io = new Bundle {}
      val cdArea = new ClockingArea(clockDomain = customClockDomain) {
        buildInterconnect(contents().toIterable, io, renames)
      }
    }.setDefinitionName(name)
  }
}
