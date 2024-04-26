package spinalextras.lib.misc



import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._
import spinal.lib.bus.regif._
import spinal.lib.bus.simple._
import spinal.lib.bus.wishbone._

import scala.collection.mutable

trait GlobalBus[T <: IMasterSlave with Nameable with Bundle] {
  var masters = mutable.ArrayBuffer[(T, Set[String])]()
  var slaves = mutable.ArrayBuffer[(T, AddressMapping, Set[String])]()

  var topComponent = {
    val topComponent = Component.toplevel

    topComponent.addPrePopTask(() => {
      this.build()
    })

    topComponent
  }

  type direction_function = T => T

  def create_bus() : T
  def create_directed_bus(name : String, dir : direction_function): T = {
    var bus = create_bus().setName(name)
    if (Component.current != topComponent) {
      bus = dir(bus)
    }
    bus
  }

  def add_interface(name : String, dir : T => T ): (T, T) = {
    if(Component.current == topComponent) {
      var new_bus = create_bus().setName(name)
      return (new_bus,new_bus)
    }

    var intermediate_bus : Option[T] = None
    var new_bus : Option[T] = None

    var c = Component.current
    do {
      val ctx = Component.push(c)

      val next_bus = create_directed_bus(name, dir)

      if(new_bus.isEmpty) {
        new_bus = Some(next_bus)
      }

      intermediate_bus.foreach(x => x <> next_bus)

      intermediate_bus = Some(next_bus)
      ctx.restore()
      c = c.parent
    } while(c != topComponent && c != null)
    assert(c != null)

    (intermediate_bus.get, new_bus.get)
  }

  def add_master(name : String, tags : Set[String] = Set()) : T = {
    val (topBus, rtn) = add_interface(name, master_direction)
    masters.append((topBus, tags))
    rtn
  }

  def add_slave(name : String, mapping : AddressMapping, tags : String*) : T = {
    val (topBus, rtn) = add_interface(name, slave_direction)

    val newMapping = mapping match {
      case SizeMapping(base, size) => {
        if((base & (size - 1)) == 0) {
          MaskMapping(base, 0xffffffffL & ~(size - 1))
        } else {
          mapping
        }
      }
      case _ => mapping
    }

    slaves.append((topBus, newMapping, Set(tags:_*)))

    rtn
  }

  def slave_direction: direction_function = x => slave(x)
  def master_direction: direction_function = x => master(x)

  def bus_interface(port : T, addressMapping: SizeMapping) : BusIf
  def slave_factory(port : T) : BusSlaveFactory


  def add_bus_interface(name: String, mapping: SizeMapping, tags: String*): BusIf = {
    val port = add_slave(name, mapping, tags:_*)
    bus_interface(port, mapping)
  }

  def add_slave_factory(name: String, mapping: SizeMapping, tags: String*): BusSlaveFactory = {
    val port = add_slave(name, mapping, tags:_*)
    slave_factory(port)
  }


  def interconnect_spec() : Seq[(T, Seq[T])] = {
    var slaves_without_arbiters = slaves.map(_._1).toSet

    val rtn = masters.map {
      case (m, tags) => {
        println(s"\t${m.name} ${tags}")

        val connected_slaves = slaves.filter { case (s, mapping, slave_tags) =>
          tags.isEmpty || slave_tags.isEmpty || slave_tags.intersect(tags).nonEmpty
        }.map { case (s, mapping, slave_tags) =>
          val mappingName = mapping match {
            case MaskMapping(base, mask) => s"MaskMapping(0x${base.toString(16)}, 0x${mask.toString(16)})"
            case SizeMapping(base, size) => s"SizeMapping(0x${base.toString(16)}, 0x${size.toString(16)})"
            case _ => mapping.toString
          }
          println(s"\t\t%-64s ${mappingName} ${slave_tags}".format(s.name))
          s
        }

        slaves_without_arbiters = slaves_without_arbiters.diff(connected_slaves.toSet)

        (m, connected_slaves)
      }
    }

    for(s <- slaves_without_arbiters) {
      println(s"${s} has no master")
    }

    rtn
  }

  def build() : Unit
}

trait GlobalBusFactory[T <: IMasterSlave with Nameable with Bundle] {
  type GlobalBus_t = GlobalBus[T]
  def create_global_bus() : GlobalBus_t
  var sysBus : Option[GlobalBus_t] = None
  def apply(): GlobalBus_t = {
    if(sysBus.isEmpty || sysBus.get.topComponent != Component.toplevel) {
      sysBus = Some(create_global_bus())
    }
    sysBus.get
  }
}

case class WishboneGlobalBus(config : WishboneConfig) extends GlobalBus[Wishbone] {

  override def create_bus(): Wishbone = {
    Wishbone(config)
  }

  override def bus_interface(port : Wishbone, mapping: SizeMapping) = WishboneBusInterface(port, mapping)
  override def slave_factory(port : Wishbone) = WishboneSlaveFactory(port)

  def build(): Unit = {
    println("System Bus Masters")

    if(masters.isEmpty) {
      val m = add_master("filler")
      m.WE := False
      if(m.SEL != null)
        m.SEL := 0
      m.CYC := False
      m.STB := False
      m.ADR := 0
      m.DAT_MOSI := 0
    }

    val ctx = Component.push(Component.toplevel)
    val wbInterconn = WishboneInterconFactory()

    for((topBus, mapping, tags) <- slaves) {
      wbInterconn.addSlave(topBus, mapping)
    }

    wbInterconn.addMasters(interconnect_spec():_*)

    ctx.restore()
  }

  override def slave_direction: direction_function = x => {
    if(x.ERR != null) {
      x.ERR := False
      x.ERR.allowOverride()
    }
    slave(x)
  }

}

case class PipelineMemoryGlobalBus(config : PipelinedMemoryBusConfig) extends GlobalBus[PipelinedMemoryBus] {
  override def create_bus(): PipelinedMemoryBus = PipelinedMemoryBus(config)

  override def build(): Unit = {
    println("System Bus Masters")

    val ctx = Component.push(Component.toplevel)
    val interconn = PipelinedMemoryBusInterconnect()

    for((topBus, mapping, tags) <- slaves) {
      interconn.addSlave(topBus, mapping)
    }

    interconn.addMasters(interconnect_spec():_*)

    ctx.restore()
  }

  override def bus_interface(port: PipelinedMemoryBus, addressMapping: SizeMapping): BusIf = ???

  override def slave_factory(port: PipelinedMemoryBus): BusSlaveFactory = ???
}
