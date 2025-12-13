package spinalextras.lib.bus

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._
import spinal.lib.bus.regif._
import spinal.lib.bus.simple._
import spinal.lib.bus.wishbone._
import spinalextras.lib.bus.general.BusSlaveProvider
import spinalextras.lib.bus.simple.PipelinedMemoryBusInterface
import spinalextras.lib.logging.{GlobalLogger, PipelinedMemoryBusLogger, SignalLogger, WishboneBusLogger}
import spinalextras.lib.testing.test_funcs

import scala.collection.mutable

trait GlobalBus[T <: IMasterSlave with Nameable with Bundle] extends BusSlaveProvider {
  var masters = mutable.ArrayBuffer[(T, Set[String])]()
  var slaves = mutable.ArrayBuffer[(T, AddressMapping, Set[String])]()

  var built = false
  var topComponent = {
    val topComponent = Component.current

    Component.toplevel.addPrePopTask(() => {
      if(!built) {
        this.build()
      }
      built = true
    })
    topComponent
  }

  def cancel(): Unit = {
    built = true
  }


  type direction_function = T => T

  def addr_width() : Int
  def create_bus() : T
  def create_directed_bus(name : String, dir : direction_function): T = {
    var bus = create_bus().setName(name)
    if (Component.current != topComponent) {
      bus = dir(bus)
    }
    bus
  }
  def stage_bus(bus : T) : T

  val names = new mutable.HashMap[String, Int]()
  def unique_name(name : String): String = {
    val idx = names.getOrElse(name, 0)
    names.update(name, idx + 1)
    if(idx == 0) {
      name
    } else {
      s"${name}${idx}"
    }
  }

  def add_interface(rawName : String, dir : T => T ): (T, T) = {
    val name = unique_name(rawName)
    if(Component.current == Component.toplevel || Component.current == topComponent) {
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

  def add_master(name : String, tags : String*) : T = {
    assert(!built)
    add_master(name, Set(tags:_*))
  }
  def add_master(name : String, tags : Set[String] = Set()) : T = {
    val (topBus, rtn) = add_interface(name, master_direction)
    masters.append((topBus, tags))
    rtn
  }

  def add_slave(name : String, mapping : AddressMapping, tags : String*) : T = {
    val (topBus, rtn) = add_interface(name, slave_direction)

    slaves.foreach(s => {
      //println(s"Checking ${s._2} vs ${newMapping}")
      val no_overlap = mapping == DefaultMapping || s._2 == DefaultMapping || !s._2.hit(mapping.lowerBound) && !mapping.hit(s._2.lowerBound)
      if(!no_overlap) {
        println("Invalid memory settings, overlap between two slave devices:")
        println(s"${s} overlaps with ${name} (${mapping})")
      }
      require(no_overlap)
    })

    slaves.append((topBus, mapping, Set(tags:_*)))

    rtn
  }

  def slave_direction: direction_function = x => slave(x)
  def master_direction: direction_function = x => master(x)

  def bus_interface(port : T, addressMapping: SizeMapping) : BusIf
  def slave_factory(port : T) : BusSlaveFactory

  var shared_bus_interfaces = new mutable.ArrayBuffer[(SizeMapping, BusIf)]
  def add_shared_bus_interface(name: String, mapping: SizeMapping, tags: String*): BusIf = {
    val port = add_slave(name, mapping, tags:_*)
    val busIf = bus_interface(port, mapping)
    busIf.regPtrReAnchorAt(mapping.base)

    shared_bus_interfaces.append((mapping, busIf))

    busIf
  }

  def apply_staging(bus : T, m2s_stage : Boolean, s2m_stage : Boolean): T = {
    bus
  }

  def add_bus_interface(name: String, mapping: SizeMapping, m2s_stage : Boolean, s2m_stage : Boolean, tags: String*): BusIf = {
    val busIf = shared_bus_interfaces.find(_._1.overlap(mapping)).map(_._2).getOrElse({
      val port = apply_staging(add_slave(name, mapping, tags:_*), m2s_stage, s2m_stage)
      bus_interface(port, mapping)
    })
    busIf.setPartialName(name)
    busIf.regPtrReAnchorAt(mapping.base)
    busIf
  }
  override def add_bus_interface(name: String, mapping: SizeMapping, tags: String*): BusIf = {
    add_bus_interface(name, mapping, false, false, tags:_*)
  }

  def add_slave_factory(name: String, mapping: SizeMapping, m2s_stage : Boolean, s2m_stage : Boolean, tags: String*): BusSlaveFactory = {
    val port = apply_staging(add_slave(name, mapping, tags:_*), m2s_stage, s2m_stage)
    slave_factory(port)
  }

  def add_slave_factory(name: String, mapping: SizeMapping, tags: String*): BusSlaveFactory = {
    add_slave_factory(name, mapping = mapping, m2s_stage = false, s2m_stage = false, tags = tags:_*)
  }


  def interconnect_spec() : Seq[(T, Seq[T])] = {
    var slaves_without_arbiters = slaves.map(_._1).toSet

    val sortedSlaves = slaves.sortBy(x =>
        x._2 match {
          case DefaultMapping => 0
          case _ => -x._2.lowerBound.toInt
        }
    )

    val rtn = masters.map {
      case (m, tags) => {
        println(s"\t${m.name} ${tags}")

        val connected_slaves = sortedSlaves.filter { case (s, mapping, slave_tags) =>
          tags.isEmpty || slave_tags.isEmpty || slave_tags.intersect(tags).nonEmpty
        }.map { case (s, inputMapping, slave_tags) =>
          val mapping = spinalextras.lib.bus.to_mask_mapping(32, inputMapping)

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

  def build(): Unit = {
    built = true
    preBuildTasks.foreach(_())
  }
}

trait GlobalBusFactory[GBT <: GlobalBus[_]] {
  type GlobalBus_t = GBT
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
  def addr_width() = config.addressWidth

  override def create_bus(): Wishbone = {
    val x = Wishbone(config)
    if (Component.current == Component.toplevel) {
//      if(x.ERR != null) {
//        x.ERR := False
//        x.ERR.allowOverride()
//      }
    }
    x
  }
  def stage_bus(bus : Wishbone) : Wishbone = {
    WishboneStage(bus)
  }
  override def bus_interface(port : Wishbone, mapping: SizeMapping) : WishboneBusInterface = WishboneBusInterface(port, mapping)
  override def slave_factory(port : Wishbone) = {
    if(port.ERR != null)
      port.ERR := False
    WishboneSlaveFactory(port)
  }

  override def apply_staging(bus: Wishbone, m2s_stage: Boolean, s2m_stage: Boolean): Wishbone = {
    WishboneStage(bus, m2s_stage, s2m_stage)
  }

  override def build(): Unit = {
    {
      val ctx = Component.push(topComponent)
      GlobalLogger(
        tags = Set("memory"),
        WishboneBusLogger.flows(InvertMapping(SizeMapping(0xb9000000L, 1 KiB)), masters.map(_._1): _*),
      )
      ctx
    }.restore()

    super.build()
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

    val ctx = Component.push(topComponent)
    val wbInterconn = WishboneInterconFactory()

    val spec = interconnect_spec()
    val valid_slave_busses = spec.flatMap(_._2).toSet
    for((topBus, mapping, tags) <- slaves) {
      if(valid_slave_busses.contains(topBus))
        wbInterconn.addSlave(topBus, mapping)
      else {
        topBus.DAT_MOSI.assignDontCare()
        topBus.ADR.assignDontCare()
        topBus.CYC := False
        topBus.STB := False
        topBus.SEL.assignDontCare()
        topBus.WE.assignDontCare()
        if(topBus.CTI != null) topBus.CTI.assignDontCare()
        if(topBus.BTE != null) topBus.BTE.assignDontCare()
      }
    }

    wbInterconn.addMasters(spec:_*)

    ctx.restore()
  }

  override def slave_direction: direction_function = x => {
    slave(x)
  }

//  if(config.useERR) {
//    val missSlave = add_slave("decoder_miss", DefaultMapping)
//    if(missSlave.ERR != null)
//      missSlave.ERR := True
//    missSlave.ACK := True
//    missSlave.DAT_MISO := 0x9ABCDEF
//  }

}
object PipelineMemoryGlobalBus {
  def Default = PipelineMemoryGlobalBus(PipelinedMemoryBusConfig(32,32))
}

case class PipelineMemoryGlobalBus(config : PipelinedMemoryBusConfig) extends GlobalBus[PipelinedMemoryBus] {
  override def create_bus(): PipelinedMemoryBus = PipelinedMemoryBus(config)

  def addr_width() = config.addressWidth
  override def build(): Unit = {
    println(s"System Bus Masters")

    super.build()

    val ctx = Component.push(topComponent)
    val interconn = PipelinedMemoryBusInterconnect()
    interconn.perfConfig()
    interconn.arbitrationPendingRspMaxDefault = 16

    val spec = interconnect_spec()
    val valid_slave_busses = spec.flatMap(_._2).toSet
    for((topBus, mapping, tags) <- slaves) {
      if(valid_slave_busses.contains(topBus)) {
        interconn.addSlave(topBus, mapping)
      } else {
        topBus.cmd.setIdle()
      }
      //interconn.noTransactionLockOn(topBus)
    }

    interconn.addMasters(spec:_*)

    ctx.restore()
  }

  def attach_debug_registers(busIf: BusIf) = {
    PipelinedMemoryBusLogger.attach_debug_registers(busIf, this.masters.map(_._1):_*)
  }

  override def bus_interface(port: PipelinedMemoryBus, addressMapping: SizeMapping): BusIf = PipelinedMemoryBusInterface(port, addressMapping)

  override def slave_factory(port: PipelinedMemoryBus): BusSlaveFactory = ???

  override def stage_bus(bus: PipelinedMemoryBus): PipelinedMemoryBus = {
    bus
  }
}
