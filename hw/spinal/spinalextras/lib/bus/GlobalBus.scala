package spinalextras.lib.bus

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._
import spinal.lib.bus.regif._
import spinal.lib.bus.simple._
import spinal.lib.bus.wishbone._
import spinalextras.lib.bus.simple.PipelinedMemoryBusInterface
import spinalextras.lib.logging.{GlobalLogger, PipelinedMemoryBusLogger, SignalLogger, WishboneBusLogger}
import spinalextras.lib.testing.test_funcs

import scala.collection.mutable

trait GlobalBus[T <: IMasterSlave with Nameable with Bundle] {
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

  def add_interface(name : String, dir : T => T ): (T, T) = {
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

    val newMapping = mapping match {
      case SizeMapping(base, size) => {
        if((base & (size - 1)) == 0) {
          MaskMapping(base, ((1L << addr_width()) - 1) & ~(size - 1))
        } else {
          mapping
        }
      }
      case _ => mapping
    }

    slaves.foreach(s => {
      //println(s"Checking ${s._2} vs ${newMapping}")
      val no_overlap = !s._2.hit(newMapping.lowerBound) && !newMapping.hit(s._2.lowerBound)
      if(!no_overlap) {
        println("Invalid memory settings, overlap between two slave devices:")
        println(s"${s} overlaps with ${name} (${newMapping})")
      }
      require(no_overlap)
    })

    slaves.append((topBus, newMapping, Set(tags:_*)))

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
    busIf.regPtrReAnchorAt(mapping.base)
    busIf
  }
  def add_bus_interface(name: String, mapping: SizeMapping, tags: String*): BusIf = {
    add_bus_interface(name, mapping, false, false, tags:_*)
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

  val preBuildTasks = new mutable.ArrayBuffer[() => Unit]()
  def addPreBuildTask(task : () => Unit) {
    preBuildTasks.append(task)
  }
  def build(): Unit = {
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
      if(x.ERR != null) {
        x.ERR := False
        x.ERR.allowOverride()
      }
    }
    x
  }
  def stage_bus(bus : Wishbone) : Wishbone = {
    WishboneStage(bus)
  }
  override def bus_interface(port : Wishbone, mapping: SizeMapping) : WishboneBusInterface = WishboneBusInterface(port, mapping)
  override def slave_factory(port : Wishbone) = WishboneSlaveFactory(port)

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
    if(x.ERR != null) {
      x.ERR := False
      x.ERR.allowOverride()
    }
    slave(x)
  }

}
object PipelineMemoryGlobalBus {
  def Default = PipelineMemoryGlobalBus(PipelinedMemoryBusConfig(30,32))
}

case class PipelineMemoryGlobalBus(config : PipelinedMemoryBusConfig) extends GlobalBus[PipelinedMemoryBus] {
  override def create_bus(): PipelinedMemoryBus = PipelinedMemoryBus(config)

  def addr_width() = config.addressWidth
  override def build(): Unit = {
    println("System Bus Masters")
    built = true
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
//
//    def directWithFormal(m : PipelinedMemoryBus, s : PipelinedMemoryBus) : Unit = {
//      val mContract = test_funcs.assertPMBContract(m).setName(s"c${m.name}_gb_contract")
//      val sContract = test_funcs.assertPMBContract(s).setName(s"c${s.name}_gb_contract")
//      println(s"Associating ${m}(${mContract.name}) with ${s}(${sContract.name})")
//      assert(mContract.outstanding_cnt === sContract.outstanding_cnt)
//      m >> s
//    }
//
//    for(m <- interconn.masters) {
//      m._2.connector = directWithFormal
//    }
//    for(s <- interconn.slaves) {
//      s._2.connector = directWithFormal
//    }
//    for(c <- interconn.connections) {
//      c.connector = directWithFormal
//    }
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
