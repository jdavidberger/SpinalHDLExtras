package spinalextras.lib.bus

import spinal.core.{Bool, Component, MultiData, log2Up}
import spinal.lib.{IMasterSlave, master, slave}
import spinal.lib.bus.misc.{AddressMapping, BusSlaveFactory, DefaultMapping, MaskMapping, NeverMapping, SizeMapping}
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusConfig, PipelinedMemoryBusDecoder, PipelinedMemoryBusSlaveFactory}
import spinal.lib.bus.wishbone.{AddressGranularity, Wishbone, WishboneConfig, WishboneSlaveFactory}
import spinalextras.lib.bus
import spinalextras.lib.bus.bus.WishboneMultiBusInterface
import spinalextras.lib.bus.general.BusSlaveProvider

import scala.collection.mutable

trait MultiBusInterface {
  def bus : MultiData with IMasterSlave
  def address_width : Int
  def create_decoder(mappings : Seq[AddressMapping]) : Seq[MultiBusInterface]
  def create_arbiter(size : Int) : Seq[MultiBusInterface]

  def isValidProducer : Bool
  def isValidConsumer : Bool
}

object MultiInterconnectConnectFactory {
  type HandlerFunction = PartialFunction[(MultiBusInterface, MultiBusInterface), Any]

  private var handlers = mutable.ArrayBuffer[HandlerFunction]()

  def apply(m: MultiBusInterface, s: MultiBusInterface): Any = {
    for (handler <- handlers) {
      val factoryFunc = handler.lift((m,s))
      if(factoryFunc.nonEmpty)
        return factoryFunc.get
    }

    throw new MatchError(s"Could not find match to ${m} <> ${s}")
  }

  def AddHandler(f: HandlerFunction): this.type = {
    handlers.append(f)
    this
  }
}

class MultiInterconnect {
  case class SlaveModel(mapping : AddressMapping)
  case class ConnectionModel(m : MultiBusInterface, s : MultiBusInterface,
                             var connector : (MultiBusInterface, MultiBusInterface) => Unit = MultiInterconnectConnectFactory.apply)

  val masters = mutable.ArrayBuffer[MultiBusInterface]()
  val slaves = mutable.HashMap[MultiBusInterface, SlaveModel]()
  val connections = mutable.ArrayBuffer[ConnectionModel]()

  def addSlave(bus: MultiBusInterface, mapping: AddressMapping) : this.type = {
    for(s <- slaves.values) {
      assert(!s.mapping.hit(mapping.lowerBound), f"Memory map conflict ${s.mapping} vs ${mapping}")
      assert(!s.mapping.hit(mapping.highestBound), f"Memory map conflict ${s.mapping} vs ${mapping}")
    }

    slaves(bus) = SlaveModel(mapping)
    this
  }

  def addSlaves(orders : (MultiBusInterface, AddressMapping)*) : this.type = {
    orders.foreach(order => addSlave(order._1,order._2))
    this
  }

  def connect(m : MultiBusInterface, s : MultiBusInterface,
              connector : (MultiBusInterface, MultiBusInterface) => Unit = MultiInterconnectConnectFactory.apply): Unit = {
    connections += ConnectionModel(m, s, connector)
  }

  def addMaster(bus : MultiBusInterface, accesses : Seq[MultiBusInterface] = Nil) : this.type = {
    masters += bus
    for(s <- accesses) connections += ConnectionModel(bus, s)
    this
  }

  def create_decoder(bus : MultiBusInterface, mappings : Seq[AddressMapping]) = {
    if(mappings.size == 1 && mappings.head == DefaultMapping) {
      Seq(bus)
    } else {
      bus.create_decoder(mappings)
    }
  }

  def create_arbiter(bus : MultiBusInterface, ports : Int) = {
    if(ports <= 1) {
      Seq(bus)
    } else {
      bus.create_arbiter(ports)
    }
  }

  def to_mask_mapping(bus: MultiBusInterface)(mapping: AddressMapping): AddressMapping = {
    spinalextras.lib.bus.to_mask_mapping(bus.address_width, mapping)
  }

  def build(): Unit = {
    val connectionsInput  = mutable.HashMap[ConnectionModel, MultiBusInterface]()
    val connectionsOutput = mutable.HashMap[ConnectionModel, MultiBusInterface]()

    for(bus <- masters){
      val busConnections = connections.filter(_.m == bus)
      val busSlaves = busConnections.map(c => slaves(c.s))
      val decodedOutputs = create_decoder(bus, busSlaves.map(_.mapping).map(to_mask_mapping(bus)))

      for((connection, decoderOutput) <- (busConnections, decodedOutputs).zipped) {
        connectionsInput(connection) = decoderOutput
      }
    }

    for((bus, _) <- slaves){
      val busConnections = connections.filter(_.s == bus)
      val busMasters = busConnections.map(_.m)
      val arbitratedInputs = create_arbiter(bus, busMasters.size)

      for((connection, arbiterInput) <- (busConnections, arbitratedInputs).zipped) {
        connectionsOutput(connection) = arbiterInput
      }
    }

    for(connection <- connections){
      val m = connectionsInput(connection)
      val s = connectionsOutput(connection)

      connection.connector(m, s)
    }

  }
}

class MultiInterconnectByTag(name : String = "multiinterconnect") extends MultiInterconnect with BusSlaveProvider {
  val component = Component.current

  val tags = new mutable.HashMap[MultiBusInterface, mutable.Set[String]]()

  override def addSlave(bus: MultiBusInterface, mapping: AddressMapping) : this.type = {
    for((sbus, s) <- slaves) {
      if(tags(sbus).intersect(tags(bus)).size > 0 && mapping != DefaultMapping && s.mapping != DefaultMapping) {
        val intersection = s.mapping.intersect(mapping)
        //assert(!s.mapping.hit(mapping.lowerBound), f"Memory map conflict ${s.mapping} vs ${mapping}")
        //assert(!s.mapping.hit(mapping.highestBound), f"Memory map conflict ${s.mapping} vs ${mapping}")
        intersection match {
          case NeverMapping => {}
          case _ => assert(intersection.maxSequentialSize == 0, f"Memory map conflict ${s.mapping} vs ${mapping}")
        }

      }
    }

    slaves(bus) = SlaveModel(mapping)
    this
  }

  def addTag(bus : MultiBusInterface, tag : String*) = {
    tags(bus) = tags.getOrElse(bus, mutable.Set()) ++ Set(tag:_*)
    this
  }

  def addMaster(bus : MultiBusInterface, tags : String*) : this.type = {
    addTag(bus, tags:_*)
    super.addMaster(bus)
    this
  }

  def addSlave(bus: MultiBusInterface, mapping: AddressMapping, tags : String*) : this.type = {
    addTag(bus, tags:_*)
    addSlave(bus, mapping = mapping)
    this
  }

  def find_masters(search_tags: Set[String]) = {
    masters.filter(m => tags(m).intersect(search_tags).nonEmpty)
  }

  def buildConnections(): Unit = {
    for(s <- slaves.keys) {
      find_masters(tags(s).toSet).foreach(m => {
        connect(m, s)
      })
    }
  }

  override def build() : Unit = {
    preBuildTasks.foreach(_())
    buildConnections()
    super.build()

    println(s"${name} System Bus Masters")
    for(m <- masters) {
      println(s"\t${m} ${tags(m)}")
      val connected_slaves = connections.filter(_.m == m).map(_.s)
      connected_slaves.foreach(s => {
        val mappingName = to_mask_mapping(m)(slaves(s).mapping) match {
          case MaskMapping(base, mask) => s"MaskMapping(0x${base.toString(16)}, 0x${mask.toString(16)})"
          case SizeMapping(base, size) => s"SizeMapping(0x${base.toString(16)}, 0x${size.toString(16)})"
          case _ => slaves(s).mapping.toString
        }
        println(s"\t\t%-64s ${mappingName} ${tags(s)}".format(s.toString))
      })
    }

  }

  override def add_slave_factory(name: String, mapping: SizeMapping, m2s_stage: Boolean, s2m_stage: Boolean, tags: String*): BusSlaveFactory = {
    val config = PipelinedMemoryBusConfig(log2Up(mapping.end), 32)

    val bus = if(component != Component.current) {
      val restore = Component.push(component)
      val bus = master(new PipelinedMemoryBus(config))
      restore.restore()
      bus
    } else {
      new PipelinedMemoryBus(config)
    }
    bus.setName(name)

    addSlave(new PipelinedMemoryBusMultiBus(bus), mapping = mapping, tags = tags:_*)
    new PipelinedMemoryBusSlaveFactory(bus.cmdS2mPipe().cmdS2mPipe().rspPipe())
  }
}