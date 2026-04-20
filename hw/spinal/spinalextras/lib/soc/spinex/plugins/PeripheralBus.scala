package spinalextras.lib.soc.spinex.plugins

import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.databind.deser.std.StdDeserializer
import com.fasterxml.jackson.databind.node.ObjectNode
import com.fasterxml.jackson.databind.{DeserializationContext, JsonNode, ObjectMapper}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusConfig}
import spinal.lib.bus.wishbone.{AddressGranularity, Wishbone, WishboneConfig}
import spinal.lib.master
import spinalextras.lib.bus.PipelinedMemoryBusMultiBus
import spinalextras.lib.bus.bus.WishboneMultiBusInterface
import spinalextras.lib.soc.spinex.{Spinex, SpinexRegisterFilePlugin}

import scala.language.postfixOps
import scala.reflect.runtime.universe.typeOf

sealed trait BusType;
object BusType {
  object AXI extends BusType;
  object Wishbone extends BusType;
  object PipelinedMemoryBus extends BusType;

  def patchEnumFields(objectMapper : ObjectMapper, schemaNode : JsonNode ) : JsonNode = {
    val ref_dt = schemaNode.at("/definitions/BusType")
    ref_dt match {
      case node: ObjectNode =>
        val enumNames = typeOf[BusType].typeSymbol.asClass.knownDirectSubclasses.toList.map(_.name.toString)
        node.put("type", "string")
        val enumArray = objectMapper.createArrayNode()
        enumNames.foreach(enumArray.add)
        node.remove("properties")
        node.set("enum", enumArray)
        ()
      case _ =>
    };
    schemaNode
  }

  def withName(s : String): BusType = {
    s.toLowerCase match {
      case "axi" => AXI
      case "wishbone" => Wishbone
      case "pipelinedmemorybus" => PipelinedMemoryBus
      case _ => {
        throw new RuntimeException(s"Could not match value ${s.toLowerCase}")
      }
    }
  }
}

case class BusTypeDeserializer() extends StdDeserializer[BusType](classOf[BusType]) {
  override def deserialize(p: JsonParser, ctxt: DeserializationContext) = {
    BusType.withName(p.getText)
  }
}


case class PeripheralBus(name : String, mapping : SizeMapping,
                         busType: BusType = BusType.PipelinedMemoryBus,
                         tags : Seq[String] = Seq("dBus"),
                         direct : Boolean = false) extends
  SpinexRegisterFilePlugin(name, mapping) {

  override def compatible: Seq[String] = super.compatible ++ Seq("spinex,peripheral",
    f"spinex,peripheral-${busType.toString.split('$').apply(1).toLowerCase}")

  lazy val bus = busType match {
    case BusType.Wishbone =>
      new WishboneMultiBusInterface(
        master(new Wishbone(WishboneConfig(32, 32, addressGranularity = AddressGranularity.BYTE)))
      )
    case BusType.PipelinedMemoryBus =>
      new PipelinedMemoryBusMultiBus(
        master(new PipelinedMemoryBus(PipelinedMemoryBusConfig(32, 32)))
      )
  }

  override def apply(som: Spinex): Unit = {
    som.io.valCallbackRec(bus, f"${name}_bus")
    som.add_slave(bus, name, mapping, direct = direct, tags:_*)
  }
}