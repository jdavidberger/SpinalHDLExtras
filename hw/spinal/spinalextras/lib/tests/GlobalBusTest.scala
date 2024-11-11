package spinalextras.lib.tests

import spinal.core._
import spinal.lib.Counter
import spinal.lib.bus.misc.{DefaultMapping, SizeMapping}
import spinal.lib.bus.regif.AccessType.RW
import spinal.lib.bus.simple._
import spinal.lib.bus.wishbone.{Wishbone, WishboneConfig}
import spinalextras.lib.Config
import spinalextras.lib.bus.{GlobalBusFactory, WishboneGlobalBus}
import spinalextras.lib.misc._

import scala.language.postfixOps

object WishboneGlobalBus extends GlobalBusFactory[WishboneGlobalBus] {
  override def create_global_bus(): WishboneGlobalBus = new WishboneGlobalBus(WishboneConfig(32, 32))
}

case class BusConsumer() extends Component {
  val io = new Bundle {
    val dataOut = out Bool()
  }
  val busIf = WishboneGlobalBus().add_bus_interface("consumer", SizeMapping(0, 0xFFFFFFFFL))
  val reg = busIf.newReg("consumer").field(UInt(32 bits), RW)

  io.dataOut := reg.xorR
}

case class BusProducer() extends Component {
  val bus = WishboneGlobalBus().add_master("producer")
  val counter = Counter(0xFFFFFFFFL) init(0)

  bus.CYC := True
  bus.ADR := 0x0
  bus.DAT_MOSI := counter.value.asBits
  bus.STB := True
  bus.WE := True

  when(bus.ACK) {
    counter.increment()
  }
}

object GlobalBusTest extends App {
  Config.spinal.generateVerilog(
    AutoInterconnect(
      "GlobalBusTest",
      () => Iterator(
        BusConsumer(),
        BusProducer()
      )
    )
  )
}