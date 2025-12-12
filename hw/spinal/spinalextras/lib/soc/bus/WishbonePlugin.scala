package spinalextras.lib.soc.bus

import spinal.core._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.regif.WishboneBusInterface
import spinal.lib.bus.wishbone.{AddressGranularity, Wishbone, WishboneConfig}
import spinal.lib.master
import spinalextras.lib.bus.bus.WishboneMultiBusInterface
import spinalextras.lib.soc.spinex.{Spinex, SpinexPlugin}

case class WishbonePlugin(mapping : SizeMapping,
                          name: String = "wb0",
                          tags : Seq[String] = Seq("dBus"),
                          wbConfig : WishboneConfig = WishboneConfig(32, 32, useERR = true, useRTY = false,
                            selWidth = 4, addressGranularity = AddressGranularity.BYTE).withBurstType) extends SpinexPlugin {
  lazy val bus = master(Wishbone(wbConfig))

  override def apply(soc: Spinex): Unit = {
    soc.io.valCallbackRec(bus, name)
    soc.add_slave(new WishboneMultiBusInterface(bus), name, mapping, direct = true, tags:_*)
  }
}