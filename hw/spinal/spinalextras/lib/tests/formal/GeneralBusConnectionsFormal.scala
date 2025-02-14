package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal.{FormalDut, anyseq, initstate}
import spinal.lib._
import spinal.lib.bus.misc.{AddressMapping, MaskMapping, SizeMapping}
import spinal.lib.bus.regif.WishboneBusInterface
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusConfig}
import spinal.lib.bus.wishbone.{AddressGranularity, Wishbone, WishboneConfig}
import spinal.lib.com.spi.ddr.SpiXdrMasterCtrl
import spinal.lib.com.spi.ddr.SpiXdrMasterCtrl.{XipBus, XipBusParameters}
import spinal.lib.formal.{ComponentWithFormalAsserts, HasFormalAsserts}
import spinalextras.lib.Config
import spinalextras.lib.bus.{MultiBusInterface, MultiInterconnectByTag, MultiInterconnectConnectFactory, PipelinedMemoryBusMultiBus, WishboneExt}
import spinalextras.lib.bus.general.{GeneralBusArbiter, GeneralBusInterface, XipBusMemBusInterfaceExtImpl}
import spinalextras.lib.testing.{FormalTestSuite, test_funcs}
import vexriscv.ip.{InstructionCacheConfig, InstructionCacheMemBus}
import vexriscv.plugin.{DBusSimpleBus, IBusCachedPlugin, IBusSimpleBus}
import spinalextras.lib.bus.bus._

import spinalextras.lib.bus.bus.XipBusFormal

import scala.language.postfixOps

case class GeneralBusConnectionComponent(val m : () => MultiBusInterface, val s : () => MultiBusInterface) extends ComponentWithFormalAsserts {
  val mBus = m()
  val sBus = s()
  println(s"Created ${mBus} and ${sBus}")
  val io = new Bundle {
    val m = slave(mBus.bus)
    val s = master(sBus.bus)
  }

  withAutoPull()
  MultiInterconnectConnectFactory(mBus, sBus)
}

case class GeneralBusConnectionFormal(val m : () => MultiBusInterface, val s : () => MultiBusInterface) extends Component {
  val dut = FormalDut(GeneralBusConnectionComponent(m, s))

  assumeInitial(ClockDomain.current.isResetActive)

  dut.anyseq_inputs()
  HasFormalAsserts.printFormalAssertsReport()
}


class GeneralBusConnectionFormalTest extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 12

  formalTests().foreach(t => test(t._1) { t._2() })

  override def generateRtlCover() = Seq()

  lazy val cacheConfig = InstructionCacheConfig(
    cacheSize = 2048,
    bytePerLine = 32,
    wayCount = 1,
    addressWidth = 32,
    cpuDataWidth = 32,
    memDataWidth = 32,
    catchIllegalAccess = true,
    catchAccessFault = true,
    asyncTagMemory = false,
    twoCycleRam = false,
    twoCycleCache = true
  )
  lazy val wbConfig = WishboneConfig(32, 32, useERR = true, useRTY = false, selWidth = 4, addressGranularity = AddressGranularity.BYTE).withBurstType
  lazy val i2cConfig = WishboneConfig(3, 8, addressGranularity = AddressGranularity.WORD)

  lazy val busConfigurations : Seq[(String, () => MultiBusInterface)] = Seq(
    ("IBus", () => new InstructionCacheMemBus(cacheConfig)),
    ("DBus", () => new DBusSimpleBus()),
    ("XipBus", () => new XipBusFormal(XipBusParameters(32, 5))),
    ("PMB", () => PipelinedMemoryBusMultiBus(new PipelinedMemoryBus(PipelinedMemoryBusConfig(32, 32)))),
    ("WB_byte", () => new Wishbone(i2cConfig)),
    ("WB_word", () => new Wishbone(wbConfig))
  )

  def checkValid(dutCreate: () => Component): Boolean = {
    try {
      Config.spinal.generateVerilog(dutCreate())
      true
    } catch {
      case _ : MatchError => {
        false
      }
    }
  }
  override def generateRtl() = (for(m <- busConfigurations; s <- busConfigurations) yield {
    (s"GeneralBusConnection_${m._1}_${s._1}", () => GeneralBusConnectionFormal(m._2, s._2))
  }).filter(x => checkValid(x._2))
}