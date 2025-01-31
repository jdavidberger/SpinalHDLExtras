package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal.{FormalDut, HasFormalAsserts, anyseq, initstate}
import spinal.lib._
import spinal.lib.bus.misc.{AddressMapping, MaskMapping, SizeMapping}
import spinal.lib.bus.regif.WishboneBusInterface
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusConfig}
import spinal.lib.bus.wishbone.{AddressGranularity, Wishbone, WishboneConfig}
import spinal.lib.com.spi.ddr.SpiXdrMasterCtrl
import spinal.lib.com.spi.ddr.SpiXdrMasterCtrl.{XipBus, XipBusParameters}
import spinalextras.lib.bus.{MultiBusInterface, MultiInterconnectByTag, PipelinedMemoryBusMultiBus, WishboneExt}
import spinalextras.lib.bus.general.{GeneralBusArbiter, GeneralBusInterface, XipBusMemBusInterfaceExtImpl}
import spinalextras.lib.testing.{FormalTestSuite, test_funcs}
import vexriscv.ip.{InstructionCacheConfig, InstructionCacheMemBus}
import vexriscv.plugin.{DBusSimpleBus, IBusCachedPlugin, IBusSimpleBus}
import spinalextras.lib.bus.bus._

import scala.language.postfixOps


case class GeneralBusInterconnectFormal[T <: Data with IMasterSlave](val masterDefs : Seq[(() => MultiBusInterface, Seq[String])],
                                                                     val slaveDefs : Seq[(() => MultiBusInterface, AddressMapping, Seq[String])]) extends Component {
  val interconnect = new MultiInterconnectByTag()

  val masters = masterDefs.map(d => (d._1(), d._2))
  val slaves = slaveDefs.map(d => (d._1(), d._2, d._3))

  masters.foreach(d => interconnect.addMaster(d._1, d._2:_*))
  slaves.foreach(d => interconnect.addSlave(d._1, d._2, d._3:_*))
  interconnect.build()

  assumeInitial(ClockDomain.current.isResetActive)

  masters.foreach(d => test_funcs.anyseq_inputs(d._1.bus))
  masters.map(_._1.bus).map {
    {
      case b: DBusSimpleBus => {
        anyseq(b.cmd.valid)
        anyseq(b.cmd.payload)
      }
      case b: InstructionCacheMemBus => {
        anyseq(b.cmd.valid)
        anyseq(b.cmd.payload)
      }
    }
  }

  slaves.foreach(d => test_funcs.anyseq_inputs(d._1.bus))
  slaves.map(_._1.bus).map {
    {
      case b: XipBus => {
//        anyseq(b.cmd.valid)
//        anyseq(b.cmd.payload)
//        anyseq(b.rsp.ready)
        anyseq(b.cmd.ready)
        anyseq(b.rsp.valid)
        anyseq(b.rsp.payload)
      }
      case b: PipelinedMemoryBus => {
//        anyseq(b.cmd.valid)
//        anyseq(b.cmd.payload)
        anyseq(b.cmd.ready)
        anyseq(b.rsp.valid)
        anyseq(b.rsp.payload)
      }
      case b : Wishbone => {

      }
    }
  }

  val cmd_fires = masters.map(_._1.bus).map {
    {
      case b: DBusSimpleBus => b.cmd.fire
      case b: InstructionCacheMemBus => b.cmd.fire
    }
  }

  val slave_cmd_fires = slaves.map(_._1.bus).map {
    {
      case b: XipBus => b.cmd.fire&& b.cmd.address =/= 0
      case b: PipelinedMemoryBus => b.cmd.fire && b.cmd.address =/= 0
      case b : Wishbone => b.ACK && b.STB && b.wordAddress()(0)
    }
  }

  val rsp_fires = masters.map(_._1.bus).map {
    {
      case b: DBusSimpleBus => b.rsp.ready
      case b: InstructionCacheMemBus => b.rsp.fire
    }
  }

  val non_error_rsp_fires = Vec(masters.map(_._1.bus).map {
    {
      case b: DBusSimpleBus => b.rsp.ready && !b.rsp.error
      case b: InstructionCacheMemBus => b.rsp.fire && !b.rsp.error
    }
  })

  val new_cmds = CountOne(Vec(cmd_fires))
  val new_rsps = CountOne(Vec(rsp_fires))

  val non_error_rsp_fires_latch = Reg(non_error_rsp_fires.asBits) init(0)
  non_error_rsp_fires_latch := non_error_rsp_fires_latch | non_error_rsp_fires.asBits

  val slave_cmd_fires_latch = Reg(slave_cmd_fires.asBits) init(0)
  slave_cmd_fires_latch := slave_cmd_fires_latch | slave_cmd_fires.asBits

  //cover(new_cmds.orR === True)
  cover(non_error_rsp_fires_latch.andR && slave_cmd_fires_latch.andR)

  withAutoPull()
  HasFormalAsserts.formalAssertsChildren(this, assumesInputValid = true, useAssumes = false)
}


class GeneralBusInterconnectFormalTest extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 30

  formalTests().foreach(t => test(t._1) { t._2() })

  override def generateRtlBMC() = Seq()
  override def generateRtlProve() = Seq()

  val cacheConfig = InstructionCacheConfig(
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
  val wbConfig = WishboneConfig(32, 32, useERR = true, useRTY = false, selWidth = 4, addressGranularity = AddressGranularity.BYTE).withBurstType
  val i2cConfig = WishboneConfig(3, 8, addressGranularity = AddressGranularity.WORD)

  override def generateRtl() = Seq(
    ("Standard", () =>
      GeneralBusInterconnectFormal(
        Seq(
          (() => new InstructionCacheMemBus(cacheConfig).setName("iBus"), Seq("ibus")),
          (() => new DBusSimpleBus().setName("dBus"), Seq("dbus")),
        ),
        Seq(
          (() => new SpiXdrMasterCtrl.XipBus(XipBusParameters(32, 5)).setName("spiFlash"),
            SizeMapping(0x20000000L, 0x01000000),
            Seq("ibus", "dbus")
            ),
          (() => PipelinedMemoryBusMultiBus(new PipelinedMemoryBus(PipelinedMemoryBusConfig(32, 32)).setName("ram")),
            SizeMapping(0x40000000l, 65536),
            Seq("ibus", "dbus")
          ),
          (() => PipelinedMemoryBusMultiBus(new PipelinedMemoryBus(PipelinedMemoryBusConfig(32, 32)).setName("apb")),
            SizeMapping(0xe0000000L, 1 MiB),
            Seq("dbus")
          ),
          (() => new Wishbone(i2cConfig).setName("i2c"),
            SizeMapping(0xee000000L, 64 Bytes),
            Seq("dbus")
          ),
          (() => new Wishbone(wbConfig).setName("wb"),
            SizeMapping(0xb0000000L, 0x0f000000L),
            Seq("dbus")
          ),
        )
      )
    )
  )
}