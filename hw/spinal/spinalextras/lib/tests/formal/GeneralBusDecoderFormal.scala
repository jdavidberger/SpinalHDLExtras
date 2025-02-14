package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal.{FormalDut, SpinalFormalConfig, anyseq}
import spinal.lib._
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping, SizeMapping}
import spinal.lib.com.spi.ddr.SpiXdrMasterCtrl.{XipBus, XipBusParameters}
import spinalextras.lib.bus.general.{DSimpleBusInterfaceExtImpl, GeneralBusDecoder, GeneralBusInterface, InstructionCacheMemBusInterfaceExtImpl}
import spinalextras.lib.testing.{FormalTestSuite, test_funcs}
import vexriscv.ip.{InstructionCacheConfig, InstructionCacheMemBus}

import scala.Seq
import scala.language.postfixOps


case class GeneralBusDecoderFormal[T <: Data with IMasterSlave](val memoryBusAccess: GeneralBusInterface[T], mappings : Seq[AddressMapping], pendingMax : Int = 3) extends Component {
  val dut = FormalDut(new GeneralBusDecoder(memoryBusAccess, mappings, pendingMax))
  assumeInitial(ClockDomain.current.isResetActive)

  cover(dut.allOutputsSawResponse() && dut.noOutstanding())
  test_funcs.anyseq_inputs(dut.io)
}


class GeneralBusDecoderFormalTest extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 15

  formalTests().foreach(t => test(t._1) { t._2() })

  lazy val instructionCacheConfigs = Seq(
    "BasicICache" -> InstructionCacheConfig(
      cacheSize = 2048, bytePerLine = 16, wayCount = 1, addressWidth = 24, cpuDataWidth = 24, memDataWidth = 32, catchIllegalAccess = true, catchAccessFault = true, asyncTagMemory = false
    ),
  )

  override def CoverConfig(): SpinalFormalConfig = formalConfig.withCover(100)

  def promotBusAccess[T <: Data with IMasterSlave](busAccess : (String, GeneralBusInterface[T])) =
    busAccess._1 -> ((mapping : Seq[AddressMapping]) => GeneralBusDecoderFormal(busAccess._2, mapping))

  lazy val generalBusDecoderFormal : Seq[(String, Seq[AddressMapping] => (Component))] =
    instructionCacheConfigs.map(x => x._1 -> InstructionCacheMemBusInterfaceExtImpl(x._2)).map(promotBusAccess)  ++
      Seq("BasicDBus" -> new DSimpleBusInterfaceExtImpl()).map(promotBusAccess)

  lazy val slaveMappings = Seq(
    "Normal" -> Seq(SizeMapping(0, 1000), SizeMapping(3000, 1000)),
    "WithDefault" -> Seq(SizeMapping(900, 1000), DefaultMapping),
    "Single" -> Seq(SizeMapping(900, 1000)),
    "Default" -> Seq(DefaultMapping)
  )

  override def generateRtl() =
    for((cfgName, f) <- generalBusDecoderFormal; (name, mapping) <- slaveMappings) yield {
      (suiteName + cfgName + name, () => f(mapping))
    }
}