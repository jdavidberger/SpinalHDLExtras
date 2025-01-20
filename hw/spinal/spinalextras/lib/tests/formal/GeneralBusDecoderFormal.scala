package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal.{FormalDut, HasFormalAsserts, anyseq}
import spinal.lib._
import spinal.lib.bus.misc.{AddressMapping, SizeMapping}
import spinal.lib.com.spi.ddr.SpiXdrMasterCtrl.{XipBus, XipBusParameters}
import spinalextras.lib.bus.bus_traits.{InstructionCacheMemBusExtImpl, XipBusMemBusExtImpl}
import spinalextras.lib.bus.{GeneralBusDecoder, IMemoryBus}
import spinalextras.lib.testing.{FormalTestSuite, test_funcs}
import vexriscv.ip.{InstructionCacheConfig, InstructionCacheMemBus}

import scala.language.postfixOps


case class GeneralBusDecoderFormal[T <: Data with IMasterSlave](val memoryBusAccess: IMemoryBus[T], mappings : Seq[AddressMapping], pendingMax : Int = 3) extends Component {
  val dut = FormalDut(new GeneralBusDecoder(memoryBusAccess, mappings, pendingMax))
  assumeInitial(ClockDomain.current.isResetActive)

  dut.formalAssumeInputs()
  dut.formalAsserts()

  test_funcs.anyseq_inputs(dut.io)
}


class GeneralBusDecoderFormalTest extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 20

  formalTests().foreach(t => test(t._1) { t._2() })

  override def generateRtl() = Seq()

  override def generateRtlProve() = Seq()

  override def generateRtlBMC() = Seq((suiteName,
        () => GeneralBusDecoderFormal(new InstructionCacheMemBusExtImpl(InstructionCacheMemBus(
          InstructionCacheConfig(
            cacheSize = 2048, bytePerLine = 16, wayCount = 1, addressWidth = 24, cpuDataWidth = 24, memDataWidth = 32, catchIllegalAccess = true, catchAccessFault = true, asyncTagMemory = false
          ))),
          Seq(SizeMapping(0, 1000), SizeMapping(3000, 1000)))
  )
  )
}