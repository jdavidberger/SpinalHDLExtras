package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal.{FormalDut, HasFormalAsserts, anyseq}
import spinal.lib._
import spinal.lib.bus.regif.WishboneBusInterface
import spinal.lib.bus.wishbone.WishboneConfig
import spinal.lib.com.spi.ddr.SpiXdrMasterCtrl.{XipBus, XipBusParameters}
import spinalextras.lib.bus.general.{GeneralBusArbiter, GeneralBusInterface, XipBusMemBusInterfaceExtImpl}
import spinalextras.lib.testing.{FormalTestSuite, test_funcs}
import vexriscv.ip.{InstructionCacheConfig, InstructionCacheMemBus}

import scala.language.postfixOps


case class GeneralBusArbiterFormal[T <: Data with IMasterSlave](val memoryBusAccess: GeneralBusInterface[T], portCount : Int, pendingRspMax : Int = 1, transactionLock : Boolean = true) extends Component {
  val dut = FormalDut(new GeneralBusArbiter(memoryBusAccess, portCount, pendingRspMax, transactionLock))
  assumeInitial(ClockDomain.current.isResetActive)

  cover(memoryBusAccess.rsp_required_count(dut.io.output) === 1 && memoryBusAccess.cmd(dut.io.output).fire)
  test_funcs.anyseq_inputs(dut.io)
}


class GeneralBusArbiterFormalTest extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 12

  formalTests().foreach(t => test(t._1) { t._2() })

  override def generateRtlCover() = Seq()

  override def generateRtl() = Seq(
    ("GeneralArbiter_XIP_1", () => GeneralBusArbiterFormal(new XipBusMemBusInterfaceExtImpl((XipBusParameters(32, 5))), 1)),
    ("GeneralArbiter_XIP_5", () => GeneralBusArbiterFormal(new XipBusMemBusInterfaceExtImpl((XipBusParameters(32, 5))), 5)),
  )
}