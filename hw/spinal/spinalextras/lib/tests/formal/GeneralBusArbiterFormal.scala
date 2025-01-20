package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal.{FormalDut, HasFormalAsserts, anyseq}
import spinal.lib._
import spinal.lib.com.spi.ddr.SpiXdrMasterCtrl.{XipBus, XipBusParameters}
import spinalextras.lib.bus.bus_traits.{InstructionCacheMemBusExtImpl, XipBusMemBusExtImpl}
import spinalextras.lib.bus.{GeneralBusArbiter, IMemoryBus}
import spinalextras.lib.testing.{FormalTestSuite, test_funcs}
import vexriscv.ip.{InstructionCacheConfig, InstructionCacheMemBus}

import scala.language.postfixOps


case class GeneralBusArbiterFormal[T <: Data with IMasterSlave](val memoryBusAccess: IMemoryBus[T], portCount : Int, pendingRspMax : Int = 1, rspRouteQueue : Boolean = false, transactionLock : Boolean = true) extends Component {
  val dut = FormalDut(new GeneralBusArbiter(memoryBusAccess, portCount, pendingRspMax, rspRouteQueue, transactionLock))
  assumeInitial(ClockDomain.current.isResetActive)

  dut.formalAssumeInputs()
  dut.formalAsserts()

  test_funcs.anyseq_inputs(dut.io)
}


class GeneralBusArbiterFormalTest extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 12

  formalTests().foreach(t => test(t._1) { t._2() })

  override def generateRtl() = Seq()

  override def generateRtlProve() = Seq()

  override def generateRtlBMC() = Seq((suiteName,
    //    () => GeneralBusArbiterFormal(new InstructionCacheMemBusExtImpl(InstructionCacheMemBus(
//      InstructionCacheConfig(
//        cacheSize = 2048, bytePerLine = 32, wayCount = 1, addressWidth = 24, cpuDataWidth = 24, memDataWidth = 32, catchIllegalAccess = true, catchAccessFault = true, asyncTagMemory = false
//      ))),
//      3)
    () => GeneralBusArbiterFormal(new XipBusMemBusExtImpl(XipBus(XipBusParameters(32, 5))), 2)
    )
  )
}