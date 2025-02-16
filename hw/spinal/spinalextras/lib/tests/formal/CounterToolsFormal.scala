package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal.{FormalDut, anyseq}
import spinal.lib._
import spinal.lib.bus.regif.WishboneBusInterface
import spinal.lib.bus.wishbone.WishboneConfig
import spinal.lib.com.spi.ddr.SpiXdrMasterCtrl.{XipBus, XipBusParameters}
import spinal.lib.formal.{ComponentWithFormalAsserts, HasFormalAsserts}
import spinalextras.lib.bus.general.{GeneralBusArbiter, GeneralBusInterface, XipBusMemBusInterfaceExtImpl}
import spinalextras.lib.misc.CounterUpDownUneven
import spinalextras.lib.testing.{FormalTestSuite, test_funcs}
import vexriscv.ip.{InstructionCacheConfig, InstructionCacheMemBus}

import scala.language.postfixOps

case class CounterUpDownUnevenComponent(val range : Int, val incBy : Int = 1, val decBy : Int = 1) extends ComponentWithFormalAsserts {
  val counter = new CounterUpDownUneven(range, incBy, decBy)
  val io = new Bundle {
    val inc, dec, clear = in Bool()
    val count = out (cloneOf(counter.value))
  }
  when(io.inc){ counter.increment() }
  when(io.dec){ counter.decrement() }
  when(io.clear) { counter.clear() }
  io.count := counter.value

  override lazy val formalValidInputs = counter.formalValidInputs
}

case class CounterToolsFormal(val range : Int, val incBy : Int = 1, val decBy : Int = 1) extends Component {
  val dut = FormalDut(new CounterUpDownUnevenComponent(range, incBy, decBy))
  assumeInitial(ClockDomain.current.isResetActive)

  dut.anyseq_inputs()
  HasFormalAsserts.printFormalAssertsReport()
}


class CounterToolsFormalTest extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 10000

  formalTests().foreach(t => test(t._1) { t._2() })

  override def generateRtlCover() = Seq()

  override def generateRtl() = Seq(
    ("CounterToolsFormal_32_8_16", () => CounterToolsFormal(32, 8, 16))
  )
}