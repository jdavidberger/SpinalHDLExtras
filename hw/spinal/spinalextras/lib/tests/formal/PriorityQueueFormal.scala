package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal.{FormalDut, SpinalFormalConfig}
import spinal.lib._
import spinalextras.lib.formal.{ComponentWithFormalProperties, HasFormalProperties}
import spinalextras.lib.memory.PriorityQueueLocalMemory
import spinalextras.lib.testing.FormalTestSuite

import scala.collection.mutable
import scala.language.postfixOps


case class PriorityQueueFormal(pow_size : Int) extends Component {
  val dut = FormalDut(new PriorityQueueLocalMemory(UInt(8 bits), (1 << pow_size) - 1))
  assumeInitial(ClockDomain.current.isResetActive)

  dut.anyseq_inputs()

  dut.formalCovers()

  HasFormalProperties.printFormalAssertsReport()
}


class PriorityQueueFormalTest extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 20

  formalTests().foreach(t => test(t._1) { t._2() })

  override def CoverConfig(): SpinalFormalConfig = formalConfig.withCover(50)

  override def generateRtl() = Seq((suiteName,
    () => PriorityQueueFormal(3))
  )
}