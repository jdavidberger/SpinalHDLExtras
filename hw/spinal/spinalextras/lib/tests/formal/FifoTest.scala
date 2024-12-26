package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal.{FormalDut, anyseq}
import spinal.lib.StreamFifo
import spinalextras.lib.testing.{FormalTestSuite, test_funcs}
import spinalextras.lib.{Memories, MemoryRequirement}

import scala.language.postfixOps

class StreamFifoFormal[T <: Data](val dataType: HardType[T],
                            val depth: Int,
                            val withAsyncRead : Boolean = false,
                            val withBypass : Boolean = false,
                            val allowExtraMsb : Boolean = true,
                            val forFMax : Boolean = false,
                            val useVec : Boolean = false) extends Component {
  val dut = FormalDut(new StreamFifo(dataType, depth, withAsyncRead, withBypass, allowExtraMsb, forFMax, useVec))
  assumeInitial(ClockDomain.current.isResetActive)

  dut.io.pop.formalAssertsMaster()
  dut.io.push.formalAssumesSlave()

  //test_funcs.assertStreamContract(dut.io.pop)
  //test_funcs.assertStreamContract(dut.io.push)
  val fifoConstraints = test_funcs.formalFifoAsserts(dut)

  anyseq(dut.io.push.valid)
  anyseq(dut.io.push.payload)
  //anyseq(dut.io.flush)
  dut.io.flush := False
  anyseq(dut.io.pop.ready)
}


class StreamFifoFormalTest extends AnyFunSuite with FormalTestSuite {
/*
  if(withBypass) require(withAsyncRead)
  if(useVec) require (withAsyncRead)
 */
  val configs = {for(
    // Don't bother testing 0 and 1 here; they have no meaningful asserts
    depth <- Seq(2, 6, 8);
    withAsynRead <- Seq(true, false);
    withBypass <- Seq(true, false);
    allowExtraMsb <- Seq(true, false);
    forFMax <- Seq(true, false);
    useVec <- Seq(true, false)
  ) yield {
    if(!withAsynRead && (useVec || withBypass)) {
      null
    } else {
      (s"fifo_${depth}_withAsynRead${withAsynRead}_withBypass${withBypass}_allowExtraMsb${allowExtraMsb}_forFMax${forFMax}_useVec${useVec}", () => new StreamFifoFormal(UInt(2 bits), depth, withAsynRead, withBypass, allowExtraMsb, forFMax, useVec))
    }
  }}.filter(_ != null)

  override def defaultDepth() = 50

  formalTests().foreach(t => test(t._1) { t._2() })

  override def generateRtl(): Seq[(String, () => Component)] = configs
}