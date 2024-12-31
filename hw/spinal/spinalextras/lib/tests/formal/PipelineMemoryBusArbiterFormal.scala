package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal.{FormalDut, anyseq}
import spinal.lib._
import spinal.lib.bus.simple.{PipelinedMemoryBusArbiter, PipelinedMemoryBusConfig}
import spinalextras.lib.testing.{FormalTestSuite, test_funcs}

import scala.language.postfixOps

case class PipelineMemoryBusArbiterFormal(pipelinedMemoryBusConfig : PipelinedMemoryBusConfig, portCount : Int, pendingRspMax : Int, rspRouteQueue : Boolean, transactionLock : Boolean = true)
  extends Component {
  val dut = FormalDut(PipelinedMemoryBusArbiter(pipelinedMemoryBusConfig, portCount, pendingRspMax, rspRouteQueue, transactionLock))
  assumeInitial(ClockDomain.current.isResetActive)

  dut.io.inputs.foreach (bus => {
    anyseq(bus.cmd.payload)
    anyseq(bus.cmd.valid)
    bus.cmd.formalAssumesSlave()
  })

  val outputContract = test_funcs.assertPMBContract(dut.io.output, assume_slave = true)
  anyseq(dut.io.output.cmd.ready)
  anyseq(dut.io.output.rsp)

  test_funcs.assumePMBArbiter(dut)
}

class PipelineMemoryBusArbiterFormalTest extends AnyFunSuite with FormalTestSuite {
  formalTests().foreach(t => test(t._1) { t._2() })

  override def defaultDepth() = 10

  override def generateRtl() = Seq(
    ("basic", () => new PipelineMemoryBusArbiterFormal(PipelinedMemoryBusConfig(32,32), 5, 8, true, true)),
  )
}
