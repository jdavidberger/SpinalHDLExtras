package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal.{FormalDut, anyseq}
import spinal.lib._
import spinal.lib.bus.simple.{PipelinedMemoryBusArbiter, PipelinedMemoryBusConfig}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut, test_funcs}

import scala.language.postfixOps

class PipelineMemoryBusArbiterFormalTest extends AnyFunSuite with FormalTestSuite {
  formalTests().foreach(t => test(t._1) { t._2() })

  override def defaultDepth() = 10

  override def generateRtl() = Seq(
    ("basic", () => GeneralFormalDut( () => PipelinedMemoryBusArbiter(PipelinedMemoryBusConfig(32,32), 3, 5, rspRouteQueue = true, transactionLock = true))),
    ("no_q", () => GeneralFormalDut( () => PipelinedMemoryBusArbiter(PipelinedMemoryBusConfig(32,32), 1, 5, rspRouteQueue = false, transactionLock = true))),
    ("basic_1", () => GeneralFormalDut( () => PipelinedMemoryBusArbiter(PipelinedMemoryBusConfig(32,32), 3, 1, rspRouteQueue = true, transactionLock = true))),
    ("no_q_1", () => GeneralFormalDut( () => PipelinedMemoryBusArbiter(PipelinedMemoryBusConfig(32,32), 1, 1, rspRouteQueue = false, transactionLock = true))),
  )
}
