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
    ("basic", () => GeneralFormalDut( () => PipelinedMemoryBusArbiter(PipelinedMemoryBusConfig(32,32), 5, 8, true, true))),
  )
}
