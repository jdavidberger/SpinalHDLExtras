package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.{ClockDomainConfig, HIGH, IntToBuilder, SYNC, UInt}
import spinalextras.lib.testing.FormalTestSuite
import spinalextras.lib.tests.MemoryPoolFIFOsFormal

import scala.language.postfixOps

class MemoryBackedFIFOsTestFormal extends AnyFunSuite with FormalTestSuite {
  formalTests().foreach(t => test(t._1) { t._2() })

  override def defaultDepth() = 20

  override def generateRtl() = Seq((suiteName, () => new MemoryPoolFIFOsFormal(UInt(8 bits), Seq(10, 50))))
  override def generateRtlProve() = Seq((suiteName, () => new MemoryPoolFIFOsFormal(UInt(8 bits), Seq(10, 50), checkResponses = false)))
}