package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.lib.bus.amba4.axi.Axi4SharedOnChipRam
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

class Axi4SharedOnChipRamFormalTester extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 10

  formalTests().foreach(t => test(t._1) {
    t._2()
  })

  override def generateRtl() = Seq()

  override def generateRtlBMC() = {
    Seq(
      (s"Basic", () =>
        GeneralFormalDut(() => new Axi4SharedOnChipRam(64, 8, 1)))
    )
  }
}
