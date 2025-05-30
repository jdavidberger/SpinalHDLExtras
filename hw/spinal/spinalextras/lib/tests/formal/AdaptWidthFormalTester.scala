package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.{BIG, Bits, IntToBuilder, LITTLE}
import spinalextras.lib.misc.{AdaptWidth, StreamWidthAdapterWithOccupancy}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

import scala.language.postfixOps

class AdaptWidthFormalTester extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 15

  formalTests().foreach(t => test(t._1) {
    t._2()
  })

  override def generateRtl() = {
    for (
      inW <- Seq(8, 12, 32, 48);
      outW <- Seq(8, 12, 32, 48);
      endianness <- Seq(LITTLE, BIG)
    ) yield {
      (s"${suiteName}_${inW}_${outW}_${endianness.getClass.getSimpleName.replace("$", "")}", () => GeneralFormalDut(() => new AdaptWidth(Bits(inW bits), Bits(outW bits), endianness)))
    }
  }
}


