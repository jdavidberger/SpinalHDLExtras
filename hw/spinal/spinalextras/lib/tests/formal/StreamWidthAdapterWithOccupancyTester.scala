package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinalextras.lib.misc.StreamWidthAdapterWithOccupancy
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

import scala.language.postfixOps

class StreamWidthAdapterWithOccupancyTester extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 10

  formalTests().foreach(t => test(t._1) {
    t._2()
  })

  override def generateRtl() = {
    for (inW <- Seq(8, 16, 32, 64);
         outW <- Seq(8, 16, 32, 64);
         endianness <- Seq(LITTLE, BIG)) yield {
      (s"${suiteName}_${inW}_${outW}_${endianness.getClass.getSimpleName.replace("$", "")}", () => GeneralFormalDut(() => new StreamWidthAdapterWithOccupancy(Bits(inW bits), Bits(outW bits), endianness)))
    }
  }
}
