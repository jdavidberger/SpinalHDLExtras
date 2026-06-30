package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.lib.com.spi.ddr.SpiXdrMasterCtrl.XipBusParameters
import spinalextras.lib.bus.general.{GeneralBusArbiter, XipBusMemBusInterfaceExtImpl}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

import scala.language.postfixOps

class GeneralBusArbiterFormalTest extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 8

  formalTests().foreach(t => test(t._1) { t._2() })

  override def generateRtlCover() = Seq()

  override def generateRtl() = Seq(
    ("GeneralArbiter_XIP_1", () => GeneralFormalDut(
      () => new GeneralBusArbiter(
        new XipBusMemBusInterfaceExtImpl((XipBusParameters(32, 5))), 1
      )
    )),
    ("GeneralArbiter_XIP_5", () => GeneralFormalDut(
      () => new GeneralBusArbiter(new XipBusMemBusInterfaceExtImpl((XipBusParameters(32, 5))), 5))
    ),
  )
}