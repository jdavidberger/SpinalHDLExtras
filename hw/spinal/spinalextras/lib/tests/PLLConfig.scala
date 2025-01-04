package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.{DoubleToBuilder, HertzNumber, IntToBuilder}
import spinalextras.lib.blackbox.lattice.lifcl._
import spinalextras.lib.misc
import spinalextras.lib.misc.ClockSpecification

import scala.language.postfixOps

class PLLConfig extends AnyFunSuite {
  val cases = Seq((Seq(
    misc.ClockSpecification(24 MHz),
    misc.ClockSpecification(60.6 MHz),
    misc.ClockSpecification(101 MHz, tolerance = 0),
    misc.ClockSpecification(101 MHz, 90),
    misc.ClockSpecification(202 MHz, 25),
    misc.ClockSpecification(71.3 MHz, 135, .05)
  ),
    PLLClockConfig(HertzNumber(1212000000.0), 1, 50, 2048, -1, List(PLLOutputClockConfig(0, 19, 19, 0, true, false, HertzNumber(60600000), 0.0), PLLOutputClockConfig(0, 11, 11, 0, true, false, HertzNumber(101000000), 0.0), PLLOutputClockConfig(0, 14, 11, 0, true, false, HertzNumber(101000000), 90.0), PLLOutputClockConfig(0, 5, 5, 0, true, false, HertzNumber(202000000), 0.0), PLLOutputClockConfig(0, 22, 16, 0, true, false, HertzNumber(71294117.64705882352941176470588235), 127.0588235294118)), true, false)
  ),
    (Seq(
      misc.ClockSpecification(24 MHz),
      misc.ClockSpecification(60 MHz),
      misc.ClockSpecification(100 MHz, tolerance = 0),
      misc.ClockSpecification(100 MHz, 90),
      misc.ClockSpecification(200 MHz, 25)
    ),
      PLLClockConfig(HertzNumber(1200000000.0),1,50,0,-1,List(PLLOutputClockConfig(0,19,19,0,true,false,HertzNumber(60000000),0.0), PLLOutputClockConfig(0,11,11,0,true,false,HertzNumber(100000000),0.0), PLLOutputClockConfig(0,14,11,0,true,false,HertzNumber(100000000),90.0), PLLOutputClockConfig(0,5,5,0,true,false,HertzNumber(200000000),0.0)),true,false)
    ))

  test("KnownGoodConfigs") {
    for((input, output) <- cases) {
      val actual = PLLConfig.createClockConfig(input.head, input.drop(1): _*)
      print(actual)
      assert( output == actual)
    }

  }
}
