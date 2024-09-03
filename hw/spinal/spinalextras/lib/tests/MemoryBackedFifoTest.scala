package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.core.{BitVector, Bits, HardType, IntToBuilder}
import spinal.lib.sim._
import spinalextras.lib.Config
import spinalextras.lib.memory.MemoryBackedFifo

class MemoryBackedFifoTest extends AnyFunSuite {
  def doTest[T <: BitVector](dataType: HardType[T], depth: Int): Unit = {
    Config.sim.doSim(
      new MemoryBackedFifo(dataType, depth)
    ) { dut =>
      SimTimeout(5000 us)
      dut.io.push.valid #= false
      dut.io.pop.ready #= false
      dut.io.flush #= false
      dut.clockDomain.forkStimulus(100 MHz)
      dut.clockDomain.waitSampling(10)

      val sco = new ScoreboardInOrder[BigInt]()

      StreamMonitor(dut.io.pop, dut.clockDomain) {
        datum => {
          println(s"Got ${datum.toBigInt}")
          sco.pushDut(datum.toBigInt)
          dut.io.pop.ready #= false
        }
      }
      dut.io.pop.ready #= false

      for (i <- 0 until depth * 3) {
        dut.io.push.valid #= true
        val randValue = i //simRandom.nextLong().abs
        sco.pushRef(randValue)
        println(s"Pushing ${randValue} / ${i}")
        dut.io.push.payload #= randValue
        if (!dut.io.pop.ready.toBoolean) {
          dut.io.pop.ready.randomize()
        }
        assert(dut.io.push.ready.toBoolean)

        dut.clockDomain.waitSampling()
        while (!dut.io.push.ready.toBoolean) {
          if (!dut.io.pop.ready.toBoolean) {
            dut.io.pop.ready.randomize()
          }
          dut.clockDomain.waitSampling()
        }

      }
      dut.io.push.valid #= false

      while (dut.io.occupancy.toBigInt > 0) {
        println(s"Finish: ${dut.io.occupancy.toBigInt}")
        dut.clockDomain.waitSampling()
        //if(!dut.io.pop.ready.toBoolean)
        // dut.io.pop.ready.randomize()
        dut.io.pop.ready #= true
      }

      sco.checkEmptyness()
    }
  }

  test("basic") {
    doTest(Bits(95 bits), 1000)
    doTest(Bits(96 bits), 1000)
  }
}
