package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.core._
import spinal.core.formal.{FormalDut, anyseq}
import spinal.lib.{Counter, CounterFreeRun, StreamFifo}
import spinal.lib.sim._
import spinalextras.lib.Config
import spinalextras.lib.memory._
import spinalextras.lib.testing.test_funcs

import scala.language.postfixOps

class MemoryBackedFIFOsTest extends AnyFunSuite {
  def doTBTest[T <: BitVector](dataType: HardType[T], depths: Seq[BigInt]): Unit = {
    Config.sim.doSim(
      new Component {
        val io = new Bundle {
          val valid = out(Bool())
        }
        val pools = new MemoryPoolFIFOs(dataType, depths)
        io.valid := Vec(pools.io.fifos.map(f => {
          val fifoTB = FifoTestBench(dataType)
          fifoTB.io.push <> f.push
          fifoTB.io.pop <> f.pop
          f.flush := False
          fifoTB.io.valid
        })).asBits.andR
      }.setDefinitionName("MemoryBackedFIFOsTestTB")
    ) { dut =>
      SimTimeout(5000 us)
      dut.clockDomain.forkStimulus(100 MHz)
      dut.clockDomain.waitSampling(100000)
    }
  }

  def doTest[T <: BitVector](dataType: HardType[T], depths: Seq[BigInt], disableReady : Boolean = true): Unit = {
    Config.sim.doSim(
      new MemoryPoolFIFOs(dataType, depths)
    ) { dut =>
      SimTimeout(5000 us)
      var allDone = false
      val scos = dut.io.fifos.map(
        f => {
          val sco = new ScoreboardInOrder[BigInt]()
          f.push.valid #= false
          f.pop.ready #= false
          f.flush #= false

          StreamMonitor(f.pop, dut.clockDomain) {
            datum => {
              println(s"Got ${f} ${datum.toBigInt}")
              sco.pushDut(datum.toBigInt)
              if(disableReady && !allDone)
                f.pop.ready.randomize()
            }
          }
          f.pop.ready #= false
          sco
        }
      )
      dut.clockDomain.forkStimulus(100 MHz)
      dut.clockDomain.waitSampling(10)

      val forks = depths.indices.map(i => {
        fork {
          val depth = depths(i)
          val fifo = dut.io.fifos(i)
          val sco = scos(i)

          for (j <- 0 until (depth * 3).toInt) {
            fifo.push.valid #= true
            val randValue = j //simRandom.nextLong().abs
            sco.pushRef(randValue)
            println(s"Pushing ${randValue} / ${j} for ${i}")
            fifo.push.payload #= randValue
            if (!fifo.pop.ready.toBoolean) {
              fifo.pop.ready.randomize()
            }
            //assert(fifo.push.ready.toBoolean)

            dut.clockDomain.waitSampling()
            while (!fifo.push.ready.toBoolean) {
              if (!fifo.pop.ready.toBoolean) {
                fifo.pop.ready.randomize()
              }
              dut.clockDomain.waitSampling()
            }

          }
          fifo.push.valid #= false

          while (fifo.occupancy.toBigInt > 0) {
            println(s"Finish: ${fifo.occupancy.toBigInt}")
            dut.clockDomain.waitSampling()
            //if(!dut.io.pop.ready.toBoolean)
            // dut.io.pop.ready.randomize()
            fifo.pop.ready #= true
          }

        }
      })

      forks.foreach(_.join())
      allDone = true
      dut.clockDomain.waitSampling()
      dut.io.fifos.foreach(f => f.pop.ready #= true)
      dut.clockDomain.waitSampling(100)

      scos.foreach(_.checkEmptyness())
    }
  }
  test("single") {
    doTest(Bits(32 bits), Seq(100))
    doTest(Bits(32 bits), Seq(100), false)
  }

  test("64_bit") {
    doTest(Bits(64 bits), Seq(10, 5))
    doTest(Bits(64 bits), Seq(10, 5), false)
  }

  test("basic") {
    doTBTest(Bits(32 bits), Seq(1000, 900, 1100))

    doTest(Bits(32 bits), Seq(1000, 900, 1100))
    doTest(Bits(32 bits), Seq(1000, 900, 1100), false)
  }
}


