package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.core._
import spinal.lib.sim._
import spinal.lib._
import spinalextras.lib.Config
import spinalextras.lib.bus.simple.SimpleMemoryProvider
import spinalextras.lib.memory.StridedAccessFIFO

import scala.collection.mutable

class StridedAccessFIFOTest extends AnyFunSuite {
  def doTest() {
    Config.sim.doSim(
      new Component {
        val fifo = StridedAccessFIFO(Bits(32 bits), Bits(32 bits), 2304, 0x00000000L, 9, 32)
        val io = new Bundle {
          val push = slave(cloneOf(fifo.io.push))
          val pop = master(cloneOf(fifo.io.pop))
        }
        val memory = SimpleMemoryProvider()
        memory.io.bus <> fifo.io.bus
        fifo.io.pop <> io.pop
        fifo.io.push <> io.push
      }.setDefinitionName("StridedAccessFIFOReaderTest")
    ) { dut =>
      SimTimeout(5000 us)
      dut.clockDomain.forkStimulus(100 MHz)
      dut.io.pop.ready #= false
      dut.io.push.valid #= false
      dut.clockDomain.waitSampling(10)
      val q = new mutable.ArrayBuffer[Seq[BigInt]]()

      StreamMonitor(dut.io.pop, dut.clockDomain) {
        data => {
          q.append(data.map(_.toBigInt))
        }
      }

      dut.io.pop.ready #= true
      for (i <- 0 until 3) {
        val init_seq = (0 until 9).flatMap(idx => Array.fill(2304 / 9)(idx)).map(BigInt(_))
        init_seq.foreach(d => {
          dut.clockDomain.waitSamplingWhere(dut.io.push.ready.toBoolean)
          dut.io.push.payload.fragment #= d
          dut.io.push.payload.last #= false
          dut.io.push.valid #= true
          dut.clockDomain.waitSampling()
          dut.io.push.valid #= false
        })
      }

      while (q.size < 256 * 3) {
        dut.clockDomain.waitSampling(10)
      }
      dut.clockDomain.waitSampling(1000)

      println(q)
      assert(q.size == 256 * 3)
    }
  }
//
//  test("Basic") {
//    doTest()
//  }
}
