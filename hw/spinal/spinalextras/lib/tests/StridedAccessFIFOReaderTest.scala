package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.core._
import spinal.lib._
import spinal.lib.sim._
import spinalextras.lib.Config
import spinalextras.lib.bus.simple.SimpleMemoryProvider
import spinalextras.lib.memory.StridedAccessFIFOReader

import scala.collection.mutable

class StridedAccessFIFOReaderTest extends AnyFunSuite {
  def doTest() {
    Config.sim.doSim(
      new Component {
        val reader = StridedAccessFIFOReader(Bits(32 bits), 1024*9, 0x00000000L, 9, rsp_latency = 15)
        val io = new Bundle {
          val pop = master(cloneOf(reader.io.pop))
        }
        val memory = SimpleMemoryProvider(
          init = (0 until 9).flatMap(idx => Array.fill(2304 / 9)(idx)).map(BigInt(_))
        )
        memory.io.bus <> reader.io.bus
        val start = RegInit(True) clearWhen (io.pop.valid) setWhen (io.pop.lastFire)

        reader.io.pop <> io.pop
      }.setDefinitionName("StridedAccessFIFOReaderTest")
    ) { dut =>
      SimTimeout(5000 us)
      dut.clockDomain.forkStimulus(100 MHz)
      dut.io.pop.ready #= true
      dut.clockDomain.waitSampling(10)
      val q = new mutable.ArrayBuffer[Seq[BigInt]]()

      StreamMonitor(dut.io.pop, dut.clockDomain) {
        data => {
          q.append(data.fragment.map(_.toBigInt))
        }
      }

      // Test random stall
      while (!(dut.io.pop.valid.toBoolean && dut.io.pop.ready.toBoolean && dut.io.pop.last.toBoolean)) {
        dut.io.pop.ready #= simRandom.nextBoolean()
        dut.clockDomain.waitSampling(1)
      }

      // Test no stall
      dut.clockDomain.waitSampling(1)
      while (!(dut.io.pop.valid.toBoolean && dut.io.pop.ready.toBoolean && dut.io.pop.last.toBoolean)) {
        dut.io.pop.ready #= true
        dut.clockDomain.waitSampling(1)
      }

      // Test heavy stall
      dut.clockDomain.waitSampling(1)
      while (!(dut.io.pop.valid.toBoolean && dut.io.pop.ready.toBoolean && dut.io.pop.last.toBoolean)) {
        dut.io.pop.ready #= false
        dut.clockDomain.waitSampling(64)

        dut.io.pop.ready #= true
        dut.clockDomain.waitSampling(1)
      }

      assert(q.size == 256 * 3)
    }
  }
//
//  test("Basic") {
//    doTest();
//  }
}
