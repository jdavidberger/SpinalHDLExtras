package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.core._
import spinal.lib.bus.simple.PipelinedMemoryBusConfig
import spinal.lib.bus.wishbone.{AddressGranularity, WishboneConfig}
import spinal.lib.sim.{FlowMonitor, ScoreboardInOrder}
import spinal.lib.wishbone.sim.{WishboneSequencer, WishboneStatus, WishboneTransaction}
import spinalextras.lib.Config
import spinalextras.lib.bus.PipelinedMemoryBusToWishbone

import scala.collection.mutable
import scala.util.Random

class PipelinedMemoryBusToWishboneTest extends AnyFunSuite {
  def runTest(config: WishboneConfig): Unit = {
    Config.sim.withWave
      .doSim(
        new PipelinedMemoryBusToWishbone(config, PipelinedMemoryBusConfig(32, 32)) {
          val sysclk = Reg(UInt(32 bits)) init (0)
          sysclk := sysclk + 1
          SimPublic(sysclk)
        }.setDefinitionName("ToF_PipelinedMemoryBusToWishbone")
      ) { dut =>
        dut.io.wb.DAT_MISO #= 0xcafecafeL
        dut.io.wb.ACK #= false
        dut.io.pmb.cmd.valid #= false

        dut.clockDomain.forkStimulus(100 MHz)
        SimTimeout(1 ms)
        dut.clockDomain.waitSampling(1)

        val pmb_resp = ScoreboardInOrder[BigInt]()
        val sco = ScoreboardInOrder[WishboneTransaction]()
        val seq = WishboneSequencer {
          WishboneTransaction(BigInt(Random.nextInt(200) * 4), BigInt(Random.nextInt(200)))
        }
        var running = true

        val wb_thread = fork {
          var q = new mutable.Queue[(BigInt, Boolean)]()

          while (running) {
            dut.io.wb.ACK #= false

            if (q.nonEmpty) {
              dut.io.wb.DAT_MISO #= 0xcafecafeL
              if (dut.io.wb.ACK.randomize()) {
                val (addr, was_write) = q.dequeue()
                if (!was_write) {
                  val response = BigInt(Random.nextInt(1000))
                  println(s"Pushing ${addr} ${response} ${dut.sysclk.toBigInt}")
                  dut.io.wb.DAT_MISO #= response
                  pmb_resp.pushRef(response)
                } else {
                  dut.io.wb.DAT_MISO #= 0xbeefbeefL
                }
              }
            }

            var stalled = false
            if (dut.io.wb.STALL != null) {
              stalled = dut.io.wb.STALL.toBoolean
            }

            if (!stalled && dut.io.wb.CYC.toBoolean && dut.io.wb.STB.toBoolean) {
              println(s"Need response for a: ${dut.io.wb.ADR.toBigInt} ${dut.io.wb.WE.toBoolean} ${dut.sysclk.toBigInt} ${simTime()}")
              q.enqueue((dut.io.wb.ADR.toBigInt, dut.io.wb.WE.toBoolean))
            }

            dut.clockDomain.waitSampling()
          }
        }

        if (dut.io.wb.STALL != null) {
          dut.io.wb.STALL #= true
          dut.clockDomain.onSamplings({
            dut.io.wb.STALL.randomize()
          })
        }

        FlowMonitor(dut.io.pmb.rsp, dut.clockDomain) { rsp =>
          println(s"Got d: ${rsp.data.toBigInt} ${dut.sysclk.toBigInt} ${simTime()}")
          assert(rsp.data.toBigInt != BigInt(0xcafecafeL))
          pmb_resp.pushDut(rsp.data.toBigInt)
        }

        fork {
          val busStatus = WishboneStatus(dut.io.wb)
          while (true) {
            dut.clockDomain.waitSamplingWhere(busStatus.isTransfer)
            val tran = WishboneTransaction.sampleAsSlave(dut.io.wb)
            println(s"Popping ${tran}")
            sco.pushDut(tran)
          }
        }


        def fixAddress(address: BigInt): BigInt = {
          if (config.wordAddressInc() > 1) {
            address & ~(config.wordAddressInc() - 1)
          } else {
            address
          }
        }

        val word_shift = log2Up(config.wordAddressInc())
        for (repeat <- 0 until 100) {
          seq.generateTransactions()
          var tran = seq.nextTransaction.map(x => x.copy(address = fixAddress(x.address))).head
          val we = Random.nextBoolean()
          if (!we) {
            tran = tran.copy(data = BigInt(0xdeadbe00L))
          }
          println(s"Pushing ${tran} ${tran.address << word_shift} ${we} ${dut.sysclk.toBigInt}")
          sco.pushRef(tran)
          dut.io.pmb.cmd.address #= tran.address >> word_shift
          dut.io.pmb.cmd.write #= we
          dut.io.pmb.cmd.valid #= true
          dut.io.pmb.cmd.data #= tran.data
          dut.io.pmb.cmd.mask #= (1 << dut.io.pmb.cmd.mask.getBitsWidth) - 1
          dut.clockDomain.waitSamplingWhere(dut.io.pmb.cmd.ready.toBoolean)
          dut.io.pmb.cmd.valid #= false
          dut.clockDomain.waitSampling()
        }
        running = false
        wb_thread.join()

      }
  }

  test("PipelinedMemoryBusToWishbone_word_std") {
    runTest(WishboneConfig(32, 32, addressGranularity = AddressGranularity.WORD))
  }
  test("PipelinedMemoryBusToWishbone_word") {
    runTest(WishboneConfig(32, 32, addressGranularity = AddressGranularity.WORD).pipelined)
  }

  test("PipelinedMemoryBusToWishbone_std") {
    runTest(WishboneConfig(32, 32, addressGranularity = AddressGranularity.BYTE))
  }
  test("PipelinedMemoryBusToWishbone") {
    runTest(WishboneConfig(32, 32, addressGranularity = AddressGranularity.BYTE).pipelined)
  }
}
