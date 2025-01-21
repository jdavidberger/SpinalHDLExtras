package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.core._
import spinal.core.formal.{FormalDut, anyseq}
import spinal.lib.bus.simple.PipelinedMemoryBusConfig
import spinal.lib.bus.wishbone.{AddressGranularity, WishboneConfig}
import spinal.lib.sim.ScoreboardInOrder
import spinal.lib.wishbone.sim._
import spinalextras.lib.Config
import spinalextras.lib.bus.WishboneToPipelinedMemoryBus

import scala.collection.mutable
import scala.util.Random



class WishboneToPipelinedMemoryBusTest extends AnyFunSuite {
  def runTest(config: WishboneConfig, rspQueue: Int): Unit = {
    Config.sim.withWave
      .doSim(
        new WishboneToPipelinedMemoryBus(PipelinedMemoryBusConfig(32, 32), config, rspQueue = rspQueue) {
          val sysclk = Reg(UInt(32 bits)) init (0)
          sysclk := sysclk + 1
          SimPublic(sysclk)
        }.setDefinitionName("WishboneToPipelinedMemoryBus")
      ) { dut =>
        dut.io.pmb.cmd.ready #= false
        dut.io.pmb.rsp.valid #= false
        dut.io.pmb.rsp.data #= 0xcafecafeL
        dut.io.wb.CYC #= false

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
          var q = new mutable.Queue[BigInt]()

          while (running) {
            dut.io.pmb.rsp.valid #= false

            if (q.nonEmpty) {
              dut.io.pmb.rsp.data #= 0xcafecafeL
              println(s"Q has ${q.size} entries")
              if (dut.io.pmb.rsp.valid.randomize()) {
                val addr = q.dequeue()
                val response = BigInt(Random.nextInt(1000))
                println(s"Pushing ${addr} ${response} ${dut.sysclk.toBigInt}")
                dut.io.pmb.rsp.data #= response
                pmb_resp.pushRef(response)
              }
            }

            var stalled = false
            if (dut.io.wb.STALL != null) {
              stalled = dut.io.wb.STALL.toBoolean
            }

            if (dut.io.pmb.cmd.valid.toBoolean && dut.io.pmb.cmd.ready.toBoolean) {
              println(s"Need response for address ${dut.io.pmb.cmd.address.toBigInt} write: ${dut.io.pmb.cmd.write.toBoolean} ${dut.sysclk.toBigInt} ${simTime()}")

              if(!dut.io.pmb.cmd.write.toBoolean) {
                q.enqueue(dut.io.pmb.cmd.address.toBigInt)
              }

              val tran = WishboneTransaction(dut.io.pmb.cmd.address.toBigInt >> log2Up(config.wordAddressInc()), dut.io.pmb.cmd.data.toBigInt)
              println(s"Popping ${tran}")
              sco.pushDut(tran)

              dut.io.pmb.cmd.ready #= false
            }

            dut.clockDomain.waitSampling()
          }
        }

        dut.clockDomain.onSamplings({
          if (!dut.io.pmb.cmd.ready.toBoolean) {
            dut.io.pmb.cmd.ready.randomize()
          }
        })

        val dri = new WishboneDriver(dut.io.wb, dut.clockDomain)
        for (repeat <- 0 until 1000) {
          seq.generateTransactions()
          var tran = seq.nextTransaction(0)
          val we = Random.nextBoolean()
          if (!we) {
            tran = tran.copy(data = BigInt(0xdeadbeefL))
          }
          println(s"Pushing ${tran} write: ${we} ${dut.sysclk.toBigInt}")
          sco.pushRef(tran)
          dut.io.wb.DAT_MOSI #= tran.data
          dri.drive(tran, we)
          dut.clockDomain.waitSampling()
        }
        running = false
        wb_thread.join()

      }
  }

  test("WishboneToPipelinedMemoryBus_non_pipelined") {
    runTest(WishboneConfig(32, 32, addressGranularity = AddressGranularity.BYTE), 10)
  }
//  test("WishboneToPipelinedMemoryBus_no_queue") {
//    runTest(WishboneConfig(32, 32, addressGranularity = AddressGranularity.BYTE).pipelined, 0)
//  }
//  test("WishboneToPipelinedMemoryBus") {
//    runTest(WishboneConfig(32, 32, addressGranularity = AddressGranularity.BYTE).pipelined, 10)
//  }
}
