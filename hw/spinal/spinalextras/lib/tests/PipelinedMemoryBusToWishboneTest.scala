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
import spinalextras.lib.bus._

import scala.collection.mutable
import scala.language.postfixOps
import scala.util.Random

class PipelinedMemoryBusToWishboneTest extends AnyFunSuite {
  def runTest(config: WishboneConfig): Unit = {
    Config.sim.withWave
      .doSim(
        new PipelinedMemoryBusToWishbone(config, PipelinedMemoryBusConfig(32, 32)) {
          val sysclk = Reg(UInt(32 bits)) init (0)
          sysclk := sysclk + 1
          SimPublic(sysclk)
        }.setDefinitionName("PipelinedMemoryBusToWishbone")
      ) { dut =>
        dut.io.wb.DAT_MISO #= 0xcafecafeL
        dut.io.pmb.cmd.valid #= false
        dut.io.wb.ACK #= false
        dut.io.pmb.cmd.valid #= false

        dut.clockDomain.forkStimulus(100 MHz)
        SimTimeout(1 ms)
        dut.clockDomain.waitSampling(1)

        def fixAddress(address: BigInt): BigInt = {
          if (config.wordAddressInc() > 1) {
            address & ~(config.wordAddressInc() - 1)
          } else {
            address
          }
        }

        val pmb_resp = ScoreboardInOrder[BigInt]()
        FlowMonitor(dut.io.pmb.rsp, dut.clockDomain) { rsp =>
          println(s"Got d: ${rsp.data.toBigInt} ${dut.sysclk.toBigInt} ${simTime()}")
          assert(rsp.data.toBigInt != BigInt(0xcafecafeL))
          pmb_resp.pushDut(rsp.data.toBigInt)
        }

        val sco = ScoreboardInOrder[(BigInt, BigInt, Boolean)]()
        val seq = WishboneSequencer {
          WishboneTransaction(BigInt(Random.nextInt(200) * 4), BigInt(Random.nextInt(200)))
        }
        var running = true
        var waitForAck = false

        val wb_thread = fork {
          while (running) {
            dut.io.wb.ACK #= false

            dut.clockDomain.waitSamplingWhere(dut.io.wb.CYC.toBoolean && dut.io.wb.STB.toBoolean)

            dut.clockDomain.waitSampling(Random.nextInt(5))

            val addr = dut.io.wb.ADR.toInt
            val was_write = dut.io.wb.WE.toBoolean

            val response = dut.io.wb.DAT_MISO.randomize()
            dut.io.wb.ACK #= true

            if(!was_write) {
              println(s"Pushing WB ACK ${addr} ${was_write} ${response} ${dut.sysclk.toBigInt}")
              pmb_resp.pushRef(response)
            }

            dut.clockDomain.waitSampling()
            dut.io.wb.ACK #= false
          }
        }

        val word_shift = log2Up(config.wordAddressInc())
        for (repeat <- 0 until 100) {
          val address = Random.nextInt(1 << 20)
          val data = Random.nextInt(1 << 30)
          val we = Random.nextBoolean()

          println(s"Pushing ${data} ${address << word_shift} write: ${we} ${dut.sysclk.toBigInt}")
          sco.pushRef((address, data, we))

          dut.clockDomain.waitSampling(Random.nextInt(5))

          dut.io.pmb.cmd.payload.address #= (address >> word_shift) << dut.io.pmb.config.wordAddressShift
          dut.io.pmb.cmd.write #= we
          dut.io.pmb.cmd.valid #= true
          dut.io.pmb.cmd.data #= data
          dut.io.pmb.cmd.mask #= (1 << dut.io.pmb.cmd.mask.getBitsWidth) - 1
          dut.clockDomain.waitSamplingWhere(dut.io.pmb.cmd.ready.toBoolean)
          dut.io.pmb.cmd.valid #= false

          dut.io.pmb.cmd.payload.address.randomize()
          dut.io.pmb.cmd.data.randomize()
        }
        running = false

        wb_thread.join()

        sco.check()

      }
  }


  test("PipelinedMemoryBusToWishbone_word_std") {
    runTest(WishboneConfig(32, 32, addressGranularity = AddressGranularity.WORD))
  }
  //  test("PipelinedMemoryBusToWishbone_word") {
  //    runTest(WishboneConfig(32, 32, addressGranularity = AddressGranularity.WORD).pipelined)
  //  }

  test("PipelinedMemoryBusToWishbone_std") {
    runTest(WishboneConfig(32, 32, addressGranularity = AddressGranularity.BYTE))
  }
  //  test("PipelinedMemoryBusToWishbone") {
  //    runTest(WishboneConfig(32, 32, addressGranularity = AddressGranularity.BYTE).pipelined)
  //  }
}
