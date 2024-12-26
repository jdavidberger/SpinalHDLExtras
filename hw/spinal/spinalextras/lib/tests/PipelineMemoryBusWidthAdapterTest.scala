package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.core._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusConfig}
import spinal.lib.sim.{FlowMonitor, ScoreboardInOrder}
import spinal.lib.slave
import spinalextras.lib.Config
import spinalextras.lib.bus.simple.{PipelineMemoryBusWidthAdapter, SimpleMemoryProvider}

class PipelineMemoryBusWidthAdapterTest extends AnyFunSuite {
  def runTest(configIn: PipelinedMemoryBusConfig, configOut: PipelinedMemoryBusConfig): Unit = {
    Config.sim.withWave
      .doSim(new Component {
        val io = new Bundle {
          val bus = slave(PipelinedMemoryBus(configIn))
        }
        val busOut = PipelinedMemoryBus(configOut)
        val adapter = PipelineMemoryBusWidthAdapter(configIn, configOut)
        adapter.io.input <> io.bus
        adapter.io.output <> busOut
        val mem = SimpleMemoryProvider(mapping = SizeMapping(0, 0x10000), config = configOut)
        mem.io.bus <> busOut
      }.setDefinitionName(s"PipelineMemoryBusWidthAdapterTest_${configIn.dataWidth}_${configOut.dataWidth}")) { dut =>
        dut.io.bus.cmd.valid #= false
        dut.io.bus.cmd.mask #= (1 << (configIn.dataWidth / 8)) - 1
        dut.clockDomain.forkStimulus(100 MHz)
        SimTimeout(1 ms)
        dut.clockDomain.waitSampling(1)

        val sco = ScoreboardInOrder[BigInt]()

        FlowMonitor(dut.io.bus.rsp, dut.clockDomain) {
          d => sco.pushDut(d.data.toBigInt)
        }

        dut.clockDomain.waitSamplingWhere(dut.io.bus.cmd.ready.toBoolean)

        for (i <- 0 until 1000) {
          val data = (i + 101) % (1L << configIn.dataWidth - 1)
          dut.io.bus.cmd.valid #= true
          dut.io.bus.cmd.data #= data
          dut.io.bus.cmd.address #= i
          dut.io.bus.cmd.write #= true
          sco.pushRef(data)
          dut.clockDomain.waitSamplingWhere(dut.io.bus.cmd.ready.toBoolean)
          dut.io.bus.cmd.valid #= false
        }

        for (i <- 0 until 1000) {
          dut.io.bus.cmd.valid #= true
          dut.io.bus.cmd.data #= 0xdeadbeefL % (1L << configIn.dataWidth - 1)
          dut.io.bus.cmd.address #= i
          dut.io.bus.cmd.write #= false
          dut.clockDomain.waitSamplingWhere(dut.io.bus.cmd.ready.toBoolean)
          dut.io.bus.cmd.valid #= false
        }

        dut.clockDomain.waitSampling(30)

        sco.checkEmptyness()
      }
  }

  for (inWidth <- Seq(8, 16, 32, 64, 128)) {
    for (outWidth <- Seq(8, 16, 32, 64, 128)) {
      test(s"PipelinedMemoryBusToWishboneTest_${inWidth}_${outWidth}") {
        runTest(PipelinedMemoryBusConfig(32 - log2Up(inWidth / 8), inWidth), PipelinedMemoryBusConfig(32 - log2Up(outWidth / 8), outWidth))
      }
    }
  }

}
