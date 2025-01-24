package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim.{SimBitVectorPimper, SimBitsPimper, SimBoolPimper, SimClockDomainHandlePimper, SimEquivBitVectorBigIntPimper, SimTimeout, simRandom}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusCmd, PipelinedMemoryBusConfig}
import spinal.lib.sim.{FlowMonitor, ScoreboardInOrder, StreamMonitor}
import spinal.lib.{master, slave}
import spinalextras.lib.Config
import spinalextras.lib.bus.{PipelinedMemoryBusCmdExt, PipelinedMemoryBusConfigExt}
import spinalextras.lib.bus.simple.{PipelineMemoryBusWidthAdapter, SimpleMemoryProvider}

class PipelineMemoryBusWidthAdapterTest extends AnyFunSuite {
  def byte_mask_to_bit_mask(mask : BigInt) = mask.toString(2).map(x => BigInt("" + x) * 0xff).fold(BigInt(0))((x,y) => (x << 8) | y )

  def runTest(configIn: PipelinedMemoryBusConfig, configOut: PipelinedMemoryBusConfig, endianness : Endianness): Unit = {
    Config.sim.withWave
      .doSim(new Component {
        val io = new Bundle {
          val bus = slave(PipelinedMemoryBus(configIn))
        }
        val busOut = PipelinedMemoryBus(configOut)
        val adapter = PipelineMemoryBusWidthAdapter(configIn, configOut, endianness = endianness)
        adapter.io.input <> io.bus
        adapter.io.output <> busOut
        val mem = SimpleMemoryProvider(mapping = SizeMapping(0, 0x10000), config = configOut)
        mem.io.bus <> busOut
      }.setDefinitionName(s"PipelineMemoryBusWidthAdapterTest_${configIn.dataWidth}_${configOut.dataWidth}_${endianness.getClass.getSimpleName}")) { dut =>
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
        val data_max = ((BigInt(1) << configIn.dataWidth) - 1)

        for (i <- 0 until 1000) {
          val data = BigInt((i + 0xabcd123456789L) % (1L << configIn.dataWidth - 1))
          dut.io.bus.cmd.valid #= true
          dut.io.bus.cmd.data #= (data_max - data)
          dut.io.bus.cmd.address #= (i << dut.io.bus.config.wordAddressShift)
          dut.io.bus.cmd.write #= true
          dut.clockDomain.waitSamplingWhere(dut.io.bus.cmd.ready.toBoolean)
          dut.io.bus.cmd.valid #= false
        }

        for (i <- 0 until 1000) {
          val data = BigInt((i + 0xabcd123456789L) % (1L << configIn.dataWidth - 1))

          val bit_mask = (dut.io.bus.cmd.mask.randomize())
          val mask = byte_mask_to_bit_mask(bit_mask)

          dut.io.bus.cmd.valid #= true
          dut.io.bus.cmd.data #= data
          dut.io.bus.cmd.address #= (i << dut.io.bus.config.wordAddressShift)
          dut.io.bus.cmd.write #= true
          sco.pushRef((data & mask) | ((data_max - data) & (data_max - mask)))

          dut.clockDomain.waitSamplingWhere(dut.io.bus.cmd.ready.toBoolean)
          dut.io.bus.cmd.valid #= false
        }

        for (i <- 0 until 1000) {
          dut.io.bus.cmd.valid #= true
          dut.io.bus.cmd.data #= 0xdeadbeefL % (1L << configIn.dataWidth - 1)
          dut.io.bus.cmd.address #= i << dut.io.bus.config.wordAddressShift
          dut.io.bus.cmd.write #= false
          dut.clockDomain.waitSamplingWhere(dut.io.bus.cmd.ready.toBoolean)
          dut.io.bus.cmd.valid #= false
        }

        dut.clockDomain.waitSampling(30)

        sco.checkEmptyness()
      }
  }

  def runInOutTest(configIn: PipelinedMemoryBusConfig, configOut: PipelinedMemoryBusConfig, endianness : Endianness): Unit = {
    Config.sim.withWave
      .doSim(new Component {
        val io = new Bundle {
          val bus = slave(PipelinedMemoryBus(configIn))
          val busOut = master(PipelinedMemoryBus(configIn))
        }
        val adapter = PipelineMemoryBusWidthAdapter(configIn, configOut, endianness = endianness)
        adapter.io.input <> io.bus

        val adapterBack = PipelineMemoryBusWidthAdapter(configOut, configIn, endianness = endianness)
        adapterBack.io.input <> adapter.io.output
        adapterBack.io.output <> io.busOut
      }.setDefinitionName(s"PipelineMemoryBusWidthAdapterTest_${configIn.dataWidth}_${configOut.dataWidth}_${endianness.getClass.getSimpleName}")) { dut =>
        dut.io.bus.cmd.valid #= false
        dut.io.bus.cmd.mask #= (1 << (configIn.dataWidth / 8)) - 1
        dut.io.busOut.cmd.ready #= false
        dut.io.busOut.rsp.valid #= false

        dut.clockDomain.forkStimulus(100 MHz)
        SimTimeout(1 ms)
        dut.clockDomain.waitSampling(1)

        def tupelize(cmd : PipelinedMemoryBusCmd) = {
          (cmd.address.toBigInt, cmd.data.toBigInt & byte_mask_to_bit_mask(cmd.mask.toBigInt), cmd.mask.toBigInt, cmd.write.toBoolean)
        }
        val sco = ScoreboardInOrder[(BigInt, BigInt, BigInt, Boolean)]()
        StreamMonitor(dut.io.busOut.cmd, dut.clockDomain) {
          d => if(d.mask.toBigInt != 0) {
            sco.pushDut(tupelize(d))
          }
        }
        dut.io.busOut.cmd.ready #= true

        dut.clockDomain.waitSamplingWhere(dut.io.bus.cmd.ready.toBoolean)

        for (i <- 0 until 1000) {
          val data = BigInt((i + 0xabcd123456789L) % (1L << configIn.dataWidth - 1))
          val byte_mask = 1 << (simRandom.nextInt(configIn.dataWidth / 8))
          dut.io.bus.cmd.mask #= byte_mask
          dut.io.bus.cmd.valid #= true
          dut.io.bus.cmd.data.randomize()
          dut.io.bus.cmd.address #= (simRandom.nextInt(10000) << dut.io.bus.config.wordAddressShift)
          dut.io.bus.cmd.write #= true
          dut.clockDomain.waitSamplingWhere(dut.io.bus.cmd.ready.toBoolean)
          dut.io.bus.cmd.valid #= false

          sco.pushRef(tupelize(dut.io.bus.cmd.payload))
        }

        dut.clockDomain.waitSampling(30)

        sco.checkEmptyness()
      }
  }

  test(s"PipelinedMemoryBusAdapterReadBackTest_${8}_${32}_little") {
    runInOutTest(PipelinedMemoryBusConfig(32, 8), PipelinedMemoryBusConfig(32, 32), LITTLE)
  }

  for (inWidth <- Seq(8, 16, 32, 64, 128)) {
    for (outWidth <- Seq(8, 16, 32, 64, 128)) {
      for(endianness <- Seq(LITTLE, BIG)) {
        test(s"PipelinedMemoryBusAdapterReadBackTest_${inWidth}_${outWidth}_${endianness.getClass.getSimpleName}") {
          runInOutTest(PipelinedMemoryBusConfig(32, inWidth), PipelinedMemoryBusConfig(32, outWidth), endianness)
        }
      }
    }
  }
//
//  for (inWidth <- Seq(8, 16, 32, 64, 128)) {
//    for (outWidth <- Seq(8, 16, 32, 64, 128)) {
//      for(endianness <- Seq(LITTLE, BIG)) {
//        test(s"PipelinedMemoryBusAdapterTest_${inWidth}_${outWidth}_${endianness.getClass.getSimpleName}") {
//          runTest(PipelinedMemoryBusConfig(32, inWidth), PipelinedMemoryBusConfig(32, outWidth), endianness)
//        }
//      }
//    }


}
