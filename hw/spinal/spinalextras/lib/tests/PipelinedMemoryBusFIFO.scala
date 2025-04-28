import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.core._
import spinal.lib.bus.misc.DefaultMapping
import spinal.lib.bus.simple.PipelinedMemoryBusConfig
import spinal.lib.{master, slave}
import spinalextras.lib.Config
import spinalextras.lib.bus.PipelineMemoryGlobalBus
import spinalextras.lib.bus.simple.SimpleMemoryProvider
import spinalextras.lib.memory.PipelinedMemoryBusFIFO

class PipelinedMemoryBusFIFOTest extends AnyFunSuite {
  def runTest(): Unit = {
    Config.sim.withWave
      .doSim(
        new Component {
          val dataType = UInt(8 bits)
          val sysBus = PipelineMemoryGlobalBus(PipelinedMemoryBusConfig(32, 8))
          val busSlave = sysBus.add_slave("default", DefaultMapping)

          val fifo = new PipelinedMemoryBusFIFO(UInt(8 bits), (0, 3), sysBus = Some(sysBus))
          val io = new Bundle {
            val push = slave Stream(dataType)
            val pop = master Stream(dataType)
          }

          val mem = new SimpleMemoryProvider(mapping = fifo.sm, config = sysBus.config)
          mem.io.bus <> busSlave

          fifo.io.push <> io.push
          fifo.io.pop <> io.pop

        }.setDefinitionName("PipelinedMemoryBusToWishbone")
      ) { dut =>
        SimTimeout(10 us)

        dut.io.push.valid #= false
        dut.io.pop.ready #= false

        dut.clockDomain.forkStimulus(100 MHz)
        dut.clockDomain.waitSampling(1)

        dut.fifo.formalAssertProperties()

        for(i <- 0 to 100) {
          dut.io.push.valid #= true
          dut.io.push.payload #= i

          dut.clockDomain.waitSampling()
          while(!dut.io.pop.ready.toBoolean) {
            dut.io.pop.ready.randomize()
            dut.clockDomain.waitSampling()
          }
          dut.io.pop.ready #= false
        }

      }
  }


  test("PipelinedMemoryBusFIFO") {
    runTest()
  }
}