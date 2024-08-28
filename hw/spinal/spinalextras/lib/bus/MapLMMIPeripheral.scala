package spinalextras.lib.bus

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.misc.{AddressMapping, MaskMapping}
import spinal.lib.bus.simple._
import spinalextras.lib.Config
import spinalextras.lib.misc._

case class MapLMMIPeripheral(globalBus : WishboneGlobalBus, mapping : AddressMapping, lmmi_prefix : String = "lmmi") extends Component {
  val io = new Bundle {
    val lmmi = master(PipelinedMemoryBus(PipelinedMemoryBusConfig(6, 8)))
  }
  noIoPrefix()
  io.lmmi.cmd.valid.setName(s"${lmmi_prefix}_request")
  io.lmmi.cmd.address.setName(s"${lmmi_prefix}_offset")
  io.lmmi.cmd.write.setName(s"${lmmi_prefix}_wr_rdn")
  io.lmmi.cmd.data.setName(s"${lmmi_prefix}_wdata")
  io.lmmi.cmd.mask.setName(s"${lmmi_prefix}_wdata_mask")
  io.lmmi.cmd.ready.setName(s"${lmmi_prefix}_ready")

  io.lmmi.rsp.valid.setName(s"${lmmi_prefix}_rdata_valid")
  io.lmmi.rsp.data.setName(s"${lmmi_prefix}_rdata")

  var bus = globalBus.add_slave(s"${lmmi_prefix}_ctrl", mapping, "cpu")
  val lmmi = io.lmmi

  PipelinedMemoryBusToWishbone(lmmi, bus.config, addressMap = x => (x >> 2)) <> bus
}

class MapLMMIPeripheralTest extends AnyFunSuite {
  test("Basic") {
    Config.sim.doSim(new PipelinedMemoryBusTimeout(PipelinedMemoryBusConfig(32, 32))) { dut =>
      dut.clockDomain.forkStimulus(100 MHz)
      dut.io.pmb_s.cmd.valid #= false
      dut.io.pmb_m.cmd.ready #= false
      dut.io.pmb_m.rsp.valid #= false
      dut.clockDomain.waitSampling(5)
      SimTimeout(1000 us)

      dut.io.pmb_s.cmd.address #= 0xbb000000L
      dut.io.pmb_s.cmd.mask #= 0xf
      dut.io.pmb_s.cmd.write #= false
      dut.io.pmb_s.cmd.data #= 2
      dut.io.pmb_s.cmd.valid #= true

      dut.clockDomain.waitSamplingWhere(dut.io.pmb_s.cmd.ready.toBoolean)
      dut.clockDomain.waitSampling(100)
    }
  }
  test("Read timeout") {
    Config.sim.doSim(new PipelinedMemoryBusTimeout(PipelinedMemoryBusConfig(32, 32))) { dut =>
      dut.clockDomain.forkStimulus(100 MHz)
      dut.io.pmb_s.cmd.valid #= false
      dut.io.pmb_m.cmd.ready #= true
      dut.io.pmb_m.rsp.valid #= false
      dut.clockDomain.waitSampling(5)
      SimTimeout(1000 us)

      dut.io.pmb_s.cmd.address #= 0xbb000000L
      dut.io.pmb_s.cmd.mask #= 0xf
      dut.io.pmb_s.cmd.write #= false
      dut.io.pmb_s.cmd.data #= 2
      dut.io.pmb_s.cmd.valid #= true

      dut.clockDomain.waitSamplingWhere(dut.io.pmb_s.cmd.ready.toBoolean)
      dut.io.pmb_s.cmd.valid #= false
      dut.clockDomain.waitSampling(100)
      dut.clockDomain.waitSamplingWhere(dut.io.pmb_s.rsp.valid.toBoolean)
      dut.clockDomain.waitSampling(100)
    }
  }
  test("No timeout") {
      Config.sim.doSim(new PipelinedMemoryBusTimeout(PipelinedMemoryBusConfig(32, 32))) { dut =>
        dut.clockDomain.forkStimulus(100 MHz)
        dut.io.pmb_s.cmd.valid #= false
        dut.io.pmb_m.cmd.ready #= true
        dut.io.pmb_m.rsp.valid #= false
        dut.clockDomain.waitSampling(5)
        SimTimeout(1000 us)

        dut.io.pmb_s.cmd.address #= 0xbb000000L
        dut.io.pmb_s.cmd.mask #= 0xf
        dut.io.pmb_s.cmd.write #= false
        dut.io.pmb_s.cmd.data #= 2
        dut.io.pmb_s.cmd.valid #= true

        dut.clockDomain.waitSamplingWhere(dut.io.pmb_s.cmd.ready.toBoolean)
        dut.io.pmb_s.cmd.valid #= false
        dut.io.pmb_m.rsp.valid #= true
        dut.io.pmb_m.rsp.data #= 0x04
        dut.clockDomain.waitSampling(100)
      }
  }
}
