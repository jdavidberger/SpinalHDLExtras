package spinalextras.lib.soc.spinex

import spinal.core._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config, Apb3SlaveFactory}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.com.jtag.{Jtag, JtagTapInstructionCtrl}
import spinal.lib.slave
import spinalextras.lib.{Memories, MemoryRequirement}
import spinalextras.lib.blackbox.opencores.i2c_master_top
import spinalextras.lib.bus.PipelinedMemoryBusExt
import spinalextras.lib.misc.{RandomNumberGenerator, RandomNumberGeneratorApb3}
import spinalextras.lib.soc.DeviceTree
import spinalextras.lib.soc.peripherals.SpinexApb3Timer

import scala.language.postfixOps

case class ExternalInterrupts(externalInterruptBits : Int) extends SpinexPlugin {
  lazy val externalInterrupts = in(Bits(externalInterruptBits bits)) default(0)
  override def apply(som: Spinex): Unit = {
    som.io.valCallback(externalInterrupts, s"externalInterrupts")
    som.system.addInterrupt(externalInterrupts)
  }
}

object ExternalInterrupts {
  def apply(interrupt : Data) = {
    val plugin = new ExternalInterrupts(interrupt.getBitsWidth)
    Component.current.addPrePopTask(() => {
      plugin.externalInterrupts := interrupt.asBits
    })
    plugin
  }
}

case class SystemRam(mapping : SizeMapping = SizeMapping(0x40000000l, 0x00010000 Bytes)) extends SpinexPlugin {
  override def apply(som: Spinex): Unit = {
    val mem = Memories(MemoryRequirement(Bits(32 bits), mapping.size / 4,
      numReadWritePorts = 2,
      needsMask = true, label = "spinex_ram"))
    val mem_pmbs = mem.pmbs()
    som.add_slave(mem_pmbs(1).resizeAddress(32), "ram", mapping, "dBus")
    som.add_slave(mem_pmbs(0).resizeAddress(32), "ram", mapping, "iBus")
  }
}

case class PrintAPBMapping() extends SpinexPlugin {
  override def apply(som: Spinex): Unit = {
    println("APB Mappings: ")
    for (m <- som.system.apbMapping.sortBy(_._2.base)) {
      println(s"\t - ${m._2}: \t${m._1}")
    }
    println("Interrupts: ")
    for(i <- som.system.interruptInfos) {
      println(s"\t - ${i._2}: \t${i._1}")
    }
  }
}

case class RandomPlugin(registerLocation : BigInt = 0x60, withSeed : Boolean = true) extends SpinexRegisterFileApbPlugin("rng", SizeMapping(registerLocation, 16 Bytes)) {
  var _regs : Seq[(String, SizeMapping)] = null
  override def regs = _regs

  override def apply(som: Spinex): Unit = {
    val rng = new RandomNumberGeneratorApb3(withSeed = withSeed)
    rng.io.apb <> apb
    _regs = busCtrlToRegs(rng.busCtrl)
    super.apply(som)
  }
}



