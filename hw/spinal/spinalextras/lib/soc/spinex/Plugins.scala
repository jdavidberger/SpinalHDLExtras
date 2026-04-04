package spinalextras.lib.soc.spinex

import spinal.core._
import spinal.lib.bus.misc.SizeMapping
import spinalextras.lib.bus.PipelinedMemoryBusExt
import spinalextras.lib.memory.{HardwareMemory, Memories, MemoryRequirement}
import spinalextras.lib.misc.RandomNumberGeneratorApb3

import java.io.File
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.Files
import scala.language.postfixOps

class ExternalInterrupts(name : String, externalInterruptBits : Int, requestIdx : Int = -1) extends SpinexPlugin {
  lazy val externalInterrupts = in(Bits(externalInterruptBits bits)) default(0)
  override def apply(som: Spinex): Unit = {
    som.io.valCallback(externalInterrupts, s"externalInterrupts")
    som.system.addNamedInterrupt(name, externalInterrupts, requestIdx)
  }
}

object ExternalInterrupts {
  def apply(interrupt : Data, requestIdx : Int = -1) = {
    val plugin = new ExternalInterrupts(interrupt.name, interrupt.getBitsWidth, requestIdx)

    Component.current.addPrePopTask(() => {
      plugin.externalInterrupts := interrupt.asBits
    })
    plugin
  }
}

case class SystemRam(name : String = "spinex_ram", mapping : SizeMapping = SizeMapping(0x40000000l, 0x00010000 Bytes)) extends SpinexPlugin {
  var mem : HardwareMemory[Bits] = null
  override def apply(som: Spinex): Unit = {
    mem = Memories(MemoryRequirement(Bits(32 bits), mapping.size / 4,
      numReadWritePorts = 2,
      needsMask = true, label =name))

    val mem_pmbs = mem.pmbs()
    som.add_slave(mem_pmbs(1).resizeAddress(32), "ram", mapping, direct = true, "dBus")
    som.add_slave(mem_pmbs(0).resizeAddress(32), "ram", mapping, direct = true, "iBus")
  }
  def init_rom(filename : String): Unit = {
    val bytes = Files.readAllBytes(new File(filename).toPath)
    val buffer = ByteBuffer.wrap(bytes)
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    val intBuffer = buffer.asIntBuffer()

    // Copy to a Scala Array
    val result = new Array[Int](intBuffer.remaining())
    intBuffer.get(result)

    mem.init(result.map(x => BigInt(x & 0xffffffffL)))
  }
}

case class PrintAPBMapping() extends SpinexPlugin {
  override def apply(som: Spinex): Unit = {
    println("APB Mappings: ")
    for (m <- som.system.apbMapping.sortBy(_._2.base)) {
      println(s"\t - ${m._2}: \t${m._1}")
    }
    println("Interrupts: ")
    for(i <- som.system.interruptInfos.keys.toSeq.sorted) {
      println(s"\t - ${i}: \t${som.system.interruptInfos(i)}")
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



