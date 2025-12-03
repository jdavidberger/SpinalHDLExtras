package spinalextras.lib.lattice

import spinal.core.{Bits, Component, Data, IntToBuilder, MemTechnologyKind, SpinalInfo, SpinalWarning}
import spinalextras.lib.blackbox.lattice.lifcl.{DP16K_Mem, DPSC512K_Mem, LscRamDpTrue_Mem, PDPSC16K_Mem, PDPSC512K_Mem}
import spinalextras.lib.memory.{HardwareMemory, MemBackedHardwardMemory, MemoryRequirement, StackedHardwareMemory}

import scala.collection.mutable
import scala.language.postfixOps

object LatticeMemories {
  var lram_available_maps = new mutable.HashMap[Component, Int]()

  def lram_available = lram_available_maps.getOrElse(Component.toplevel, 5)

  def find_lram[T <: Data](requirements: MemoryRequirement[T]): Option[() => HardwareMemory[Bits]] = {
    if (requirements.numPorts > 2)
      return None

    Some(
      () => {
        if (lram_available <= 0) {
          SpinalWarning(s"Out of LRAM for ${requirements}")
          MemBackedHardwardMemory(requirements.copy(dataType = Bits(32 bits)))
        } else {
          lram_available_maps(Component.toplevel) = lram_available - 1
          val latency = requirements.latencyRange._2.min(2)
          SpinalInfo(s"Using LRAM for ${requirements}, ${lram_available} lrams remaining")
          (requirements.numReadPorts, requirements.numWritePorts, requirements.numReadWritePorts) match {
            case (1, 1, 0) => new PDPSC512K_Mem(target_latency = latency, initialContent = requirements.initialContent)
            case (0, 0, 1) => new DPSC512K_Mem(target_latency = latency, read_write_ports = 1, initialContent = requirements.initialContent)
            case (0, 0, 2) => new DPSC512K_Mem(target_latency = latency, read_write_ports = 2, initialContent = requirements.initialContent)
          }
        }
      }
    )
  }

  def find_ebr[T <: Data](requirements: MemoryRequirement[T]): () => HardwareMemory[T] = {
    () => {
      val latency = requirements.latencyRange._2.min(2)
      (requirements.numReadPorts, requirements.numWritePorts, requirements.numReadWritePorts) match {
        case (1, 1, 0) => new PDPSC16K_Mem(data_pins = requirements.dataType.getBitsWidth, target_latency = latency).asType(requirements.dataType)
        case (0, 0, 2) => new DP16K_Mem(target_latency = latency).asType(requirements.dataType)
        case _ => new MemBackedHardwardMemory[T](requirements)
      }
    }
  }

  def apply[T <: Data](memKind: MemTechnologyKind)(requirements: MemoryRequirement[T]): HardwareMemory[T] = {
    val allocationSize = (requirements.dataType.getBitsWidth.min(32) * requirements.num_elements) / 8

    val shouldUseLRam = (memKind.technologyKind.toLowerCase == "lram" || allocationSize > (10 KiB))
    val lram_factory = find_lram(requirements)

    if (shouldUseLRam && lram_factory.isDefined) {
      new StackedHardwareMemory(requirements, lram_factory.get)
    } else {
      (requirements.numReadPorts, requirements.numWritePorts, requirements.numReadWritePorts) match {
        case (1, 1, 0) => new LscRamDpTrue_Mem(requirements)
        case (0, 0, 2) => new LscRamDpTrue_Mem(requirements)
        case (0, 0, 1) => new LscRamDpTrue_Mem(requirements)
        case _ => new MemBackedHardwardMemory[T](requirements)
      }
    }
  }
}
