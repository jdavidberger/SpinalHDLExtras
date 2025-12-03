package spinalextras.lib.memory

import spinal.core._
import spinalextras.lib.impl.ImplementationSpecificFactory
import spinalextras.lib.lattice.LatticeMemories

import scala.language.{implicitConversions, postfixOps}

class MemoryRequirementBits(dataWidth : Int, num_elements : BigInt, numReadWritePorts : Int, numReadPorts : Int, numWritePorts : Int) extends
  MemoryRequirement(Bits(dataWidth bits), num_elements = num_elements, numReadWritePorts = numReadWritePorts, numReadPorts = numReadPorts, numWritePorts = numWritePorts) {

  override def toString: String =
    s"${dataWidth}bits_${num_elements}d_${numReadWritePorts}rw_${numReadPorts}r_${numWritePorts}"
}

object Memories {
  def factory[T <: Data](allowNativeMem : Boolean = true): ImplementationSpecificFactory[HardwareMemory[T], (MemoryRequirement[T], MemTechnologyKind)] =
    new ImplementationSpecificFactory[HardwareMemory[T], (MemoryRequirement[T], MemTechnologyKind)] {
      simulationHandler = {
        case _ => args => { MemBackedHardwardMemory[T](args._1)
        }
      }
      AddHandler { case Device("lattice", "lifcl", name, resetKind) => { args => LatticeMemories[T](args._2)(args._1) } }

      if(allowNativeMem) {
        AddHandler { case _ => args => MemBackedHardwardMemory[T](args._1) }
      }
    }

  def apply[T <: Data](reqs : MemoryRequirement[T], technologyKind: MemTechnologyKind = auto): HardwareMemory[T] = factory()(reqs, technologyKind)
  def getSpecialMemory[T <: Data](reqs : MemoryRequirement[T], technologyKind: MemTechnologyKind = auto): HardwareMemory[T] = factory(allowNativeMem = false)(reqs, technologyKind)

  def applyAuto[T <: Data](reqs: MemoryRequirement[T]): HardwareMemory[T] = apply(reqs, auto)
}
