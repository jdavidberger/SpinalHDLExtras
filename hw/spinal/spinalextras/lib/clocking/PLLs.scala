package spinalextras.lib.clocking

import spinal.core.{Bool, ClockDomain, Data, Device, False, FixedFrequency, crossClockDomain, out}
import spinal.lib.Counter
import spinalextras.lib.blackbox.ClockGenerator
import spinalextras.lib.blackbox.lattice.lifcl.PLLConfig
import spinalextras.lib.impl.ImplementationSpecificFactory
import spinalextras.lib.misc.ClockSpecification

trait PLL {
  def inputSpecification : ClockSpecification
  def outputSpectifications : Seq[ClockSpecification]

  val lock : Bool
  val ClockDomains : Seq[ClockDomain]
}

class SimulationPLL(input: ClockSpecification, outputs: Seq[ClockSpecification]) extends PLL {
  val lock = out(Bool())

  override val ClockDomains: Seq[ClockDomain] = {
    outputs.zipWithIndex.map(s => {
      val generator = new ClockGenerator(s._1.freq)
      new ClockDomain(generator.io.clk, frequency = FixedFrequency(s._1.freq))
    })
  }

  val counter = Counter(16)
  counter.increment()

  lock.addTag(crossClockDomain)
  lock.setAsReg() setWhen(counter.willOverflow) init(False)

  override def inputSpecification: ClockSpecification = input
  override def outputSpectifications: Seq[ClockSpecification] = outputs
}

object PLLs {
  def factory[T <: Data] = new ImplementationSpecificFactory[PLL, (ClockSpecification, Seq[ClockSpecification])] {
    simulationHandler = {
      case _ => args => {
        new SimulationPLL(args._1, args._2)
      }
    }

    AddHandler {
      case Device("lattice", "lifcl", name, resetKind) => {
        case (external_clock, outputClocks) =>
          new spinalextras.lib.blackbox.lattice.lifcl.PLL(PLLConfig.create(external_clock,
            outputClocks: _*
          ))
      }
    }

  }

  def apply(external_clock : ClockSpecification, output_clocks : Seq[ClockSpecification]) = factory(external_clock, output_clocks)
}