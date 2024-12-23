package spinalextras.lib.misc

import spinal.core.ClockDomain.ClockFrequency
import spinal.core.{ClockDomain, FixedFrequency, HertzNumber}

case class ClockSpecification(freq: HertzNumber,
                              phaseOffset: Double = 0, tolerance: Double = 0.01) {
  def toClockFrequency() = {
    FixedFrequency(freq)
  }
}

object ClockSpecification {
  def fromClock(f: ClockDomain): ClockSpecification = {
    fromFrequency(f.frequency)
  }
  def fromFrequency(f: ClockFrequency): ClockSpecification = {
    ClockSpecification(f.getValue, tolerance = math.max((f.getMax / f.getValue).toDouble - 1, 1 - (f.getMin / f.getValue).toDouble))
  }
}