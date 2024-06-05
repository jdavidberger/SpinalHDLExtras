package spinalextras.lib

import spinal.core.ClockDomain.ClockFrequency
import spinal.core.HertzNumber

case class FixedFrequencyWithError(value: HertzNumber, error_percentage: Double) extends ClockFrequency {
  def getValue: HertzNumber = value
  def getMax:   HertzNumber = value * (1 + error_percentage)
  def getMin:   HertzNumber = value * (1 - error_percentage)
}

case class FixedRangeFrequency(minV : HertzNumber, maxV : HertzNumber) extends ClockFrequency {
  require(minV <= maxV)
  def getValue: HertzNumber = (minV + maxV) / 2
  def getMax:   HertzNumber = maxV
  def getMin:   HertzNumber = minV
}