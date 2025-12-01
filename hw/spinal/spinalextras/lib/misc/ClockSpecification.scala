package spinalextras.lib.misc

import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.databind.DeserializationContext
import com.fasterxml.jackson.databind.deser.std.StdDeserializer
import com.fasterxml.jackson.databind.node.ObjectNode
import spinal.core.ClockDomain.ClockFrequency
import spinal.core.{ClockDomain, FixedFrequency, HertzNumber}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


case class HertzDeserializer() extends StdDeserializer[HertzNumber](classOf[HertzNumber]) {
  override def deserialize(p: JsonParser, ctxt: DeserializationContext) = {
    val t = p.readValueAsTree[ObjectNode]()
    HertzNumber(t.get("value").asDouble())
  }
}

case class ClockSpecification(freq: HertzNumber,
                              phaseOffset: Double = 0, tolerance: Double = 0.01) {
  assert(tolerance < 1, "Tolerance is specified between 0 and 1; 0.5 for instance means 50% tolerance")
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
  def removeRedundant(specs : Seq[ClockSpecification]): Seq[ClockSpecification] = {
    val allTolerances = new mutable.HashMap[(HertzNumber, Double), mutable.ArrayBuffer[Double]]()
    specs.foreach(x => allTolerances.getOrElseUpdate((x.freq, x.phaseOffset), new ArrayBuffer[Double]()).append(x.tolerance))
    val minTolerances = allTolerances.mapValues(_.min)
    minTolerances.map(x => ClockSpecification(x._1._1, x._1._2, x._2)).toSeq
  }
}