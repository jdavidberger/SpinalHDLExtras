package spinalextras.lib.misc

import com.fasterxml.jackson.core.{JsonGenerator, JsonParser, TreeNode}
import com.fasterxml.jackson.databind.{DeserializationContext, SerializerProvider}
import com.fasterxml.jackson.databind.deser.std.StdDeserializer
import com.fasterxml.jackson.databind.node.ObjectNode
import com.fasterxml.jackson.databind.ser.std.StdSerializer
import spinal.core.ClockDomain.ClockFrequency
import spinal.core.{ClockDomain, FixedFrequency, HertzNumber, TimeNumber}
import spinalextras.lib.misc.ClockToleranceType.{AtMost, Centered}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


case class HertzDeserializer() extends StdDeserializer[HertzNumber](classOf[HertzNumber]) {
  override def deserialize(p: JsonParser, ctxt: DeserializationContext) : HertzNumber = {
    val suffixes : Map[String, Double] = Map(
      "ehz" -> 1e18,
      "phz" -> 1e15,
      "thz" -> 1e12,
      "ghz" -> 1e9,
      "mhz" -> 1e6,
      "khz" -> 1e3,
      "hz" -> 1,
    )
    if (p.getCurrentToken.toString == "START_OBJECT") {
      val tree : TreeNode = p.readValueAsTree()
      HertzNumber(BigDecimal(tree.get("value").toString))
    } else if(p.getCurrentToken.toString == "VALUE_STRING") {
      val t = p.getValueAsString.split(' ')
      HertzNumber(t(0).toDouble * suffixes(t(1).toLowerCase))
    } else {
      throw new Exception("Could not parse json hertz field")
    }
  }
}

case class TimeNumberDeserializer() extends StdDeserializer[TimeNumber](classOf[TimeNumber]) {
  override def deserialize(p: JsonParser, ctxt: DeserializationContext) = {
    val suffixes : Map[String, Double] = Map(
      "hr" -> 36000,
      "min" -> 60,
      "sec" -> 1,
      "ms" -> 1e-3,
      "us" -> 1e-6,
      "ns" -> 1e-9,
      "ps" -> 1e-12,
      "fs" -> 1e-15
    )

    val t = p.getValueAsString.split(' ')
    TimeNumber(t(0).toDouble * suffixes(t(1).toLowerCase))
  }
}

sealed trait ClockToleranceType
object ClockToleranceType {
  case object Centered extends ClockToleranceType;
  case object AtLeast extends ClockToleranceType;
  case object AtMost extends ClockToleranceType;
}

case class ClockToleranceTypeDeserializer() extends StdDeserializer[ClockToleranceType](classOf[ClockToleranceType]) {
  override def deserialize(p: JsonParser, ctxt: DeserializationContext) = {
    p.getText match {
      case "AtLeast" => ClockToleranceType.AtLeast
      case "AtMost" => ClockToleranceType.AtMost
      case _ => ClockToleranceType.Centered
    }
  }
}

case class ClockToleranceTypeSerializer() extends StdSerializer[ClockToleranceType](classOf[ClockToleranceType]) {

  override def serialize(value: ClockToleranceType, gen: JsonGenerator, provider: SerializerProvider) = {
    gen.writeString(value.toString)
  }
}

case class ClockSpecification(freq: HertzNumber,
                              phaseOffset: Double = 0, tolerance: Double = 0.01,
                              toleranceType: ClockToleranceType = ClockToleranceType.Centered) {
  assert(tolerance < 1, "Tolerance is specified between 0 and 1; 0.5 for instance means 50% tolerance")
  def toClockFrequency() = {
    FixedFrequency(freq)
  }

  def LowestFrequency: HertzNumber = toleranceType match {
    case ClockToleranceType.AtLeast => freq
    case _ => freq * (1 - tolerance)
  }

  def HighestFrequency: HertzNumber = {
    toleranceType match {
      case ClockToleranceType.AtMost => freq
      case _ => (freq * (1 + tolerance))
    }
  }

  def normalize : ClockSpecification = {
    toleranceType match {
      case ClockToleranceType.Centered => this
      case _ => {
        val newCenter = (LowestFrequency + HighestFrequency) / 2
        val newTolerance = 1 - (LowestFrequency / newCenter)
        new ClockSpecification(freq = newCenter,
          phaseOffset = phaseOffset, tolerance = newTolerance.doubleValue(), toleranceType = Centered)
      }
    }
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
    specs.map(_.normalize).foreach(x => allTolerances.getOrElseUpdate((x.freq, x.phaseOffset), new ArrayBuffer[Double]()).append(x.tolerance))
    val minTolerances = allTolerances.mapValues(_.min)
    minTolerances.filter(x => x._1._1.toInt != 0).map(x => ClockSpecification(x._1._1, x._1._2, x._2)).toSeq
  }
}