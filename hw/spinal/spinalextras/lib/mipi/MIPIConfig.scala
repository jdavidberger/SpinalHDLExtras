package spinalextras.lib.mipi

import spinalextras.lib.mipi.MIPIDataTypes.MIPIDataTypes


object MIPIDataTypes extends Enumeration {
  type MIPIDataTypes = Value
  val UNKNOWN = Value(-1)
  val FrameStartCode = Value(0x00)
  val FrameEndCode = Value(0x01)
  val Embedded8BitNonImageData = Value(0x12)
  
  val YUV4208Bit = Value(0x18)
  val YUV42010Bit = Value(0x19)
  val LegacyYUV4208Bit = Value(0x1A)
  val Reserved = Value(0x1B)
  val YUV4208BitChromaShifted = Value(0x1C)
  val YUV42010BitChromaShifted = Value(0x1D)
  val YUV4228Bit = Value(0x1E)
  val YUV42210Bit = Value(0x1F)

  val RGB444 = Value(0x20)
  val RGB555 = Value(0x21)

  val RGB565 = Value(0x22) //
  val RGB666 = Value(0x23)
  val RGB888 = Value(0x24)

  val RAW6 = Value(0x28)
  val RAW7 = Value(0x29)
  val RAW8 = Value(0x2A)
  val RAW10 = Value(0x2B)
  val RAW12 = Value(0x2C)
  val RAW14 = Value(0x2D)

  val UserDefined8BitDataType1 = Value(0x30)
  val UserDefined8BitDataType2 = Value(0x31)
  val UserDefined8BitDataType3 = Value(0x32)
  val UserDefined8BitDataType4 = Value(0x33)
  val UserDefined8BitDataType5 = Value(0x34)
  val UserDefined8BitDataType6 = Value(0x35)
  val UserDefined8BitDataType7 = Value(0x36)
  val UserDefined8BitDataType8 = Value(0x37)

  def bit_width(dt : MIPIDataTypes) : Int = {
    dt match {
      case UNKNOWN => 0
      case FrameStartCode => 0
      case FrameEndCode => 0
      case Embedded8BitNonImageData => 8
      case YUV4208Bit => 8
      case YUV42010Bit => 10
      case LegacyYUV4208Bit => 8
      case Reserved => ???
      case YUV4208BitChromaShifted => 8
      case YUV42010BitChromaShifted => 10
      case YUV4228Bit => 8
      case YUV42210Bit => 10
      case RGB444 => 12
      case RGB555 => 15
      case RGB565 => 16
      case RGB666 => 18
      case RGB888 => 24
      case RAW6 => 6
      case RAW7 => 7
      case RAW8 => 8
      case RAW10 => 10
      case RAW12 => 12
      case RAW14 => 14
      case UserDefined8BitDataType1 => ???
      case UserDefined8BitDataType2 => ???
      case UserDefined8BitDataType3 => ???
      case UserDefined8BitDataType4 => ???
      case UserDefined8BitDataType5 => ???
      case UserDefined8BitDataType6 => ???
      case UserDefined8BitDataType7 => ???
      case UserDefined8BitDataType8 => ???
    }
  }
}

case class MIPIConfig(NUM_RX_LANES: Int = 2, RX_GEAR: Int = 8, OUTPUT_LANES: Int = 1, ref_dt : MIPIDataTypes) {
  def GEARED_LANES = NUM_RX_LANES * RX_GEAR
  def DT_WIDTH = OUTPUT_LANES * MIPIDataTypes.bit_width(ref_dt)
}