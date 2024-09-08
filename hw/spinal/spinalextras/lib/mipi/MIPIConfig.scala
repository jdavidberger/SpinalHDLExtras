package spinalextras.lib.mipi

import spinalextras.lib.mipi.MIPIDataTypes.MIPIDataTypes


object MIPIDataTypes extends Enumeration {
  type MIPIDataTypes = Value
  val UNKNOWN = Value(-1)
  val FrameStartCode = Value(0x00)
  val FrameEndCode = Value(0x01)
  val Embedded8BitNonImageData = Value(0x12)
  val YUV4228Bit = Value(0x1E)
  val RGB565 = Value(0x22) //
  val RGB666 = Value(0x23)
  val RGB888 = Value(0x24)
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
}

case class MIPIConfig(NUM_RX_LANES: Int = 2, RX_GEAR: Int = 8, DT_WIDTH: Int = 10, ref_dt : MIPIDataTypes) {
  def GEARED_LANES = NUM_RX_LANES * RX_GEAR

}