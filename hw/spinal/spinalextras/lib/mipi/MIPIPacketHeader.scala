package spinalextras.lib.mipi

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class MIPIPacketHeader(cfg : MIPIConfig) extends Bundle with IMasterSlave {
  val datatype = UInt(6 bits)
  val word_count = UInt(16 bits)
  val virtual_channel = UInt((2 bits))

  val ecc = Bits(6 bits)
  val checksum = Bits(16 bits)

  val virtual_channel_ext = UInt(2 bits)
  val is_long_packet = Bool()
  val is_long_av_packet = Bool()

  def is_short_packet = !is_long_packet

  override def asMaster(): Unit = {
    out(datatype, word_count, virtual_channel,
      ecc, checksum,
      virtual_channel_ext,
      is_long_av_packet, is_long_packet)
  }
}
