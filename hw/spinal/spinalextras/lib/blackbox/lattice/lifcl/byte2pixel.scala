package spinalextras.lib.blackbox.lattice.lifcl


import spinal.core._
import spinal.lib.Flow
import spinalextras.lib.mipi.{MIPIConfig, MIPIPacket, PixelFlow}

class byte2pixel(cfg : MIPIConfig, byte_cd: ClockDomain = null, pixel_cd: ClockDomain = null, var ip_name : String = null) extends BlackBox {
  if(ip_name == null) {
    val byte_f = if(byte_cd == null || byte_cd.frequency.isInstanceOf[UnknownFrequency]) "" else s"_byte${byte_cd.frequency.getValue.decomposeString.replace(" ", "")}"
    val pixel_f = if(pixel_cd == null || pixel_cd.frequency.isInstanceOf[UnknownFrequency]) "" else s"_pixel${pixel_cd.frequency.getValue.decomposeString.replace(" ", "")}"
    ip_name = s"byte2pixel_${cfg.NUM_RX_LANES}x${cfg.RX_GEAR}_${cfg.ref_dt}_p${cfg.DT_WIDTH}${byte_f}${pixel_f}"
  }

  val io = new Bundle {
    val reset_byte_n_i = in Bool()
    val clk_byte_i = in Bool()
    val sp_en_i = in Bool()
    val dt_i = in UInt(6 bits)
    val lp_av_en_i = in Bool()
    val payload_en_i = in Bool()
    val payload_i = in UInt(cfg.GEARED_LANES bits)
    val wc_i = in UInt(16 bits)
    val reset_pixel_n_i = in Bool()
    val clk_pixel_i = in Bool()
    val fv_o = out Bool()
    val lv_o = out Bool()
    val pd_o = out UInt(cfg.DT_WIDTH bits)
    val p_odd_o = out UInt(2 bits)
    val pixcnt_c_o = out UInt(19 bits)
    val pix_out_cntr_o = out UInt(16 bits)
    val wc_pix_sync_o = out UInt(16 bits)
  }

  def assignMIPIBytes(bytes : Flow[MIPIPacket]) = {
    io.sp_en_i := bytes.valid && bytes.is_short_packet
    io.dt_i := bytes.datatype
    io.lp_av_en_i := bytes.valid && bytes.is_long_av_packet
    io.payload_en_i := bytes.valid
    io.payload_i := bytes.payload.payload.asUInt
    io.wc_i := bytes.word_count
  }

  def pixelflow = {
    val pixel_flow = PixelFlow(cfg.DT_WIDTH)
    pixel_flow.frame_valid := io.fv_o
    pixel_flow.line_valid := io.lv_o
    pixel_flow.payload := io.pd_o.asBits
    pixel_flow
  }

  noIoPrefix()

  def attachClockDomains(byte_cd: ClockDomain, pixel_cd: ClockDomain)= {
    if(byte_cd != null) {
      io.clk_byte_i := byte_cd.readClockWire
    }

    if(pixel_cd != null) {
      io.clk_pixel_i := pixel_cd.readClockWire
      io.reset_pixel_n_i := ~pixel_cd.readResetWire
      io.reset_byte_n_i := ~pixel_cd.readResetWire
    }
  }

  setDefinitionName(ip_name)

  Component.push(parent)
  attachClockDomains(byte_cd, pixel_cd)
}