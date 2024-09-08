package spinalextras.lib.blackbox.lattice.lifcl


import spinal.core._
import spinal.core.in.Bool
import spinal.lib._
import spinalextras.lib.mipi.{MIPIConfig, MIPIIO, MIPIPacket}

import scala.language.postfixOps


class dphy_rx(cfg : MIPIConfig,
              misc_signals : Boolean = true,
              sync_cd: ClockDomain = null,
              byte_cd: ClockDomain = null,
              var ip_name : String = null) extends BlackBox {
  if(ip_name == null) {
    val sync_f = if(sync_cd == null || sync_cd.frequency.isInstanceOf[UnknownFrequency]) "" else s"_sync${sync_cd.frequency.getValue.decomposeString.replace(" ", "")}"
    val byte_f = if(byte_cd == null || byte_cd.frequency.isInstanceOf[UnknownFrequency]) "" else s"_byte${byte_cd.frequency.getValue.decomposeString.replace(" ", "")}"
    ip_name = s"dphy_rx_${cfg.NUM_RX_LANES}x${cfg.RX_GEAR}_${cfg.ref_dt}${sync_f}${byte_f}"
  }

  val io = new Bundle {
    val rxcsr_vcx_on_i = in Bool()
    val rxcsr_dropnull_i = in Bool()
    val pll_lock_i = in Bool()
    val sync_clk_i = in Bool()
    val sync_rst_i = in Bool()
    val ready_o = out Bool()
    val clk_byte_o = out Bool()
    val clk_byte_hs_o = out Bool()
    val clk_byte_fr_i = in Bool()
    val reset_n_i = in Bool()
    val reset_byte_fr_n_i = in Bool()
    val clk_p_io = inout(Analog(Bool()))
    val clk_n_io = inout(Analog(Bool()))
    val d_p_io = inout(Analog(Bits(cfg.NUM_RX_LANES bits)))
    val d_n_io = inout(Analog(Bits(cfg.NUM_RX_LANES bits)))
    val lp_d_rx_p_o = out(Analog(Bits(cfg.NUM_RX_LANES bits)))
    val lp_d_rx_n_o = out(Analog(Bits(cfg.NUM_RX_LANES bits)))
    val bd_o = out(Bits(cfg.GEARED_LANES bits))
    val hs_d_en_o = if(misc_signals) out Bool() else null
    val hs_sync_o = out Bool()
    val payload_en_o = out Bool()
    val payload_o = out(Bits(cfg.GEARED_LANES bits))
    val dt_o = out(UInt(6 bits))
    val vc_o = out(UInt(2 bits))
    val vcx_o = out(UInt(2 bits))
    val wc_o = out(UInt(16 bits))
    val ecc_o = out(Bits(6 bits))
    val payload_bytevld_o = if(misc_signals) out(Bits(8 bits)) else null
    val payload_crc_o = out(Bits(16 bits))
    val payload_crcvld_o = out Bool()
    val ecc_check_o = out Bool()
    val ecc_byte_error_o = out Bool()
    val ecc_1bit_error_o = out Bool()
    val ecc_2bit_error_o = out Bool()
    val dphy_rxdatawidth_hs = out(Bits(cfg.NUM_RX_LANES bits))
    val dphy_cfg_num_lanes = out(Bits(2 bits))
    val ref_dt_i = in(Bits(6 bits)) default(cfg.ref_dt.id)
    val tx_rdy_i = in Bool()
    val sp_en_o = out Bool()
    val lp_en_o = out Bool()
    val lp_av_en_o = out Bool()
  }

  def assignMIPI(mipi : MIPIIO) = {
    io.clk_p_io := mipi.clk_p
    io.clk_n_io := mipi.clk_n
    io.d_p_io := mipi.data_p
    io.d_n_io := mipi.data_n
  }

  def MIPIPacket = {
    val bytes = new Flow(new MIPIPacket(cfg))
    bytes.virtual_channel_ext := io.vcx_o
    bytes.ecc := io.ecc_o
    bytes.checksum := io.payload_crc_o
    bytes.datatype := io.dt_o
    bytes.virtual_channel := io.vc_o
    bytes.word_count := io.wc_o
    bytes.valid := io.lp_en_o || io.sp_en_o
    bytes.is_long_packet := io.lp_en_o
    bytes.is_long_av_packet := io.lp_av_en_o
    bytes.payload.payload := io.payload_o
    bytes
  }

  def attachClockDomains(sync_cd: ClockDomain, byte_cd: ClockDomain): Unit = {
    if(sync_cd != null) {
      io.reset_n_i := ~sync_cd.readResetWire
      io.sync_clk_i := sync_cd.readClockWire
      io.sync_rst_i := sync_cd.readResetWire
    }

    if(byte_cd != null) {
      io.reset_byte_fr_n_i := ~byte_cd.readResetWire
      io.clk_byte_fr_i := byte_cd.readClockWire
    }
  }

  noIoPrefix()
  setDefinitionName(ip_name)

  Component.push(parent)
  attachClockDomains(sync_cd, byte_cd)

}