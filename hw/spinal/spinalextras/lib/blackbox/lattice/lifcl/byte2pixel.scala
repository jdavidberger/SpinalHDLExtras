package spinalextras.lib.blackbox.lattice.lifcl


import spinal.core._
import spinal.lib.{Flow, master, slave}
import spinalextras.lib.logging.{GlobalLogger, SignalLogger}
import spinalextras.lib.mipi
import spinalextras.lib.mipi.{MIPIConfig, MIPIPacketHeader, PixelFlow}

class byte2pixel(cfg : MIPIConfig, enable_misc_signals : Boolean = true, byte_cd: ClockDomain = null, pixel_cd: ClockDomain = null,
                 var ip_name : String = null, enable_logging : Boolean = true, clock_suffix : Boolean = false) extends BlackBox {
  if(ip_name == null) {
    val byte_f = if(byte_cd == null || byte_cd.frequency.isInstanceOf[UnknownFrequency]) "" else s"_byte${byte_cd.frequency.getValue.decomposeString.replace(" ", "")}"
    val pixel_f = if(pixel_cd == null || pixel_cd.frequency.isInstanceOf[UnknownFrequency]) "" else s"_pixel${pixel_cd.frequency.getValue.decomposeString.replace(" ", "")}"
    val clock_suffix_str = if(clock_suffix) s"${byte_f}${pixel_f}" else ""
    ip_name = s"byte2pixel_${cfg.NUM_RX_LANES}x${cfg.RX_GEAR}_${cfg.ref_dt}_p${cfg.DT_WIDTH}${clock_suffix_str}"
  }

  val io = new Bundle {
    val reset_byte_n_i = in Bool()
    val clk_byte_i = in Bool()
    val sp_en_i = in Bool()
    val dt_i = in UInt(6 bits)
    val lp_av_en_i = in Bool()

    val payload = slave Flow(Bits(cfg.GEARED_LANES bits))
    payload.valid.setName("payload_en_i")
    payload.payload.setName("payload_i")

    val wc_i = in UInt(16 bits)
    val reset_pixel_n_i = in Bool()
    val clk_pixel_i = in Bool()
    val fv_o = out Bool()
    val lv_o = out Bool()
    val pd_o = out UInt(cfg.DT_WIDTH bits)

    val pixelFlow = master(PixelFlow(cfg.DT_WIDTH))
    pixelFlow.frame_valid.setName("fv_o")
    pixelFlow.line_valid.setName("lv_o")
    pixelFlow.payload.setName("pd_o")

    val p_odd_o = out UInt(2 bits)
    val pixcnt_c_o = out UInt(19 bits)
    val pix_out_cntr_o = out UInt(16 bits)
    val wc_pix_sync_o = out UInt(16 bits)

    val debug_signals = enable_misc_signals generate new Bundle {
      val
      /**
       * Indicates FIFO empty condition
       */
      fifo_empty_o,

      /**
       * Payload data Read Enable, active high
       */
      mem_re_o,

      /**
       * Payload data Write Enable, active high
       */
      mem_we_o,

      /**
       * Indicates FIFO full condition
       */
      fifo_full_o = out Bool()

      /**
       * Pixel data read cycle
       */
      val read_cycle_o = out Bits(2 bits)

      /**
       * Payload data Write Enable, active high
       */
      val write_cycle_o = out Bits(4 bits)
    }.setPartialName("")
  }

  def assignMIPIBytes(bytes : Flow[Bits]): Unit = {
    io.payload << bytes
  }

  def assignMIPIHeader(bytes : Flow[MIPIPacketHeader]) = {
    io.sp_en_i := bytes.is_short_packet
    io.dt_i := bytes.datatype
    io.lp_av_en_i := bytes.is_long_av_packet
    io.wc_i := bytes.word_count
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
  enable_logging generate new ClockingArea(byte_cd) {
    GlobalLogger(
      SignalLogger.concat("b2p_debug",
        io.debug_signals.mem_re_o, io.debug_signals.mem_we_o,
        io.debug_signals.fifo_full_o, io.debug_signals.read_cycle_o, io.debug_signals.write_cycle_o
      )
    )
  }
}