package spinalextras.lib.blackbox.lattice.lifcl


import spinal.core._
import spinal.core.in.Bool
import spinal.lib._
import spinalextras.lib.logging.{GlobalLogger, SignalLogger}
import spinalextras.lib.mipi.{MIPIConfig, MIPIIO, MIPIPacketHeader}

import scala.language.postfixOps


class dphy_rx(cfg : MIPIConfig,
              enable_packet_parser : Boolean = true,
              enable_misc_signals : Boolean = true,
              enable_fifo_misc_signals : Boolean = true,
              enable_logging : Boolean = true,
              sync_cd: ClockDomain = null,
              byte_cd: ClockDomain = null,
              clock_suffix : Boolean = false,
              var ip_name : String = null) extends BlackBox {
  if(ip_name == null) {
    val sync_f = if(sync_cd == null || sync_cd.frequency.isInstanceOf[UnknownFrequency]) "" else s"_sync${sync_cd.frequency.getValue.decomposeString.replace(" ", "")}"
    val byte_f = if(byte_cd == null || byte_cd.frequency.isInstanceOf[UnknownFrequency]) "" else s"_byte${byte_cd.frequency.getValue.decomposeString.replace(" ", "")}"
    val clock_suffix_str = if(clock_suffix) s"${sync_f}${byte_f}" else ""
    ip_name = s"dphy_rx_${cfg.NUM_RX_LANES}x${cfg.RX_GEAR}${clock_suffix_str}"
  }

  val io = new Bundle {
    /**
     * This signal is tied to 0 when it is not exposed.
     * Drive this signal when it is exposed:
     * • 1’b0 – No extended virtual channel ID; uses 24-bit
     * Hamming code.
     * • 1’b1 – Packet header ECC byte[7:6] is used as
     * extended virtual channel ID; uses 26-bit Hamming
     * code.
     */
    val rxcsr_vcx_on_i = enable_packet_parser generate in Bool() default(False)

    /**
     * This signal is tied to 0 when it is not exposed.
     * Drive this signal when it is exposed:
     * • 1’b0 – Null and Blanking packets trigger an
     * assertion of lp_en. Payload is also transmitted
     * out. The output signal lp_av_en stays low.
     * • 1’b1 – Null and Blanking packets are ignored by
     * the IP
     */
    val rxcsr_dropnull_i = enable_packet_parser generate in Bool()
    val pll_lock_i = in Bool()
    val sync_clk_i = in Bool()
    val sync_rst_i = in Bool()
    val ready_o = out Bool()
    val clk_byte_o = out Bool()
    val clk_byte_hs_o = out Bool()

    /**
     * Low asserted reset for the nets in the clk_lp_hs_ctrl
     * clock domain. The signal driving this port must be
     * synchronized to the clk_lp_hs_ctrl.
     */
    val reset_lp_n_i = in Bool()
    /**
     * Clocks the logic that detects the Rx D-PHY clock lane
     * LP <-> HS transitions. The minimum frequency for
     * clk_lp_ctrl_i is 40 MHz, as the minimum TLPX is 50 ns
     * (1/25 ns = 40 MHz).
     */
    val clk_lp_ctrl_i = in Bool()

    /**
     * Continuously running byte clock. This is div8 (in Gear
     * 16) or div4 (in Gear 8) of the input D-PHY clock. This
     * also clocks the logic that detects the Rx D-PHY data
     * lane transitions (lp_hs_ctrl_d0-3 modules). This is used
     * by the word_align, lane_align, and capture_control
     * modules. Payload output is also in this clock domain.
     */
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

    /**
     * Indicates the successful detection of the
     * synchronization code ‘B8 in the data lanes. This signal
     * asserts from the start of synchronization pattern ‘B8 up
     * to the last data captured before detecting LP-11 state
     * of any lane (for Soft D-PHY) or data lane 0 (for Hard DPHY).
     * Default is 1’d0
     */
    val hs_sync_o = out Bool()

    val tx_rdy_i = in Bool()

    val packet_parser = enable_packet_parser generate new Bundle {
      /**
       * Asserts with lp_en_o if long packet received is the
       * same as the input reference data type ref_dt_i.
       */
      val lp_av_en_o = out Bool()

      /**
       * Signifies the arrival of long packet data. This asserts
       * when a valid long packet data type is received.
       */
      val lp_en_o = out Bool()

      val payload_en_o, sp_en_o = out Bool()

      val payload_o = out(Bits(cfg.GEARED_LANES bits))
      val dt_o = out(UInt(6 bits))
      val ecc_o = out(Bits(6 bits))
      val vc_o, vcx_o = out(UInt(2 bits))
      val wc_o = out(UInt(16 bits))
      val payload_bytevld_o = out(Bits(8 bits))
      val payload_crc_o = out(Bits(16 bits))
      val payload_crcvld_o = out Bool()

      val ecc_check_o, ecc_byte_error_o, ecc_1bit_error_o, ecc_2bit_error_o = out Bool()
      val ref_dt_i = in(Bits(6 bits)) default(cfg.ref_dt.id)
    }.setPartialName("")

    val dphy_rxdatawidth_hs = out(Bits(cfg.NUM_RX_LANES bits))
    val dphy_cfg_num_lanes = out(Bits(2 bits))

    val misc_signals = enable_misc_signals generate new Bundle {
      /**
       * Active-high enable signal for the line termination of the D-PHY
       * clock lane. This is asserted on detection of transition from
       * LP-11 to LP-01 of the clock lane, and de-asserted upon
       * detection of LP-11 after a high-speed mode.
       * Default is 1’d1 if D-PHY Clock Mode == Continuous and 1’d0 if
       * D-PHY Clock Mode == Non-Continuous.
       */
      val term_clk_en_o = out Bool()

      /**
       * Active-high enable signal for the line termination of the D-PHY
       * clock lane. This is asserted on detection of transition from
       * LP-11 to LP-01 of the lanes, and de-asserted upon detection of
       * LP-11 after a high-speed mode.
       * Default is {NUM_LANES{1’d0}}.
       */
      val term_d_en_o = out Bits(cfg.NUM_RX_LANES bits)

      /**
       * Active-high high-speed mode enable signal for data lane d0.
       * For Hard D-PHY IP, this signal is also used for HS mode enable
       * for other data lanes.
       * Default is 1’d0.
       */
      val hs_d_en_o = out Bool()

      /**
       * Contention detection indicator on lane 0.
       */
      val cd_d0_o = out Bool()


      /**
       * Contention detection indicator on clock lane
       */

      val cd_clk_o = out Bool()
      /**
       * 2-bit state encoding of the D-PHY clock controller:
       * 2'b00 – Idle state
       * 2'b01 – LP11 state
       * 2'b10 – LP01 state
       * 2'b11 – HS state
       * Default is 2’d0
       */
      val lp_hs_state_clk_o = out(Bits(2 bits))

      /**
       * 2-bit state encoding of the D-PHY data lane 0 controller:
       * 2'b00 – Idle state
       * 2'b01 – LP11 state
       * 2'b10 – LP01 state
       * 2'b11 – HS state
       * Default is 2’d0.
       */
      val lp_hs_state_d_o = out Bits(2 bits)
    }.setPartialName("")

    val fifo_misc_signals = enable_fifo_misc_signals generate new Bundle {
      /**
       * State Machine for reading data from FIFO.
       * SINGLE Mode:
       * 2’b00 – IDLE state
       * 2’b01 – Read data from buffer instance 0
       * QUEUE Mode:
       * 2’b00 – IDLE state
       * 2’b01 – Read data from buffer instance 0
       * 2’b11 – Read data done
       * PINGPONG Mode:
       * 2’b00 – IDLE state
       * 2’b01 – Read data from buffer instance 0
       * 2’b10 – Read data from buffer instance 1
       * 2’b11 – Read data done
       * Default is 2’d0.
       */
      val rxdatsyncfr_state_o = out Bits(2 bits)

      /**
       * FIFO empty flag of instance 0/1.
       * Default is 1’d1.
       */
      val rxemptyfr0_o, rxemptyfr1_o = out Bool()

      /**
       * FIFO full of instance 0/1.
       * Default is 1’d0.
       */
      val rxfullfr0_o, rxfullfr1_o = out Bool()

      /**
       * State Machine of RX Queue:
       * 2’b00 – IDLE state
       * 2’b01 – Pop entry from queue
       * 2’b10 – Wait for read data from buffer is done
       * 2’b11 – One delay cycle before Idle
       * Default is 2’d0.
       */
      val rxque_curstate_o = out Bits(2 bits)

      /**
       * RX Queue empty flag.
       * Default is 1’d1.
       */
      val rxque_empty_o = out Bool()

      /**
       * RX Queue full flag.
       * Default is 1’d0.
       */
      val rxque_full_o = out Bool()

      /**
       * An error flag that indicates a write happened when there is still
       * an outstanding transfer in the RX FIFO. This flag is cleared
       * when a new HS transfer happens.
       * Default is 1’d0
       */
      val fifo_dly_err_o = out Bool()

      /**
       * An error flag that indicates a read happened when the FIFO is
       * empty. This happens if the TX clock is faster than RX clock and
       * there is not enough data in the FIFO. This flag is cleared when
       * a new HS transfer happens. Increase the FIFO delay setting to
       * give time for data to accumulate in the buffer.
       * Default is 1’d0.
       */
      val fifo_undflw_err_o = out Bool()

      /**
       * An error flag that indicates a write happens when the FIFO is
       * full. This happens if the TX cannot flush out the FIFO fast
       * enough. This flag is cleared when a new HS transfer happens.
       * Decrease the delay setting, increase the FIFO depth, or both.
       * Default is 1’d0.
       */
      val fifo_ovflw_err_o = out Bool()
    }.setPartialName("")
  }

  def assignMIPI(mipi : MIPIIO) = {
    io.clk_p_io := mipi.clk_p
    io.clk_n_io := mipi.clk_n
    io.d_p_io := mipi.data_p
    io.d_n_io := mipi.data_n
  }

  def MIPIPacketHeader = {
    val bytes = new Flow(new MIPIPacketHeader(cfg))
    bytes.virtual_channel_ext := io.packet_parser.vcx_o
    bytes.ecc := io.packet_parser.ecc_o
    bytes.checksum := io.packet_parser.payload_crc_o
    bytes.datatype := io.packet_parser.dt_o
    bytes.virtual_channel := io.packet_parser.vc_o
    bytes.word_count := io.packet_parser.wc_o
    bytes.valid := io.packet_parser.lp_en_o || io.packet_parser.sp_en_o
    bytes.is_long_packet := io.packet_parser.lp_en_o
    bytes.is_long_av_packet := io.packet_parser.lp_av_en_o
    bytes
  }

  def MIPIBytes = {
    val bytes = new Flow(Bits(cfg.GEARED_LANES bits))
    bytes.payload := io.packet_parser.payload_o
    bytes.valid := io.packet_parser.payload_en_o
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
      io.clk_lp_ctrl_i := byte_cd.readClockWire
      io.reset_lp_n_i := ~byte_cd.readResetWire
    }
  }

  noIoPrefix()
  setDefinitionName(ip_name)

  Component.push(parent)
  attachClockDomains(sync_cd, byte_cd)

  enable_logging generate new ClockingArea(byte_cd) {
    GlobalLogger(
      SignalLogger.concat("MIPI_misc_debug",
        io.misc_signals,
        io.fifo_misc_signals.elements.filterNot(_._1.contains("empty"))
      ),
      SignalLogger.concat("MIPI_packet_debug",
        io.packet_parser,
      )
    )
  }
}