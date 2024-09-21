package spinalextras.lib.mipi.lattice

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.bus.regif.BusIf
import spinalextras.lib.mipi._
import spinalextras.lib.blackbox.lattice.lifcl._
import spinalextras.lib.logging.{GlobalLogger, SignalLogger}

import scala.language.postfixOps

case class MIPIToPixel(cfg : MIPIConfig,
                       sync_cd : ClockDomain,
                       pixel_cd : ClockDomain,
                       byte_cd : ClockDomain = null,
                       byte_freq: HertzNumber = null,
                       sensor_name : String = "",
                       clock_suffix : Boolean = true
                 ) extends Component {
  val io = new Bundle {
    val mipi = slave(MIPIIO(cfg.NUM_RX_LANES))
    val pll_lock = in(Bool())

    val tx_rdy = in(Bool()) default(True)
    val ref_dt = in(UInt(6 bits))

    val pixelFlow = master(PixelFlow(cfg.DT_WIDTH))
  }

  if(sensor_name != "") {
    io.mipi.setPartialName(s"${sensor_name}_mipi")
    io.pixelFlow.setPartialName(s"${sensor_name}_pixelFlow")
  }

  noIoPrefix()
  val mipi_to_bytes = new dphy_rx(cfg, sync_cd = sync_cd, byte_cd = byte_cd, byte_freq = byte_freq, clock_suffix = clock_suffix)
  mipi_to_bytes.assignMIPI(io.mipi)

  mipi_to_bytes.io.pll_lock_i := io.pll_lock
  mipi_to_bytes.io.tx_rdy_i := io.tx_rdy
  mipi_to_bytes.io.packet_parser.ref_dt_i := io.ref_dt.asBits

  mipi_to_bytes.io.rxcsr_dropnull_i := False
  mipi_to_bytes.io.rxcsr_vcx_on_i := False

  val bytes_to_pixels = new byte2pixel(cfg, pixel_cd = pixel_cd, byte_cd = mipi_to_bytes.byte_cd(), clock_suffix = false)
  bytes_to_pixels.assignMIPIHeader(mipi_to_bytes.MIPIPacketHeader)
  bytes_to_pixels.assignMIPIBytes(mipi_to_bytes.MIPIBytes)
  io.pixelFlow <> bytes_to_pixels.io.pixelFlow

  def byte_clock_domain() : ClockDomain = {
    mipi_to_bytes.byte_cd()
  }

  def attach_bus(busSlaveFactory: BusIf): Unit = {
    mipi_to_bytes.attach_bus(busSlaveFactory)
    bytes_to_pixels.attach_bus(busSlaveFactory)
  }
}