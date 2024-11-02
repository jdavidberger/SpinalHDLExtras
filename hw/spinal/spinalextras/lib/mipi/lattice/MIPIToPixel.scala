package spinalextras.lib.mipi.lattice

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.bus.regif.BusIf
import spinalextras.lib.blackbox.lattice.lifcl.dphy_rx
import spinalextras.lib.mipi._
import spinalextras.lib.logging.{GlobalLogger, SignalLogger}

import scala.language.postfixOps

case class MIPIToPixel(cfg : MIPIConfig,
                       sync_cd : ClockDomain,
                       pixel_cd : ClockDomain,
                       byte_cd : ClockDomain = null,
                       sensor_name : String = "",
                       clock_suffix : Boolean = true,
                       is_continous_clock : Option[Boolean] = None
                 ) extends Component {
  val io = new Bundle {
    val mipi = slave(MIPIIO(cfg.NUM_RX_LANES))
    val pll_lock = in(Bool())

    val tx_rdy = in(Bool()) default(True)

    val pixelFlow = master(PixelFlow(cfg.DT_WIDTH))
    def pixelFragment = PixelFlow2Fragment(pixelFlow)
  }
  val byte_freq = cfg.dphy_byte_freq

  if(sensor_name != "") {
    io.mipi.setPartialName(s"${sensor_name}_mipi")
    io.pixelFlow.setPartialName(s"${sensor_name}_pixelFlow")
  }

  noIoPrefix()
  val mipi_to_bytes = new dphy_rx(cfg, sync_cd = sync_cd, byte_cd = byte_cd, clock_suffix = clock_suffix, is_continous_clock = is_continous_clock,
  //  enable_fifo_misc_signals = Some(true)
  )

  mipi_to_bytes.assignMIPI(io.mipi)

  mipi_to_bytes.io.pll_lock_i := io.pll_lock
  mipi_to_bytes.io.tx_rdy_i := io.tx_rdy
  mipi_to_bytes.io.packet_parser.ref_dt_i := cfg.ref_dt.id

  mipi_to_bytes.io.rxcsr_dropnull_i := False
  mipi_to_bytes.io.rxcsr_vcx_on_i := False

  val bytes_to_pixels = byte2pixel(cfg, pixel_cd = pixel_cd, byte_cd = mipi_to_bytes.byte_cd())

  bytes_to_pixels.assignMIPIHeader(mipi_to_bytes.MIPIPacketHeader)
  bytes_to_pixels.assignMIPIBytes(mipi_to_bytes.MIPIBytes)
  io.pixelFlow <> bytes_to_pixels.io.pixelFlow

  def byte_clock_domain() : ClockDomain = {
    mipi_to_bytes.byte_cd()
  }

  val input_rate = cfg.RX_GEAR * cfg.NUM_RX_LANES * cfg.dphy_byte_freq.toDouble
  val sink_rate = cfg.DT_WIDTH * pixel_cd.frequency.getValue.toDouble
  require(input_rate <= sink_rate, s"Configuration doesn't work; pixel clock can't keep up with the output ${input_rate} >= ${sink_rate}")

  def attach_bus(busSlaveFactory: BusIf): Unit = {
    mipi_to_bytes.attach_bus(busSlaveFactory)
    bytes_to_pixels.attach_bus(busSlaveFactory)
  }
}