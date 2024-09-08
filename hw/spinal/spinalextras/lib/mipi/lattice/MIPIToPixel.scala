package spinalextras.lib.mipi.lattice

import spinal.core._
import spinal.lib._
import spinalextras.lib.mipi._
import spinalextras.lib.blackbox.lattice.lifcl._

import scala.language.postfixOps

case class MIPIToPixel(cfg : MIPIConfig,
                  sync_cd : ClockDomain,
                  byte_cd : ClockDomain,
                  pixel_cd : ClockDomain
                 ) extends Component {
  val io = new Bundle {
    val mipi = slave(MIPIIO(cfg.NUM_RX_LANES))
    val pll_lock = in(Bool())

    val tx_rdy_i = in(Bool()) default(True)
    val ref_dt = in(UInt(6 bits))

    val pixelFlow = master(PixelFlow(cfg.DT_WIDTH))

    val hs_sync = out(Bool())
    //val hs_d_en = out(Bool())

    //val bytes = master(Flow(MIPIPacket(cfg)))
  }
  noIoPrefix()
  val mipi_to_bytes = new dphy_rx(cfg, sync_cd = sync_cd, byte_cd = byte_cd)
  mipi_to_bytes.assignMIPI(io.mipi)

  mipi_to_bytes.io.pll_lock_i := io.pll_lock
  mipi_to_bytes.io.tx_rdy_i := io.tx_rdy_i
  mipi_to_bytes.io.ref_dt_i := io.ref_dt.asBits

  io.hs_sync := mipi_to_bytes.io.hs_sync_o

  //io.bytes << mipi_to_bytes.MIPIPacket

  mipi_to_bytes.io.rxcsr_dropnull_i := False
  mipi_to_bytes.io.rxcsr_vcx_on_i := False

  val bytes_to_pixels = new byte2pixel(cfg, byte_cd = byte_cd, pixel_cd = pixel_cd)
  bytes_to_pixels.assignMIPIBytes(mipi_to_bytes.MIPIPacket)
  io.pixelFlow <> bytes_to_pixels.pixelflow
}