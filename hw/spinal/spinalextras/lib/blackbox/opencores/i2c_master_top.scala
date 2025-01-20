package spinalextras.lib.blackbox.opencores

import spinal.core._
import spinal.lib._
import spinal.lib.bus.wishbone.{AddressGranularity, Wishbone, WishboneConfig}
import spinalextras.lib.logging.{GlobalLogger, SignalLogger}

case class i2c_master_top() extends BlackBox {
  addGeneric("ARST_LVL", 1)

  val io = new Bundle {
    val wb_clk_i = in Bool()
    val wb_rst_i = (in Bool()).default(False)
    val arst_i = (in Bool()).default(False)

    val wb_inta_o = out Bool()

    val wb = slave(Wishbone(WishboneConfig(3, 8, addressGranularity = AddressGranularity.WORD)))
    wb.ADR.setPartialName("adr_i")
    wb.DAT_MOSI.setPartialName("dat_i")
    wb.DAT_MISO.setPartialName("dat_o")
    wb.WE.setPartialName("we_i")
    wb.STB.setPartialName("stb_i")
    wb.CYC.setPartialName("cyc_i")
    wb.ACK.setPartialName("ack_o")


    // i2c lines
    val sda_pad_i, scl_pad_i = in(Bool())
    val sda_pad_o, scl_pad_o, sda_padoen_o, scl_padoen_o = out(Bool())
  }

  setDefinitionName("i2c_master_top")
  noIoPrefix()
  mapCurrentClockDomain(io.wb_clk_i, io.wb_rst_i)

  def log_signals(): Unit = {
    GlobalLogger(
      SignalLogger.concat("i2c",
        io.scl_pad_i, io.scl_pad_o, io.scl_padoen_o,
        io.sda_pad_i, io.sda_pad_o, io.sda_padoen_o
      )
    )
  }

  def attachi2c(i2c0_scl : Bool, i2c0_sda : Bool): Unit = {
    io.scl_pad_i := i2c0_scl
    when(~io.scl_padoen_o) {
      i2c0_scl := io.scl_pad_o
    }
    io.sda_pad_i := i2c0_sda
    when(~io.sda_padoen_o) {
      i2c0_sda := io.sda_pad_o
    }
  }
}