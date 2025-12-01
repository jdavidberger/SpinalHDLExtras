package spinalextras.lib.peripherals.i2c

import spinal.core._
import spinal.lib._
import spinal.lib.bus.wishbone.{AddressGranularity, Wishbone, WishboneConfig}
import spinal.lib.io._
import spinalextras.lib.logging.{GlobalLogger, SignalLogger, WishboneBusLogger}

/***
 * This is a translation of https://opencores.org/projects/i2c into spinal. The translation is so we don't have to keep
 * the verilog component around but can work with software drivers who are used to this interface.
 *
 * It has logging / diagnostic logic available for it which the original didn't. It was loosely verified to be identical
 * even for timing up to a depth of 30 or so using the BMC formal tooling in yosys.
 */
case class I2cMaster() extends Component {
  val io = new Bundle {
    // Wishbone interface
    val wb = slave(Wishbone(
      WishboneConfig(
        addressWidth = 3,
        dataWidth = 8,
        addressGranularity = AddressGranularity.WORD
      )
    ))

    // Interrupt output
    val interrupt = out Bool()

    val scl = master (TriState(Bool()))
    val sda = master (TriState(Bool()))
  }

  // Define registers
  val prer = Reg(UInt(16 bits)) init(U(0xFFFF))
  val ctr = Reg(Bits(8 bits)) init(0)
  val txr = Reg(Bits(8 bits)) init(0)
  val rxr = Bits(8 bits)
  val cr = Reg(Bits(8 bits)) init(0)
  val sr = Bits(8 bits)

  // Control and status signals
  val done = Bool()
  val core_en = ctr(7)
  val ien = ctr(6)
  val irxack = Bool()
  val rxack = RegNext(irxack, init = False)
  val tip = RegInit(False)
  val irq_flag = RegInit(False)
  val i2c_busy = Bool()
  val i2c_al = Bool()
  val al = RegInit(False)

  // Command register decoding
  val sta = cr(7)
  val sto = cr(6)
  val rd = cr(5)
  val wr = cr(4)
  val ack = cr(3)
  val iack = cr(0)

  // Wishbone acknowledge generation
  val wb_ack = RegInit(False)
  when(io.wb.CYC && io.wb.STB && !wb_ack) {
    wb_ack := True
  }.otherwise {
    wb_ack := False
  }
  io.wb.ACK := wb_ack

  // Wishbone data output mux
  val wb_wacc = io.wb.WE && wb_ack

  val wb_dat_o = RegInit(B(0, 8 bits))
  // Read data multiplexer
  io.wb.DAT_MISO := wb_dat_o
  switch(io.wb.ADR) {
    is(0) { wb_dat_o := prer(7 downto 0).asBits }
    is(1) { wb_dat_o := prer(15 downto 8).asBits }
    is(2) { wb_dat_o := ctr.asBits }
    is(3) { wb_dat_o := rxr.asBits }
    is(4) { wb_dat_o := sr }
    is(5) { wb_dat_o := txr.asBits }
    is(6) { wb_dat_o := cr.asBits }
    is(7) { wb_dat_o := B(0, 8 bits) } // reserved
  }

  // Register write logic
  when(wb_wacc) {
    switch(io.wb.ADR) {
      is(0) { prer(7 downto 0) := io.wb.DAT_MOSI.asUInt }
      is(1) { prer(15 downto 8) := io.wb.DAT_MOSI.asUInt }
      is(2) { ctr := io.wb.DAT_MOSI }
      is(3) { txr := io.wb.DAT_MOSI }
      is(4) { when(core_en) { cr := io.wb.DAT_MOSI } }
    }
  } otherwise {
    // Command register special handling
    when(done || i2c_al) {
      cr(7 downto 4) := 0 // Clear command bits when done or arbitration lost
    }
    cr(2 downto 1) := 0 // Reserved bits
    when(iack) {
      cr(0) := False // Clear IRQ_ACK bit
    }
  }

  // Status register and interrupt handling
  tip := rd || wr

  irq_flag := (done | i2c_al | irq_flag) & ~iack; // interrupt request flag is always generated

  io.interrupt := RegNext(irq_flag && ien, False)

  // Status register assignments
  sr := B(0, 8 bits)
  sr(7) := rxack
  sr(6) := i2c_busy
  sr(5) := al
  sr(1) := tip
  sr(0) := irq_flag

  when(i2c_al) {
    al := True
  }.elsewhen(sta) {
    al := False
  }

  // I2C Byte Controller Instantiation
  val byteController = new I2cMasterByteCtrl()
  byteController.io.enable := core_en
  byteController.io.clkCnt := prer
  byteController.io.cmd.start := sta
  byteController.io.cmd.stop := sto
  byteController.io.cmd.read := rd
  byteController.io.cmd.write := wr
  byteController.io.cmd.ackIn := ack
  byteController.io.cmd.dataIn := txr
  done := byteController.io.rsp.cmdAck
  irxack := byteController.io.rsp.ackOut
  rxr := byteController.io.rsp.dataOut
  i2c_busy := byteController.io.rsp.busy
  i2c_al := byteController.io.rsp.al

  io.scl <> byteController.io.scl
  io.sda <> byteController.io.sda

  def attachi2c(i2c0_scl : Bool, i2c0_sda : Bool): Unit = {
    io.scl.read := i2c0_scl
    when(io.scl.writeEnable) {
      i2c0_scl := io.scl.write
    }
    io.sda.read := i2c0_sda
    when(io.sda.writeEnable) {
      i2c0_sda := io.sda.write
    }
  }

  withAutoPull()

  GlobalLogger(
    Set("i2c"),
    SignalLogger.concat("i2c",
      io.sda, io.scl, io.interrupt
    )
  )

  GlobalLogger(
    Set("i2c-dbg"),
    WishboneBusLogger.flows(io.wb)
  )
}
