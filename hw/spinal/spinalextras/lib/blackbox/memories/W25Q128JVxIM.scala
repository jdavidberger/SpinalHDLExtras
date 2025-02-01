package spinalextras.lib.blackbox.memories

import spinal.core._

import scala.language.postfixOps

case class W25Q128JVxIM(filename: String = "firmware.hex", offset: BigInt = 0x200000) extends BlackBox {
  addGeneric("FIRMWARE_FILENAME", filename)
  addGeneric("FIRMWARE_OFFSET", offset)

  val io = new Bundle {
    val CSn = in Bool()
    val CLK = in Bool()
    val DIO = inout(Analog(Bool()))
    val DO = inout(Analog(Bool()))
    val WPn = inout(Analog(Bool()))
    val HOLDn = inout(Analog(Bool()))
  }
  noIoPrefix()

}

case class W25Q128JVxIM_quad(filename : String = "firmware.hex", offset : BigInt = 0x200000) extends Component {
  val io = new Bundle {
    val spiflash_cs_n = in Bool()
    val spiflash_clk = in Bool()
    val spiflash_dq = inout(Analog(Bits(4 bits)))
  }
  val flash = W25Q128JVxIM(filename, offset)

  flash.io.CSn := io.spiflash_cs_n
  flash.io.CLK := io.spiflash_clk

  flash.io.DIO <> io.spiflash_dq(0)
  flash.io.DO <> io.spiflash_dq(1)
  flash.io.WPn <> io.spiflash_dq(2)
  flash.io.HOLDn <> io.spiflash_dq(3)
}

