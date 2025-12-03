package spinalextras.lib.blackbox.lattice.lifcl

import spinal.core._
import spinal.lib._
import spinalextras.lib.memory._
import spinalextras.lib.blackbox.lattice.lifcl.PDPSC16K.DataWidth

object PDPSC16K {
  object DataWidth extends Enumeration {
    val X36, X18, X9, X4, X2, X1 = Value
    
    implicit def toStr(x: Value) = x.toString
    def toBitWidth(x: Value): Int = x match {
      case X36 => 36
      case X18 => 18
      case X9 => 9
      case X4 => 4
      case X2 => 2
      case X1 => 1
    }
  }

}  

// 16Kb pseudo dual port single clock block RAM
case class PDPSC16K(
  // Write port width
  DATA_WIDTH_W: PDPSC16K.DataWidth.Value = DataWidth.X36,
  // Read port width
  DATA_WIDTH_R: PDPSC16K.DataWidth.Value = DataWidth.X36,
  // Register output
  OUTREG : Boolean = false,
  // Reset sync/async control
  RESETMODE: String = "SYNC",
  // Enable global set/reset for the output registers
  GSR : Boolean = true,
  // Config download enable. [11: Static/Dyanmic, 10:No Initalization]
  INIT_DATA: String = "STATIC",
  // Enable ECC
  ECC: Boolean = false,
  // Write chip select active setting, 0=active high 1=active low
  CSDECODE_W: String = "000",
  // Read chip select active setting, 0=active high 1=active low 
  CSDECODE_R: String = "000",
  // Reset release sync/async control
  ASYNC_RST_RELEASE : Boolean = false,
  initialContent : Seq[BigInt] = Seq()
) extends BlackBox {

  addGeneric("DATA_WIDTH_W", DATA_WIDTH_W.toString)
  addGeneric("DATA_WIDTH_R", DATA_WIDTH_R.toString)
  addGeneric("RESETMODE", RESETMODE)
  addGeneric("INIT_DATA", INIT_DATA)

  addGeneric("ECC", if(ECC) "ECC_EN" else "BYTE_N")
  addGeneric("CSDECODE_W", CSDECODE_W)
  addGeneric("CSDECODE_R", CSDECODE_R)

  addGeneric("OUTREG", if(OUTREG) "OUT_REG" else "NO_REG")
  addGeneric("GSR", if(GSR) "ENABLED" else "DISABLED")
  addGeneric("ASYNC_RST_RELEASE", if(ASYNC_RST_RELEASE) "ASYNC" else "SYNC")

  require(initialContent.size <= 0x3F)
  initialContent.zipWithIndex.foreach { case (d, idx) => {
    addGeneric(f"INITVAL_$idx%02X", f"0x$d%X")
  }}

  val io = new Bundle {
    // Data in
    val DI = in Bits(DataWidth.toBitWidth(DATA_WIDTH_W) bits)
    // Write address
    val ADW = in Bits(14 bits)
    // Read address  
    val ADR = in Bits(14 bits)
    // Clock
    val CLK = in Bool()
    // Read clock enable
    val CER = in Bool() default(True)
    // Write clock enable
    val CEW = in Bool() default(True)
    // Write chip select
    val CSW = in Bits(3 bits) default(0)
    // Read chip select
    val CSR = in Bits(3 bits) default(0)
    // Output register reset
    val RST = in Bool()
    // Data out
    val DO = out Bits(DataWidth.toBitWidth(DATA_WIDTH_R) bits)
    // ECC error flag (single bit error)
    val ONEBITERR = out Bool()
    // ECC error flag (two bit error)
    val TWOBITERR = out Bool()
  }

  mapCurrentClockDomain(io.CLK, io.RST)
  noIoPrefix()
}

class PDPSC16K_Mem(data_pins : Int = 36, target_latency : Int = 2) extends HardwareMemory[Bits]() {
  val (pins, elements) = if(data_pins == 1) {
    (DataWidth.X1, 1 << 14)
  } else if(data_pins <= 2) {
    (DataWidth.X2, 1 << 13)
  } else if(data_pins <= 4) {
    (DataWidth.X4, 1 << 12)
  } else if(data_pins <= 9) {
    (DataWidth.X9, 1 << 11)
  } else if(data_pins <= 18) {
    (DataWidth.X18, 1 << 10)
  } else {
    (DataWidth.X36, 1 << 9)
  }

  override val requirements = MemoryRequirement(
    Bits(DataWidth.toBitWidth(pins) bits), num_elements, 0, 1, 1
  )
  override lazy val latency : Int = target_latency
  assert(latency == 2 || latency == 1)
  val outreg = latency == 2

  val mem = new PDPSC16K(
    OUTREG = outreg,
    DATA_WIDTH_R = pins,
    DATA_WIDTH_W = pins,
    initialContent = requirements.initialContent
  )

  val read = io.readPorts.head
  mem.io.ADR := read.cmd.payload.asBits
  read.rsp.data := mem.io.DO

  override lazy val actual_num_elements = elements

  if(latency == 2) {
    read.rsp.valid := RegNext(RegNext(read.cmd.valid))
  } else {
    read.rsp.valid := RegNext(read.cmd.valid)
  }

  val write = io.writePorts.head
  mem.io.ADW := write.cmd.payload.address.asBits
  mem.io.DI := write.cmd.data
  mem.io.CSW.lsb := ~write.cmd.valid
}