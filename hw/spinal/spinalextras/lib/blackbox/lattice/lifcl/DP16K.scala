package spinalextras.lib.blackbox.lattice.lifcl

import spinal.core._
import spinalextras.lib.memory.{HardwareMemory, MemoryRequirement}

// 16Kb dual port block RAM
case class DP16K(
  // Write port width
  DATA_WIDTH_A: PDPSC16K.DataWidth.Value = PDPSC16K.DataWidth.X18,
  // Read port width  
  DATA_WIDTH_B: PDPSC16K.DataWidth.Value = PDPSC16K.DataWidth.X18,
  // Register output
  OUTREG_A: Boolean = false,
  // Register output
  OUTREG_B: Boolean = false,
  // Enable global set/reset for the output registers
  GSR: Boolean = true,
  // Config download enable. [11: Static/Dyanmic, 10:No Initalization]
  INIT_DATA: String = "STATIC",
  // Reset sync/async control
  RESETMODE_A: String = "SYNC",
  // Reset sync/async control
  RESETMODE_B: String = "SYNC",
  // Port A chip select active setting
  CSDECODE_A: String = "000",
  // Port B chip select active setting
  CSDECODE_B: String = "000",
  // Port A sync/async reset release
  ASYNC_RST_RELEASE_A: String = "SYNC",
  // Port B sync/async reset release
  ASYNC_RST_RELEASE_B: String = "SYNC"
) extends BlackBox {

  addGeneric("DATA_WIDTH_A", DATA_WIDTH_A.toString)
  addGeneric("DATA_WIDTH_B", DATA_WIDTH_B.toString)
  addGeneric("OUTREG_A", if(OUTREG_A) "USED" else "BYPASSED")
  addGeneric("OUTREG_B", if(OUTREG_B) "USED" else "BYPASSED")
  addGeneric("GSR", if(GSR) "ENABLED" else "DISABLED")
  addGeneric("INIT_DATA", INIT_DATA)
  addGeneric("RESETMODE_A", RESETMODE_A)
  addGeneric("RESETMODE_B", RESETMODE_B)
  addGeneric("CSDECODE_A", CSDECODE_A)
  addGeneric("CSDECODE_B", CSDECODE_B)
  addGeneric("ASYNC_RST_RELEASE_A", ASYNC_RST_RELEASE_A)
  addGeneric("ASYNC_RST_RELEASE_B", ASYNC_RST_RELEASE_B)

  // Add INITVAL_XX generics using a for loop
  for (i <- 0 until 0x40) {
    addGeneric(f"INITVAL_${i}%02X", "0x00000000000000000000000000000000000000000000000000000000000000000000000000000000")
  }

  val io = new Bundle {
    // Port A data in
    val DIA = in Bits(18 bits)
    // Port B data in
    val DIB = in Bits(18 bits)
    // Port A address
    val ADA = in Bits(14 bits)
    // Port B address
    val ADB = in Bits(14 bits)
    // Port A clock
    val CLKA = in Bool()
    // Port B clock
    val CLKB = in Bool()
    // Port A clock enable
    val CEA = in Bool() default(True)
    // Port B clock enable
    val CEB = in Bool() default(True)
    // Port A write enable
    val WEA = in Bool()
    // Port B write enable
    val WEB = in Bool()
    // Port A chip select
    val CSA = in Bits(3 bits) default(0)
    // Port B chip select
    val CSB = in Bits(3 bits) default(0)
    // Port A output register reset
    val RSTA = in Bool()
    // Port B output register reset
    val RSTB = in Bool()
    // Port A data out
    val DOA = out Bits(18 bits)
    // Port B data out
    val DOB = out Bits(18 bits)
  }

  noIoPrefix()
}


class DP16K_Mem(target_latency : Int = 2, read_write_ports : Int = 2, initialContent : Seq[BigInt] = Seq()) extends HardwareMemory[Bits]() {
  override val requirements = MemoryRequirement(
    Bits(32 bits), (1 << 14), read_write_ports, 0, 0
  )
  override lazy val latency : Int = target_latency
  assert(latency == 2 || latency == 1)
  assert(read_write_ports == 2 || read_write_ports == 1)

  val outreg = latency == 2
  val mem = new DP16K()

  val mem_port_a = (mem.io.DIA, mem.io.ADA, mem.io.WEA, mem.io.CEA, mem.io.CSA, mem.io.DOA)
  val mem_port_b = (mem.io.DIB, mem.io.ADB, mem.io.WEB, mem.io.CEB, mem.io.CSB, mem.io.DOB)
  for(port_maps <- io.readWritePorts.zip(Seq(mem_port_a, mem_port_b))) {
    val (port, (di, adr, we, cs, benb, dout)) = port_maps
    di := port.cmd.data
    adr := port.cmd.address.asBits
    we := port.cmd.write
    cs := port.cmd.valid
    benb := ~port.cmd.mask

    port.rsp.data := dout

    if(latency == 2) {
      port.rsp.valid := RegNext(RegNext(port.readFire, False), False)
    } else {
      port.rsp.valid := RegNext(port.readFire, False)
    }
  }
}