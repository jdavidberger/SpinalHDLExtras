package spinalextras.lib.blackbox.lattice.lifcl

import spinal.lib._
import spinal.core._
import spinalextras.lib.{HardwareMemory, MemoryRequirement}

import scala.language.postfixOps

class PDPSC512K(
                 OUTREG : Boolean = false,
                 GSR : Boolean = true,
                 ASYNC_RESET : Boolean = false,
                 ASYNC_RESET_RELEASE : Boolean = false,
                 ENABLE_ECC : Boolean = false, //Enable ECC or Byte-enable support
                 initialContent : Seq[BigInt] = Seq()
               ) extends BlackBox {

  addGeneric("OUTREG", if(OUTREG) "OUT_REG" else "NO_REG")
  addGeneric("GSR", if(GSR) "ENABLED" else "DISABLED")
  addGeneric("RESETMODE", if(ASYNC_RESET) "ASYNC" else "SYNC")
  addGeneric("ASYNC_RESET_RELEASE", if(ASYNC_RESET_RELEASE) "ASYNC" else "SYNC")
  addGeneric("ECC_BYTE_SEL", if(ENABLE_ECC) "ECC_EN" else "BYTE_EN")

  require(initialContent.size <= 0x7F)
  initialContent.zipWithIndex.foreach { case (d, idx) => {
    addGeneric(f"INITVAL_$idx%02X", f"0x$d%X")
  }}

  val io = new Bundle {
    val CLK = in Bool()

    val DI = in Bits(32 bits)
    val ADW = in Bits(14 bits)
    val ADR = in Bits(14 bits)
    val CEW = in Bool() default(True)
    val CER = in Bool() default(True)
    val WE = in Bool()
    val CSW = in Bool() default(True)
    val CSR = in Bool() default(True)
    val RSTR = in Bool()
    val BYTEEN_N = !ENABLE_ECC generate (in Bits(4 bits))

    val DO = out Bits(32 bits)
    val ERRDECA = ENABLE_ECC generate (out Bits(2 bits))
    val ERRDECB = ENABLE_ECC generate (out Bits(2 bits))
  }
  noIoPrefix()

  // Map the generic clock
  mapCurrentClockDomain(clock=io.CLK, reset=io.RSTR)
}

class PDPSC512K_Mem(target_latency : Int = 2, initialContent : Seq[BigInt] = Seq()) extends HardwareMemory[Bits]() {
  override val requirements = MemoryRequirement(
    Bits(32 bits), (1 << 14), 0, 1, 1
  )
  override lazy val latency : Int = target_latency
  assert(latency == 2 || latency == 1)
  val outreg = latency == 2
  val mem = new PDPSC512K(OUTREG = outreg, initialContent = initialContent)

  val read = io.readPorts.head
  mem.io.ADR := read.cmd.payload.asBits
  read.rsp.data := mem.io.DO

  override lazy val actual_num_elements = (512 KiB) / 32

  if(latency == 2) {
    read.rsp.valid := RegNext(RegNext(read.cmd.valid, False), False)
  } else {
    read.rsp.valid := RegNext(read.cmd.valid, False)
  }

  val write = io.writePorts.head
  mem.io.ADW := write.cmd.payload.address.asBits
  mem.io.DI := write.cmd.data
  mem.io.WE := write.cmd.valid
  mem.io.BYTEEN_N := ~write.cmd.mask
}