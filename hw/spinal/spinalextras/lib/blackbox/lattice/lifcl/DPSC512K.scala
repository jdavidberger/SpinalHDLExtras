package spinalextras.lib.blackbox.lattice.lifcl

import spinal.core._
import spinalextras.lib.memory.{HardwareMemory, MemoryRequirement}

class DPSC512K(
                 OUTREG : Boolean = false,
                 GSR : Boolean = true,
                 ENABLE_ECC : Boolean = false, //Enable ECC or Byte-enable support
                 initialContent : Seq[BigInt] = Seq()
               ) extends BlackBox {

  addGeneric("OUTREG_A", if(OUTREG) "OUT_REG" else "NO_REG")
  addGeneric("OUTREG_B", if(OUTREG) "OUT_REG" else "NO_REG")
  addGeneric("GSR", if(GSR) "ENABLED" else "DISABLED")

  val ASYNC_RESET = ClockDomain.current.config.resetKind == ASYNC
  addGeneric("RESETMODE", if(ASYNC_RESET) "ASYNC" else "SYNC")
  addGeneric("ASYNC_RESET_RELEASE", if(ASYNC_RESET) "ASYNC" else "SYNC")
  addGeneric("ECC_BYTE_SEL", if(ENABLE_ECC) "ECC_EN" else "BYTE_EN")


  require(initialContent.size <= 0x7F)
  initialContent.zipWithIndex.foreach { case (d, idx) => {
    addGeneric(f"INITVAL_$idx%02X", f"0x$d%X")
  }}

  val io = new Bundle {
    val CLK = in Bool()

    val DIA = in Bits(32 bits)
    val DIB = in Bits(32 bits) default(0)
    val ADA = in Bits(14 bits)
    val ADB = in Bits(14 bits) default(0)
    val CEA = in Bool() default(True)
    val CEB = in Bool() default(True)
    val WEA = in Bool()
    val WEB = in Bool() default(False)
    val CSA = in Bool() default(False)
    val CSB = in Bool() default(False)
    val RSTA = in Bool() default(ClockDomain.current.isResetActive)
    val RSTB = in Bool() default(ClockDomain.current.isResetActive)
    val BENA_N = in Bits(4 bits)
    val BENB_N = in Bits(4 bits) default(0)
    val CEOUTA = in Bool() default(True)
    val CEOUTB = in Bool() default(True)

    val DOA = out Bits(32 bits)
    val DOB = out Bits(32 bits)
    val ERRDECA = out Bits(2 bits)
    val ERRDECB = out Bits(2 bits)
  }
  noIoPrefix()

  // Map the generic clock
  mapCurrentClockDomain(clock=io.CLK)
}

class DPSC512K_Mem(target_latency : Int = 2, read_write_ports : Int = 2, initialContent : Seq[BigInt] = Seq()) extends HardwareMemory[Bits]() {
  override val requirements = MemoryRequirement(
    Bits(32 bits), (1 << 14), read_write_ports, 0, 0
  )
  override lazy val latency : Int = target_latency
  assert(latency == 2 || latency == 1)
  assert(read_write_ports == 2 || read_write_ports == 1)

  val outreg = latency == 2
  val mem = new DPSC512K(OUTREG = outreg, initialContent = initialContent)

  val mem_port_a = (mem.io.DIA, mem.io.ADA, mem.io.WEA, mem.io.CSA, mem.io.BENA_N, mem.io.DOA)
  val mem_port_b = (mem.io.DIB, mem.io.ADB, mem.io.WEB, mem.io.CSB, mem.io.BENB_N, mem.io.DOB)
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