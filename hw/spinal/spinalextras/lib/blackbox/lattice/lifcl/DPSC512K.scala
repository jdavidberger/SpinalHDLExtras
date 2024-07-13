package spinalextras.lib.blackbox.lattice.lifcl

import spinal.core._
import spinalextras.lib.{HardwareMemory, MemoryRequirement}

class DPSC512K(
                 OUTREG : Boolean = false,
                 GSR : Boolean = true,
                 ENABLE_ECC : Boolean = false //Enable ECC or Byte-enable support
               ) extends BlackBox {

  addGeneric("OUTREG_A", if(OUTREG) "OUT_REG" else "NO_REG")
  addGeneric("OUTREG_B", if(OUTREG) "OUT_REG" else "NO_REG")
  addGeneric("GSR", if(GSR) "ENABLED" else "DISABLED")

  val ASYNC_RESET = ClockDomain.current.config.resetKind == ASYNC
  addGeneric("RESETMODE", if(ASYNC_RESET) "ASYNC" else "SYNC")
  addGeneric("ASYNC_RESET_RELEASE", if(ASYNC_RESET) "ASYNC" else "SYNC")
  addGeneric("ECC_BYTE_SEL", if(ENABLE_ECC) "ECC_EN" else "BYTE_EN")

  val io = new Bundle {
    val CLK = in Bool()

    val DIA = in Bits(32 bits)
    val DIB = in Bits(32 bits)
    val ADA = in Bits(14 bits)
    val ADB = in Bits(14 bits)
    val CEA = in Bool() default(True)
    val CEB = in Bool() default(True)
    val WEA = in Bool()
    val WEB = in Bool()
    val CSA = in Bool()
    val CSB = in Bool()
    val RSTA = in Bool() default(ClockDomain.current.readResetWire)
    val RSTB = in Bool() default(ClockDomain.current.readResetWire)
    val BENA_N = in Bits(4 bits)
    val BENB_N = in Bits(4 bits)
    val CEOUTA = in Bool()
    val CEOUTB = in Bool()

    val DOA = out Bits(32 bits)
    val DOB = out Bits(32 bits)
    val ERRDECA = out Bits(2 bits)
    val ERRDECB = out Bits(2 bits)
  }
  noIoPrefix()

  // Map the generic clock
  mapCurrentClockDomain(clock=io.CLK)
}

class DPSC512K_Mem extends HardwareMemory[Bits]() {
  override val requirements = MemoryRequirement(
    Bits(32 bits), (1 << 14), 2, 0, 0
  )

  val (port_a, port_b) = (io.readWritePorts(0), io.readWritePorts(1))

  val mem = new DPSC512K(OUTREG = true)
  var latency = 2

  val mem_port_a = (mem.io.DIA, mem.io.ADA, mem.io.WEA, mem.io.CSA, mem.io.BENA_N, mem.io.DOA)
  val mem_port_b = (mem.io.DIB, mem.io.ADB, mem.io.WEB, mem.io.CSB, mem.io.BENB_N, mem.io.DOB)
  for(port_maps <- Seq(port_a, port_b).zip(Seq(mem_port_a, mem_port_b))) {
    val (port, (di, adr, we, cs, benb, dout)) = port_maps
    di := port.cmd.data
    adr := port.cmd.address.asBits
    we := port.cmd.write
    cs := port.cmd.valid
    benb := port.cmd.mask

    port.rsp.data := dout
    port.rsp.valid := RegNext(RegNext(port.cmd.valid))
  }
}