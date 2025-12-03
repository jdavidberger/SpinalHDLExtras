package spinalextras.lib.blackbox.lattice.lifcl

import spinal.core._
import spinalextras.lib.memory.{HardwareMemory, MemoryRequirement}

case class LscRamDpTrue(
                         wordCount: BigInt,
                         wrAddressWidth: Int,
                         wrDataWidth: Int,
                         rdAddressWidth: Int,
                         rdDataWidth: Int,
                         wrMaskEnable: Boolean,   // "true"/"false" or "1"/"0" depending on FPGA expects
                         wrMaskWidth: Int
                       ) extends BlackBox {

  // Set the RTL name
  setBlackBoxName("lscc_ram_dp_true")

  // Map Verilog generics
  addGeneric("FAMILY", "LIFCL")
  addGeneric("ADDR_DEPTH_A", wordCount)
  addGeneric("ADDR_WIDTH_A", wrAddressWidth)
  addGeneric("DATA_WIDTH_A", wrDataWidth)
  addGeneric("ADDR_DEPTH_B", wordCount)
  addGeneric("ADDR_WIDTH_B", rdAddressWidth)
  addGeneric("DATA_WIDTH_B", rdDataWidth)
  addGeneric("GSR", "enable")
  addGeneric("MODULE_TYPE", "ram_dp_true")
  addGeneric("BYTE_ENABLE_A", wrMaskEnable)
  addGeneric("BYTE_SIZE_A", wrMaskWidth)
  addGeneric("BYTE_EN_POL_A", "active-high")
  addGeneric("WRITE_MODE_A", "normal")
  addGeneric("BYTE_ENABLE_B", wrMaskEnable)
  addGeneric("BYTE_SIZE_B", wrMaskWidth)
  addGeneric("MEM_ID", "MEM0")

  // Define I/O bundle
  val io = new Bundle {
    val addr_a_i      = in  UInt(wrAddressWidth bits)
    val addr_b_i      = in  UInt(rdAddressWidth bits)
    val wr_data_a_i   = in  Bits(wrDataWidth bits)
    val wr_data_b_i   = in  Bits(rdDataWidth bits)
    val clk_a_i       = in  Bool()
    val clk_b_i       = in  Bool()
    val clk_en_a_i    = in  Bool()
    val clk_en_b_i    = in  Bool()
    val wr_en_a_i     = in  Bool()
    val wr_en_b_i     = in  Bool()
    val rst_a_i       = in  Bool()
    val rst_b_i       = in  Bool()
    val ben_a_i       = in  Bits(wrMaskWidth bits)
    val ben_b_i       = in  Bits(wrMaskWidth bits)
    val rd_data_a_o   = out Bits(wrDataWidth bits)
    val rd_data_b_o   = out Bits(rdDataWidth bits)
    val ecc_one_err_a_o = out Bool()
    val ecc_two_err_a_o = out Bool()
    val ecc_one_err_b_o = out Bool()
    val ecc_two_err_b_o = out Bool()
  }

  // Expose ports directly
  noIoPrefix()
}


class LscRamDpTrue_Mem[T <: Data](_requirements : MemoryRequirement[T]) extends HardwareMemory[T]() {
  override def requirements: MemoryRequirement[T] = _requirements

  override lazy val latency : Int = 1

  assert(requirements.initialContent == null || requirements.initialContent.isEmpty)

  val mem = new LscRamDpTrue(
    wordCount = requirements.num_elements, wrAddressWidth = log2Up(requirements.num_elements), wrDataWidth = requirements.dataType.getBitsWidth,
    rdAddressWidth = log2Up(requirements.num_elements), rdDataWidth = requirements.dataType.getBitsWidth, wrMaskEnable = requirements.needsMask, wrMaskWidth = requirements.dataType.getBitsWidth / 8
  )

  val mem_port_a = (mem.io.wr_data_a_i, mem.io.addr_a_i, mem.io.wr_en_a_i, mem.io.clk_a_i, mem.io.ben_a_i, mem.io.rd_data_a_o)
  val mem_port_b = (mem.io.wr_data_b_i, mem.io.addr_b_i, mem.io.wr_en_b_i, mem.io.clk_b_i, mem.io.ben_b_i, mem.io.rd_data_b_o)
  for(port_maps <- io.readWritePorts.zip(Seq(mem_port_a, mem_port_b))) {
    val (port, (di, adr, we, cs, benb, dout)) = port_maps
    di := port.cmd.data
    adr := port.cmd.address
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