package spinalextras.lib.bus

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.wishbone._
import spinalextras.lib.formal.fillins.Axi4Formal.Axi4FormalExt
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties, FormalProperty}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

import scala.language.postfixOps

object data_state extends SpinalEnum {
  val S_IDLE, S_WAIT_RD, S_SEND_WR, S_WAIT_BSEND = newElement()
}

object adr_state extends SpinalEnum {
  val A_IDLE, A_WAIT = newElement();
}

case class Wb2Axi4(wbConfig : WishboneConfig, axiConfig: Axi4Config, val AXI_WRITE_ID : Int = 0, val AXI_READ_ID : Int = 1) extends ComponentWithFormalProperties {
  val C_AXI_DATA_WIDTH: Int = axiConfig.dataWidth
  val C_AXI_ADDR_WIDTH: Int = axiConfig.addressWidth
  val C_AXI_ID_WIDTH: Int = axiConfig.idWidth
  val DW: Int = axiConfig.dataWidth
  val AW: Int = axiConfig.addressWidth
//  val AXI_WRITE_ID: Int = axiConfig
//  val AXI_READ_ID: Int = axiConfig.readId

  //assert(wbConfig.dataWidth == axiConfig.dataWidth, "Wishbone and AXI data width must be the same")
  //assert(wbConfig.addressWidth == axiConfig.addressWidth, "Wishbone and AXI address width must be the same")

  val io = new Bundle {
    val wb = slave(Wishbone(wbConfig))
    val axi = master(Axi4(axiConfig))
  }

//  io.wb.ACK :=  False
  if(io.wb.ERR != null)
    io.wb.ERR :=  False


  val DWSIZE = log2Up(DW) - 3
  val odd_addr = io.wb.ADR(2)

  io.axi.aw.id   := U(AXI_WRITE_ID, C_AXI_ID_WIDTH bits)
  io.axi.aw.len  := 0
  io.axi.aw.size := DWSIZE
  io.axi.aw.burst:= 0

  if(io.axi.aw.lock != null) io.axi.aw.lock := 0
  if(io.axi.aw.cache != null) io.axi.aw.cache:= 3
  if(io.axi.aw.prot != null) io.axi.aw.prot := B"3'b010"
  if(io.axi.config.useQos)
    io.axi.aw.qos  := B"4'h0"

  if(io.axi.ar.lock != null) io.axi.ar.lock := 0
  if(io.axi.config.useQos)
    io.axi.ar.qos  := B"4'h0"
  io.axi.ar.id   := U(AXI_READ_ID, C_AXI_ID_WIDTH bits)
  io.axi.ar.len  := 0
  io.axi.ar.size := DWSIZE
  io.axi.ar.burst:= B"2'b00"
  if(io.axi.ar.cache != null) io.axi.ar.cache:= B"4'h3"
  if(io.axi.ar.prot != null) io.axi.ar.prot := B"3'b010"

  io.axi.w.last := io.axi.w.valid
  val valid = io.wb.CYC && io.wb.STB

  val n_state = Reg(data_state()).init(data_state.S_IDLE)

  val addr_sent = RegInit(False) clearWhen(io.axi.r.fire || io.axi.b.fire) setWhen(io.axi.ar.fire || io.axi.aw.fire)
  val bpending = RegInit(False) setWhen(io.axi.w.fire) clearWhen(io.axi.b.fire)

  io.axi.w.valid := False
  io.axi.b.ready := bpending
  io.axi.r.ready := addr_sent && !io.wb.WE

  io.axi.aw.valid := !addr_sent && valid && io.wb.WE
  io.axi.ar.valid := !addr_sent && valid && !io.wb.WE

  // State machine that will take Wishbone cycles and convert them to AXI
  // transactions
  //We need 2 state machines, one to send address requests and another one to send read/write requests. Only when both SM's are idle,
  //its OK to kick off a new transaction.

  switch(n_state) {
    is(data_state.S_IDLE) {
      when(valid) {
        when(io.wb.WE && io.axi.w.fire) {
          n_state := data_state.S_WAIT_BSEND
        } otherwise {
          when (!io.wb.WE) {
            n_state := data_state.S_WAIT_RD
          }
        }
      }
    }
    is(data_state.S_WAIT_RD) {
      when(io.axi.r.fire) {
        n_state := data_state.S_IDLE
      }
    }
    is(data_state.S_WAIT_BSEND) {
      when(io.axi.b.fire) {
        n_state := data_state.S_IDLE
      }
    }
  }

  io.wb.ACK := io.axi.r.fire || io.axi.b.fire

  when(n_state === data_state.S_IDLE) {
    io.axi.w.valid := valid && io.wb.WE && addr_sent
  }

  io.axi.aw.addr := io.wb.byteAddress().resized
  io.axi.w.data  := Mux(!odd_addr, io.wb.DAT_MOSI.resized, io.wb.DAT_MOSI << 32)

  if (axiConfig.useStrb) {
    io.axi.w.strb  := Mux(!odd_addr, io.wb.SEL.resized, io.wb.SEL << 4)
  }

  io.axi.ar.addr := io.wb.byteAddress().resized
  io.wb.DAT_MISO     := Mux(odd_addr, io.axi.r.data(63 downto 32), io.axi.r.data(31 downto 0))

  override def covers(): Seq[FormalProperty] = new FormalProperties(this) {
    addFormalProperty(io.wb.ACK && io.wb.WE, "Demonstrate write")
    addFormalProperty(io.axi.ar.fire, "Demonstrate axi addr read")
    addFormalProperty(io.axi.r.fire, "Demonstrate axi read")
    addFormalProperty(io.wb.ACK && !io.wb.WE, "Demonstrate wb read")
  }

  override def formalComponentProperties(): Seq[FormalProperty] = new FormalProperties(this) {
    addFormalProperty(io.axi.readContract.outstandingReads === (io.wb.WE ? False |  addr_sent).asUInt)

    addFormalProperty(io.axi.writeContract.outstandingWrites === (io.wb.WE ? (addr_sent ^ bpending) | False).asUInt)
    addFormalProperty(io.axi.writeContract.outstandingWriteResponses === (io.wb.WE ? bpending | False).asUInt)
    when(bpending) {
      addFormalProperty(addr_sent && io.wb.WE)
      addFormalProperty(n_state === data_state.S_WAIT_BSEND)
    }
    when(addr_sent) {
      addFormalProperty(valid, "If address is sent, we have a pending transaction and valid should be true")
    }
  }
}

class Wb2Axi4FormalTester extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 50

  formalTests().foreach(t => test(t._1) {
    t._2()
  })

  override def generateRtl() = {
    Seq(
      (s"Basic", () =>
        GeneralFormalDut(() => new Wb2Axi4(WishboneConfig(32, 32,
          addressGranularity = AddressGranularity.BYTE,
          selWidth = 4), Axi4Config(32, 64, idWidth = 8, useRegion = false))))
    )
  }
}


class Axi4SharedOnChipRamFormalTester extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 20

  formalTests().foreach(t => test(t._1) {
    t._2()
  })
  override def generateRtl() = Seq()
  override def generateRtlBMC() = {
    Seq(
      (s"Basic", () =>
        GeneralFormalDut(() => new Axi4SharedOnChipRam(64, 8, 1)))
    )
  }
}



