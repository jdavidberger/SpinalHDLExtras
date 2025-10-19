package spinalextras.lib.bus

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim.{SimBaseTypePimper, SimBoolPimper, SimClockDomainHandlePimper, SimEquivBitVectorBigIntPimper, SimTimeout}
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.wishbone._
import spinal.lib.fsm.{EntryPoint, State, StateMachine}
import spinalextras.lib.Config
import spinalextras.lib.formal.fillins.Axi4Formal.Axi4FormalExt
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties, FormalProperty}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

import scala.language.postfixOps

object data_state extends SpinalEnum {
  val S_IDLE, S_WAIT_RD, S_WAIT_BSEND = newElement()
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

  val valid = io.wb.CYC && io.wb.STB
  val (readValid, writeValid) = (valid && !io.wb.WE, valid && io.wb.WE)

  val addr_sent = RegInit(False) clearWhen(io.axi.r.fire || io.axi.b.fire) setWhen(io.axi.ar.fire || io.axi.aw.fire)
  val w_sent = RegInit(False) setWhen(io.axi.w.fire) clearWhen(io.axi.b.fire)

  val read_addr_sent = addr_sent && readValid
  val write_addr_sent = addr_sent && writeValid

  io.axi.w.valid := writeValid && !w_sent
  io.axi.b.ready := True
  io.axi.r.ready := addr_sent && !io.wb.WE

  io.axi.aw.valid := !addr_sent && writeValid
  io.axi.ar.valid := !addr_sent && readValid

  io.wb.ACK := io.axi.r.fire || io.axi.b.fire

  io.axi.aw.addr := io.wb.byteAddress().resized
  io.axi.w.data  := Mux(!odd_addr, io.wb.DAT_MOSI.resized, io.wb.DAT_MOSI << 32)
  io.axi.w.last := True

  if (axiConfig.useStrb) {
    io.axi.w.strb  := Mux(!odd_addr, io.wb.SEL.resized, io.wb.SEL << 4)
  }

  io.axi.ar.addr := io.wb.byteAddress().resized
  io.wb.DAT_MISO     := Mux(odd_addr, io.axi.r.data(63 downto 32), io.axi.r.data(31 downto 0))

  override def covers(): Seq[FormalProperty] = new FormalProperties(this) {
    addFormalProperty(io.wb.ACK && io.wb.WE, "Demonstrate write")
    addFormalProperty(io.axi.ar.fire, "Demonstrate axi addr read")
    addFormalProperty(io.axi.r.fire, "Demonstrate axi read")
    addFormalProperty(io.axi.w.fire, "Demonstrate axi write")
    addFormalProperty(io.axi.w.isStall, "Demonstrate axi write stall")
    addFormalProperty(io.wb.ACK && !io.wb.WE, "Demonstrate wb read")
  }

  override def formalComponentProperties(): Seq[FormalProperty] = new FormalProperties(this) {
    addFormalProperty(io.axi.readContract.outstandingReads === (read_addr_sent.asUInt ), "Outstanding reads mismatch")

    // We can characterize what we expect from the write contract based on the joint state of addr sent / w_sent
    val writeState = write_addr_sent ## w_sent
    addFormalProperty(io.axi.writeContract.enforcer.wCompleteFifo.io.occupancy === (writeState === 1).asUInt, "Completed fifo mismatch")
    addFormalProperty(io.axi.writeContract.outstandingWriteResponses === (writeState === 3).asUInt, "Outstanding write response mismatch")
    addFormalProperty(io.axi.writeContract.outstandingWrites === (writeState === 2).asUInt, "Outstanding writes mismatch")

    addFormalProperty(io.axi.writeContract.enforcer.awLengthFifo.io.occupancy === 0)

    when(!writeValid) {
      addFormalProperty(!w_sent, "W sent can't be true")
    }

    addFormalProperty(io.axi.writeContract.pendingWrites === 0, "No pending writes permitted")
    //addFormalProperty(io.axi.writeContract.outstandingWrites === (io.wb.WE ? (addr_sent ^ bpending) | False).asUInt, "Outstanding write mismatch")
    //
    //addFormalProperty(io.axi.writeContract.pendingBursts === RegNext(valid && io.wb.WE && addr_sent === False, False).asUInt, "Track pending bursts")

//    when(n_state === data_state.S_WAIT_BSEND) {
//      addFormalProperty(valid && io.wb.WE)
//    }
    when(addr_sent) {
      addFormalProperty(valid, "If address is sent, we have a pending transaction and valid should be true")
    }
  }
}


class Wb2AxiTester extends AnyFunSuite {
  def doTest(wbConfig : WishboneConfig): Unit = {
    Config.sim.doSim(
      new Component {
        val io = new Bundle {
          val wb = slave(Wishbone(wbConfig))
        }

        val ram = new Axi4SharedOnChipRam(64, 2048, 1, false)
        ram.ram.initialContent = (0 until 2048).map(BigInt(_)).toArray
        val wb2axi = new Wb2Axi4(wbConfig, ram.axiConfig)
        io.wb <> wb2axi.io.wb

        ram.io.axi <> wb2axi.io.axi.toShared()
      }
    ) { dut =>
      SimTimeout(5000 us)
      dut.clockDomain.forkStimulus(100 MHz)

      dut.io.wb.SEL #= 0xf
      dut.io.wb.CYC #= false
      dut.io.wb.STB #= false

      dut.clockDomain.waitSampling(10)

      for(i <- 0 until 256) {
        dut.io.wb.CYC #= true
        dut.io.wb.STB #= true
        dut.io.wb.ADR #= i * 4
        dut.io.wb.DAT_MOSI #= i
        dut.io.wb.WE #= true
        dut.clockDomain.waitSamplingWhere(dut.io.wb.ACK.toBoolean)
        dut.io.wb.CYC #= true
        dut.io.wb.STB #= false
      }

      dut.clockDomain.waitSampling(10)

      for(i <- 0 until 256) {
        dut.io.wb.CYC #= true
        dut.io.wb.STB #= true
        dut.io.wb.ADR #= i * 4
        dut.io.wb.WE #= false
        dut.clockDomain.waitSamplingWhere(dut.io.wb.ACK.toBoolean)
        println(i, dut.io.wb.DAT_MISO.toBigInt)
        dut.io.wb.STB #= false
        dut.io.wb.ADR #= 0
        dut.clockDomain.waitSampling(3)
      }

    }
  }


  test("ByteTest") {
    doTest(WishboneConfig(32, 32, addressGranularity = AddressGranularity.BYTE, selWidth = 4))
  }
  test("WordTest") {
    doTest(WishboneConfig(32, 32, addressGranularity = AddressGranularity.WORD, selWidth = 4))
  }

}

class Wb2Axi4FormalTester extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 10

  formalTests().foreach(t => test(t._1) {
    t._2()
  })

  override def generateRtl() = {
    Seq(
      (s"Basic", () =>
        GeneralFormalDut(() => new Wb2Axi4(WishboneConfig(32, 32,
          addressGranularity = AddressGranularity.BYTE,
          selWidth = 4), Axi4Config(32, 64, idWidth = 1, useRegion = false))))
    )
  }
}






