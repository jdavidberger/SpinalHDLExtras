package spinalextras.lib.bus

import spinal.core._
import spinal.core.sim.{SimBitVectorPimper, SimBoolPimper, SimClockDomainHandlePimper, SimPublic, SimTimeout, fork, simTime}
import spinal.lib.bus.simple._
import spinal.lib.bus.wishbone._
import spinal.lib._
import spinal.lib.sim.{FlowMonitor, ScoreboardInOrder}
import spinal.lib.wishbone.sim.{WishboneDriver, WishboneMonitor, WishboneSequencer, WishboneStatus, WishboneTransaction}
import spinalextras.lib.Config

import scala.collection.mutable
import scala.language.postfixOps
import scala.util.Random

object WishbonePipelinedHelpers {
  def create_translation_signals(we : Bool, requestAccepted : Bool, ack : Bool, rspQueue : Int): (Bool, Bool, Bool) = {
    def create_queue(cnt : Int) : StreamFifoInterface[Bool] = StreamFifo(Bool(), depth = cnt, latency = 0).io

    val readyForNewReq = Bool()
    val hasOutstandingReq = Bool()
    val reqWasWE = Bool()

    if(rspQueue > 1) {
      val ackQueue = create_queue(rspQueue + 1)
      ackQueue.push.payload := we
      ackQueue.push.valid := requestAccepted

      ackQueue.pop.ready := ack

      hasOutstandingReq := ackQueue.pushOccupancy > 0
      reqWasWE := ackQueue.pop.payload
      readyForNewReq := ackQueue.push.ready
    } else {
      readyForNewReq.setAsReg() init(True) clearWhen(requestAccepted) setWhen(ack)
      hasOutstandingReq := !readyForNewReq
      reqWasWE setAsReg() init(False)
      when(requestAccepted) {
        reqWasWE := we
      }
    }

    (readyForNewReq, hasOutstandingReq, reqWasWE)
  }
}

case class WishboneToPipelinedMemoryBus(pipelinedMemoryBusConfig : PipelinedMemoryBusConfig, wbConfig: WishboneConfig, rspQueue : Int = 8, addressMap : (UInt => UInt) = identity) extends Component{
  val io = new Bundle {
    val wb = slave(Wishbone(wbConfig))
    val pmb = master(PipelinedMemoryBus(pipelinedMemoryBusConfig))
  }

  val (readyForNewReq, hasOutstandingReq, reqWasWE) =
    WishbonePipelinedHelpers.create_translation_signals(io.wb.WE, io.pmb.cmd.fire, io.wb.ACK,
      if(wbConfig.isPipelined) rspQueue else 1)

  io.pmb.cmd.payload.address := addressMap(io.wb.byteAddress()).resized
  io.pmb.cmd.payload.write := io.wb.WE
  io.pmb.cmd.payload.data := io.wb.DAT_MOSI.resized
  io.pmb.cmd.valid := io.wb.CYC && io.wb.STB && readyForNewReq

  if(io.wb.ERR != null)
    io.wb.ERR := False

  if(io.wb.SEL != null)
    io.pmb.cmd.mask := io.wb.SEL.resized
  else
    io.pmb.cmd.mask.setAll()

  if(io.wb.STALL != null) {
    io.wb.STALL := !readyForNewReq || !io.pmb.cmd.ready
  }

  io.wb.DAT_MISO := io.pmb.rsp.data.resized
  io.wb.ACK := False
  when(hasOutstandingReq) {
    io.wb.ACK := reqWasWE || io.pmb.rsp.valid
  }

  //assert(!io.wb.ACK || hasOutstandingReq, "Miscounted acks")
}

object WishboneToPipelinedMemoryBus {
  def apply(bus: Wishbone, rspQueue : Int): PipelinedMemoryBus = {
    val config = PipelinedMemoryBusConfig(addressWidth = bus.config.addressWidth, dataWidth = bus.config.dataWidth)

    if(bus.isMasterInterface ^ (bus.component == Component.current)) {
      val adapter = new WishboneToPipelinedMemoryBus(config, bus.config, rspQueue)
      adapter.io.wb <> bus
      adapter.io.pmb
    } else {
      val adapter = new PipelinedMemoryBusToWishbone(bus.config, config, rspQueue)
      adapter.io.wb <> bus
      adapter.io.pmb
    }
  }
}

case class PipelinedMemoryBusToWishbone(wbConfig: WishboneConfig, pipelinedMemoryBusConfig : PipelinedMemoryBusConfig, rspQueue : Int = 8) extends Component{
  //assert(wbConfig.dataWidth == pipelinedMemoryBusConfig.dataWidth)

  val io = new Bundle {
    val pmb = slave(PipelinedMemoryBus(pipelinedMemoryBusConfig))
    val wb = master(Wishbone(wbConfig))
  }

  //FwbSlave(io.wb)
  //FwbMaster(io.wb)

  val (readyForNewReq, hasOutstandingReq, reqWasWE) =
    WishbonePipelinedHelpers.create_translation_signals(io.wb.WE, io.pmb.cmd.fire, io.wb.ACK,
      if(wbConfig.isPipelined) rspQueue else 1)

  io.wb.assignByteAddress(io.pmb.cmd.payload.address, allowAddressResize = true)
  io.wb.WE := io.pmb.cmd.payload.write
  io.wb.CYC := io.pmb.cmd.valid || hasOutstandingReq
  io.wb.STB := io.pmb.cmd.valid && readyForNewReq
  io.wb.DAT_MOSI := io.pmb.cmd.data.resized

  if(io.wb.SEL != null)
    io.wb.SEL := io.pmb.cmd.mask

  if(io.wb.CTI != null)
    io.wb.CTI := 0

  if(io.wb.BTE != null)
    io.wb.BTE := 0

  io.pmb.cmd.ready := io.wb.isRequestAck

  //assert(!io.wb.ACK || hasOutstandingReq, "Miscounted acks")
  io.pmb.rsp.valid := !reqWasWE & io.wb.ACK
  io.pmb.rsp.payload.data := io.wb.DAT_MISO.resized
}

object PipelinedMemoryBusToWishbone {
  def apply(bus : PipelinedMemoryBus, rspQueue : Int): Wishbone = {
    apply(bus, rspQueue, WishboneConfig(addressWidth = bus.config.addressWidth, dataWidth = bus.config.dataWidth, useSTALL = rspQueue > 1, addressGranularity = AddressGranularity.WORD))
  }
  def apply(bus : PipelinedMemoryBus, rspQueue : Int, config : WishboneConfig): Wishbone = {
    apply(bus, rspQueue, config, identity)
  }
  def apply(bus : PipelinedMemoryBus, rspQueue : Int, config : WishboneConfig, addressMap : (UInt => UInt)): Wishbone = {
    val wb = Wishbone(config)
    if(bus.isMasterInterface ^ (bus.component == Component.current)) {
      val adapter = new PipelinedMemoryBusToWishbone(config, bus.config, rspQueue)
      adapter.io.pmb <> bus
      adapter.io.wb <> wb
    } else {
      val adapter = new WishboneToPipelinedMemoryBus(bus.config, config, rspQueue, addressMap = addressMap)
      adapter.io.pmb <> bus
      adapter.io.wb <> wb
    }
    wb
  }
  def apply(bus : PipelinedMemoryBus, config : WishboneConfig, addressMap : (UInt => UInt)): Wishbone = {
    apply(bus, 0, config, addressMap = addressMap)
  }
  def apply(bus : PipelinedMemoryBus, config : WishboneConfig): Wishbone = {
    apply(bus, 0, config, addressMap = identity)
  }

}

import org.scalatest.funsuite.AnyFunSuite

class PipelinedMemoryBusToWishboneTest extends AnyFunSuite {
  def runTest(config : WishboneConfig): Unit = {
    Config.sim.withWave.withVerilator
      .doSim(
        new PipelinedMemoryBusToWishbone(config, PipelinedMemoryBusConfig(32, 32)) {
          val sysclk = Reg(UInt(32 bits)) init(0)
          sysclk := sysclk + 1
          SimPublic(sysclk)
        }.setDefinitionName("ToF_PipelinedMemoryBusToWishbone")
      ) { dut =>
        dut.io.wb.DAT_MISO #= 0xcafecafeL
        dut.io.wb.ACK #= false
        dut.io.pmb.cmd.valid #= false

        dut.clockDomain.forkStimulus(100 MHz)
        SimTimeout(1 ms)
        dut.clockDomain.waitSampling(1)

        val pmb_resp = ScoreboardInOrder[BigInt]()
        val sco = ScoreboardInOrder[WishboneTransaction]()
        val seq = WishboneSequencer{
          WishboneTransaction(BigInt(Random.nextInt(200) * 4),BigInt(Random.nextInt(200)))
        }
        var running = true

        val wb_thread = fork {
          var q = new mutable.Queue[(BigInt, Boolean)]()

          while(running) {
            dut.io.wb.ACK #= false

            if(q.nonEmpty) {
              dut.io.wb.DAT_MISO #= 0xcafecafeL
              if(dut.io.wb.ACK.randomize()) {
                val (addr, was_write) = q.dequeue()
                if(!was_write) {
                  val response = BigInt(Random.nextInt(1000))
                  println(s"Pushing ${addr} ${response} ${dut.sysclk.toBigInt}")
                  dut.io.wb.DAT_MISO #= response
                  pmb_resp.pushRef(response)
                } else {
                  dut.io.wb.DAT_MISO #= 0xbeefbeefL
                }
              }
            }

            var stalled = false
            if(dut.io.wb.STALL != null) {
              stalled = dut.io.wb.STALL.toBoolean
            }

            if (!stalled && dut.io.wb.CYC.toBoolean && dut.io.wb.STB.toBoolean) {
              println(s"Need response for ${dut.io.wb.ADR.toBigInt} ${dut.io.wb.WE.toBoolean} ${dut.sysclk.toBigInt} ${simTime()}")
              q.enqueue((dut.io.wb.ADR.toBigInt, dut.io.wb.WE.toBoolean))
            }

            dut.clockDomain.waitSampling()
          }
        }

        if(dut.io.wb.STALL != null) {
          dut.io.wb.STALL #= true
          dut.clockDomain.onSamplings({
            dut.io.wb.STALL.randomize()
          })
        }

        FlowMonitor(dut.io.pmb.rsp, dut.clockDomain) { rsp =>
          println(s"Got ${rsp.data.toBigInt} ${dut.sysclk.toBigInt} ${simTime()}")
          assert(rsp.data.toBigInt != BigInt(0xcafecafeL))
          pmb_resp.pushDut(rsp.data.toBigInt)
        }

        fork{
          val busStatus = WishboneStatus(dut.io.wb)
          while(true){
            dut.clockDomain.waitSamplingWhere(busStatus.isTransfer)
            val tran = WishboneTransaction.sampleAsSlave(dut.io.wb)
            println(s"Popping ${tran}")
            sco.pushDut(tran)
          }
        }

        for(repeat <- 0 until 1000){
          seq.generateTransactions()
          var tran = seq.nextTransaction(0)
          val we = Random.nextBoolean()
          if(!we) {
            tran = tran.copy(data = BigInt(0xdeadbeefL))
          }
          println(s"Pushing ${tran} ${we} ${dut.sysclk.toBigInt}")
          sco.pushRef(tran)
          dut.io.pmb.cmd.address #= tran.address
          dut.io.pmb.cmd.write #= we
          dut.io.pmb.cmd.valid #= true
          dut.io.pmb.cmd.data #= tran.data
          dut.clockDomain.waitSamplingWhere(dut.io.pmb.cmd.ready.toBoolean)
          dut.io.pmb.cmd.valid #= false
          dut.clockDomain.waitSampling()
        }
        running = false
        wb_thread.join()

      }
  }

  test("PipelinedMemoryBusToWishbone_std") {
    runTest(WishboneConfig(32, 32, addressGranularity = AddressGranularity.BYTE))
  }
  test("PipelinedMemoryBusToWishbone") {
    runTest(WishboneConfig(32, 32, addressGranularity = AddressGranularity.BYTE).pipelined)
  }
}


