package spinalextras.lib.bus

import spinal.core._

import spinal.core.sim.{SimBitVectorPimper, SimBoolPimper, SimClockDomainHandlePimper, SimPublic, SimTimeout, fork, simTime}
import spinal.lib.bus.simple._
import spinal.lib.bus.wishbone._
import spinal.lib._
import spinalextras.lib.formal.ComponentWithFormalProperties
import spinal.lib.sim.{FlowMonitor, ScoreboardInOrder}
import spinal.lib.wishbone.sim.{WishboneDriver, WishboneMonitor, WishboneSequencer, WishboneStatus, WishboneTransaction}
import spinalextras.lib.Config
import spinalextras.lib.bus._
import spinalextras.lib.testing.test_funcs

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
      readyForNewReq.setAsReg() init(True) clearWhen(requestAccepted && !ack) setWhen(ack)
      hasOutstandingReq := !readyForNewReq
      reqWasWE := we
    }

    (readyForNewReq, hasOutstandingReq, reqWasWE)
  }
}

case class WishboneToPipelinedMemoryBus(pipelinedMemoryBusConfig : PipelinedMemoryBusConfig,
                                        wbConfig: WishboneConfig, rspQueue : Int = 8, addressMap : (UInt => UInt) = identity) extends ComponentWithFormalProperties {
  val io = new Bundle {
    val wb = slave(Wishbone(wbConfig))
    val pmb = master(PipelinedMemoryBus(pipelinedMemoryBusConfig))
  }

  assert(io.pmb.cmd.valid === False || (io.wb.byteAddress() & (io.wb.config.wordAddressInc() - 1)) === 0, "PMB needs word alignment")
  val pendingRead = RegInit(False) setWhen(io.pmb.readRequestFire) clearWhen(io.wb.ACK)
  io.pmb.cmd.valid := io.wb.masterHasRequest & !pendingRead
  io.pmb.cmd.data := io.wb.DAT_MOSI
  io.pmb.cmd.address := io.wb.byteAddress().resized
  io.pmb.cmd.write := io.wb.WE

  if(io.wb.SEL != null) {
    io.pmb.cmd.mask := io.wb.SEL
  } else {
    io.pmb.cmd.mask.setAll()
  }
  if(io.wb.ERR != null) {
    io.wb.ERR := False
  }
  io.wb.DAT_MISO := io.pmb.rsp.data
  io.wb.ACK := io.pmb.rsp.valid

  when(io.wb.masterHasRequest) {
    when(io.wb.WE) {
      io.wb.ACK := io.pmb.cmd.fire
    } otherwise {
      //pendingRead := io.pmb.cmd.fire && !io.pmb.rsp.valid
    }
  }

//
//  override protected def formalChecks()(implicit useAssumes: Boolean): Unit = new Composite(this, FormalCompositeName) {
//    assertOrAssume(pendingRead.asUInt === io.pmb.formalContract.outstandingReads)
//    when(pendingRead) {
//      assertOrAssume(io.wb.masterHasRequest && !io.wb.WE)
//    }
//    formalCheckOutputsAndChildren()
//  }
}

object WishboneToPipelinedMemoryBus {
  def apply(bus: Wishbone, rspQueue : Int): PipelinedMemoryBus = {
    val config = PipelinedMemoryBusConfig(addressWidth = bus.byteAddress().getWidth, dataWidth = bus.config.dataWidth)
    WishboneToPipelinedMemoryBus(bus, rspQueue, config)
  }

  def apply(bus: Wishbone, rspQueue : Int, config : PipelinedMemoryBusConfig): PipelinedMemoryBus = {
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

case class PipelinedMemoryBusToWishbone(wbConfig: WishboneConfig, pipelinedMemoryBusConfig : PipelinedMemoryBusConfig, rspQueue : Int = 8) extends ComponentWithFormalProperties {
  //assert(wbConfig.dataWidth == pipelinedMemoryBusConfig.dataWidth)

  val io = new Bundle {
    val pmb = slave(PipelinedMemoryBus(pipelinedMemoryBusConfig))
    val wb = master(Wishbone(wbConfig))
  }

  //test_funcs.assertPMBContract(io.pmb)
  //test_funcs.assertWishboneBusContract(io.wb)

  val (readyForNewReq, hasOutstandingReq, reqWasWE) =
    WishbonePipelinedHelpers.create_translation_signals(io.wb.WE, io.pmb.cmd.fire, io.wb.ACK,
      if(wbConfig.isPipelined) rspQueue else 1)

  io.wb.assignByteAddress(io.pmb.cmd.address, allowAddressResize = true)
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
  val rsp = cloneOf(io.pmb.rsp)
  rsp.valid := !reqWasWE & io.wb.isRequestAck
  rsp.payload.data := io.wb.DAT_MISO.resized
  io.pmb.rsp <> rsp.stage()
}

object PipelinedMemoryBusToWishbone {
  def apply(bus : PipelinedMemoryBus, rspQueue : Int): Wishbone = {
    apply(bus, rspQueue, WishboneConfig(addressWidth = bus.config.addressWidth, dataWidth = bus.config.dataWidth, useSTALL = rspQueue > 1, addressGranularity = AddressGranularity.BYTE))
  }
  def apply(bus : PipelinedMemoryBus, rspQueue : Int, config : WishboneConfig): Wishbone = {
    apply(bus, rspQueue, config, identity)
  }
  def apply(bus : PipelinedMemoryBus, rspQueue : Int, config : WishboneConfig, addressMap : (UInt => UInt)): Wishbone = {
    val wb = Wishbone(config)
    if(bus.isMasterInterface ^ (bus.component == Component.current)) new Composite(bus, "pmb2wb") {
      val adapter = new PipelinedMemoryBusToWishbone(config, bus.config, rspQueue)
      adapter.io.pmb <> bus
      adapter.io.wb <> wb
      //bus.assertBusEquivalence(adapter.io.pmb)
      //wb.formalAssertEquivalence(adapter.io.wb)
    } else new Composite(bus, "wb2pmb") {
      val adapter = new WishboneToPipelinedMemoryBus(bus.config, config, rspQueue, addressMap = addressMap)
      adapter.io.pmb <> bus
      adapter.io.wb <> wb
      //bus.assertBusEquivalence(adapter.io.pmb)
      //wb.formalAssertEquivalence(adapter.io.wb)
    }
    wb
  }
  def apply(bus : PipelinedMemoryBus, config : WishboneConfig, addressMap : (UInt => UInt)): Wishbone = {
    apply(bus, 0, config, addressMap = addressMap)
  }
  def apply(bus : PipelinedMemoryBus, config : WishboneConfig): Wishbone = {
    apply(bus, 0, config, addressMap = identity)
  }
  def createDriver(bus : PipelinedMemoryBus, config : WishboneConfig): Wishbone = new Composite(bus, "wb2pmb"){
    val wb = Wishbone(config)
    val adapter = new WishboneToPipelinedMemoryBus(bus.config, config, 0, addressMap = identity)
    adapter.io.pmb <> bus
    adapter.io.wb <> wb
    //bus.assertBusEquivalence(adapter.io.pmb)
    //wb.formalAssertEquivalence(adapter.io.wb)
  }.wb
}




