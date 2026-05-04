package spinalextras.lib.dma

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._
import spinal.lib.bus.simple._
import spinal.lib.fsm._
import spinalextras.lib.formal.fillins.PipelinedMemoryBusFormal.PipelinedMemoryBusFormalExt
import spinalextras.lib.formal.{FormalProperties, FormalProperty}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

// ============================================================
//  MemoryToStream  –  Memory → RTL Stream
//
//  For each descriptor the engine:
//    1. Issues burst reads from `addr` for `length` bytes via
//       its own dataBus port (arbitrated safely by the base class)
//    2. Pushes words into a StreamFifo which feeds txStream
//       with Fragment.last on the final word
//    3. Pushes a completion onto completionIn for the write-back FSM
// ============================================================
class MemoryToStream(cfg: DmaConfig = DmaConfig()) extends ScatterGatherBase(cfg) {

  val txStream = master(Stream(Fragment(Bits(cfg.dataWidth bits))))

  // ----------------------------------------------------------
  //  Internal FIFO
  // ----------------------------------------------------------
  val dataFifo = StreamFifo(Fragment(Bits(cfg.dataWidth bits)), cfg.fifoDepth)
  txStream << dataFifo.io.pop

  // ----------------------------------------------------------
  //  Data bus – exclusively driven by the data-mover FSM below.
  //
  //  Declared as a lazy val so that it exists before the arbiter
  //  lazy val (in the base class) reads subclassBusAccessors.
  // ----------------------------------------------------------
  lazy val dataBus = PipelinedMemoryBus(cfg.addrWidth, cfg.dataWidth)

  override lazy val subclassBusAccessors: Seq[PipelinedMemoryBus] = {
    Seq(dataBus)
  }

  // ----------------------------------------------------------
  //  Data-mover FSM
  //
  //  WAIT_DESC   – stall until the fetch FSM presents a descriptor
  //  ISSUE_READS – fire read commands up to burstLen in-flight;
  //                simultaneously drain responses into dataFifo
  //  DRAIN       – all commands issued; drain remaining responses
  //  DONE        – latch and push completion, then loop
  // ----------------------------------------------------------
  val dmDesc      = Reg(Descriptor(cfg))
  val dmWordAddr  = Reg(UInt(cfg.addrWidth bits))
  val dmWordsLeft = Reg(UInt(cfg.lengthWidth bits))
  val dmInflight  = CounterUpDown(cfg.burstLen)
  val dmIsLast = Reg(Bool())

  fetchedDesc.ready := False
  dataFifo.io.push.setIdle()

  // Completion payload latched on DONE entry so it is stable for the
  // full duration completionIn.valid is asserted.
  val completionDescBase  = Reg(UInt(cfg.addrWidth bits)) init 0
  val completionErrorCode = Reg(Bits(8 bits))             init 0
  val completionDescSnap  = Reg(Descriptor(cfg))

  completionIn.valid                        := False
  completionIn.payload.completion.descBase  := completionDescBase
  completionIn.payload.completion.errorCode := completionErrorCode
  completionIn.payload.desc                 := completionDescSnap

  // dataBus payload wired from registers — stable whenever valid is high.
  dataBus.cmd.write   := False
  dataBus.cmd.mask    := B(cfg.byteWidth bits, default -> true)
  dataBus.cmd.data    := 0
  dataBus.cmd.address := dmWordAddr
  dataBus.cmd.valid   := False

  def bytesToWords(len: UInt): UInt =
    ((len + U(cfg.byteWidth - 1, cfg.lengthWidth bits)) >> log2Up(cfg.byteWidth)).resized

  // Responses arrive on dataBus.rsp independently of command issue.
  // Because dataBus is a dedicated port, rsp.valid here is always
  // a response to one of our own requests — no owner-flag filtering needed.
  when(dataBus.rsp.valid) {
    val isLast = (dmInflight === 1) && (dmWordsLeft === 0)
    dataFifo.io.push.valid            := True
    dataFifo.io.push.payload.fragment := dataBus.rsp.data
    dataFifo.io.push.payload.last     := isLast && dmIsLast
    dmInflight.decrement()
    regBytesDone := regBytesDone + cfg.byteWidth
  }

  val dataMoverFsm = new StateMachine {

    val WAIT_DESC   = new State with EntryPoint
    val ISSUE_READS = new State
    val DRAIN       = new State
    val DONE        = new State

    // ---- Wait for descriptor ----
    WAIT_DESC.whenIsActive {
      fetchedDesc.ready := True
      when(fetchedDesc.fire) {
        dmDesc      := fetchedDesc.payload
        dmWordAddr  := fetchedDesc.payload.addr//.alignedTo(cfg.byteWidth)
        dmWordsLeft := bytesToWords(fetchedDesc.payload.length)
        dmIsLast := fetchedDesc.lastFrame
        goto(ISSUE_READS)
      }
    }

    // ---- Issue burst reads; concurrently drain responses ----
    ISSUE_READS.whenIsActive {
      val canIssue = dmWordsLeft =/= 0 &&
        !dmInflight.willOverflowIfInc &&
        dataFifo.io.availability > dmInflight

      dataBus.cmd.valid := canIssue
      when(dataBus.cmd.fire) {
        dmWordAddr  := dmWordAddr + cfg.byteWidth
        dmWordsLeft := dmWordsLeft - 1
        dmInflight.increment()
      }

      when(dmWordsLeft === 0 && !canIssue) {
          goto(DRAIN)
      }
    }

    // ---- Drain remaining in-flight reads ----
    DRAIN.whenIsActive {
      when(dataBus.rsp.valid) {
        val isLast = (dmInflight === 1) && (dmWordsLeft === 0)
        when(isLast) { goto(DONE) }
      }
    }

    // ---- Descriptor complete ----
    DONE.onEntry {
      completionDescBase  := regCurPtr
      completionErrorCode := 0
      completionDescSnap  := dmDesc
    }
    DONE.whenIsActive {
      completionIn.valid := True
      when(completionIn.fire) { goto(WAIT_DESC) }
    }
  }

  override def formalComponentProperties(): Seq[FormalProperty] = new FormalProperties(this) {
    addFormalProperty(dmInflight === dataBus.contract.outstandingReads, "DMInFlight should equal outstanding reads")
    when(!(dataMoverFsm.isActive(dataMoverFsm.DRAIN) || dataMoverFsm.isActive(dataMoverFsm.ISSUE_READS))) {
      addFormalProperty(dmInflight === 0, "Outstanding reads should only occur in issue / drain")
    }
    addFormalProperty(dataFifo.io.availability >= dmInflight, "Availability must be greater than in flight")
  }
}

class MemoryToStreamFormalTest extends AnyFunSuite with FormalTestSuite {
  formalTests().foreach(t => test(t._1) { t._2() })

  override def defaultDepth() = 10
  override def generateRtl() = Seq(("Basic", () => new GeneralFormalDut(() => new MemoryToStream())))
}
