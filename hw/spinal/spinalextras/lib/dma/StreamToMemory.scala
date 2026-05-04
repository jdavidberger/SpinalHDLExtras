package spinalextras.lib.dma

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._
import spinal.lib.bus.simple._
import spinal.lib.fsm._
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

import scala.language.postfixOps

// ============================================================
//  StreamToMemory  –  RTL Stream → Memory
//
//  For each descriptor the engine:
//    1. Accepts words from rxStream into a StreamFifo
//    2. Pops words from the FIFO and issues write commands via
//       its own dataBus port (arbitrated safely by the base class)
//       until Fragment.last is seen OR `length` bytes consumed
//    3. Pushes a completion onto completionIn for the write-back FSM
//
//  Fragment.last acts as an early-termination signal (packet boundary).
//  The descriptor `length` field is a maximum / buffer size guard.
// ============================================================
class StreamToMemory(cfg: DmaConfig = DmaConfig()) extends ScatterGatherBase(cfg) {

  val rxStream = slave(Stream(Fragment(Bits(cfg.dataWidth bits))))

  // ----------------------------------------------------------
  //  Internal FIFO
  // ----------------------------------------------------------
  val dataFifo = StreamFifo(Fragment(Bits(cfg.dataWidth bits)), cfg.fifoDepth)
  dataFifo.io.push << rxStream

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
  //  WAIT_DESC  – stall until the fetch FSM presents a descriptor
  //  WRITE_DATA – pop FIFO words and issue write commands
  //  DONE       – latch and push completion, then loop
  //
  //  The FIFO pop and the bus write are a single joint transaction:
  //  dataFifo.io.pop.ready is tied to dataBus.cmd.fire so both
  //  advance together, preserving the Stream contract on the pop side.
  // ----------------------------------------------------------
  val dmDesc      = Reg(Descriptor(cfg))
  val dmWordAddr  = Reg(UInt(cfg.addrWidth bits))
  val dmWordsMax  = Reg(UInt(cfg.lengthWidth bits))
  val dmWordsWrit = Reg(UInt(cfg.lengthWidth bits))

  fetchedDesc.ready     := False
  dataFifo.io.pop.ready := False

  val completionDescBase  = Reg(UInt(cfg.addrWidth bits)) init 0
  val completionErrorCode = Reg(Bits(8 bits))             init 0
  val completionDescSnap  = Reg(Descriptor(cfg))

  completionIn.valid                        := False
  completionIn.payload.completion.descBase  := completionDescBase
  completionIn.payload.completion.errorCode := completionErrorCode
  completionIn.payload.desc                 := completionDescSnap

  // dataBus payload wired from registers — stable whenever valid is high.
  // .data sources from the FIFO pop fragment, which is also a stable
  // registered value held until dataFifo.io.pop.ready fires.
  dataBus.cmd.write   := True
  dataBus.cmd.mask    := B(cfg.byteWidth bits, default -> true)
  dataBus.cmd.valid   := False
  dataBus.cmd.address := dmWordAddr
  dataBus.cmd.data    := dataFifo.io.pop.payload.fragment

  def bytesToWords(len: UInt): UInt =
    ((len + U(cfg.byteWidth - 1, cfg.lengthWidth bits)) >> log2Up(cfg.byteWidth)).resized

  val dataMoverFsm = new StateMachine {

    val WAIT_DESC  = new State with EntryPoint
    val WRITE_DATA = new State
    val DONE       = new State

    // ---- Wait for descriptor ----
    WAIT_DESC.whenIsActive {
      fetchedDesc.ready := True
      when(fetchedDesc.fire) {
        dmDesc      := fetchedDesc.payload
        dmWordAddr  := fetchedDesc.payload.addr//.alignedTo(cfg.byteWidth)
        dmWordsMax  := bytesToWords(fetchedDesc.payload.length)
        dmWordsWrit := 0
        goto(WRITE_DATA)
      }
    }

    // ---- Pop FIFO words and write to memory ----
    WRITE_DATA.whenIsActive {
      val bufferNotFull = dmWordsWrit < dmWordsMax

      dataBus.cmd.valid     := dataFifo.io.pop.valid && bufferNotFull
      dataFifo.io.pop.ready := dataBus.cmd.fire

      when(dataBus.cmd.fire) {
        dmWordAddr   := dmWordAddr + cfg.byteWidth
        dmWordsWrit  := dmWordsWrit + 1
        regBytesDone := regBytesDone + cfg.byteWidth

        when(dataFifo.io.pop.payload.last || (dmWordsWrit === dmWordsMax - 1)) {
          goto(DONE)
        }
      }

      // Discard any words the stream delivers beyond the descriptor limit
      when(dataFifo.io.pop.valid && !bufferNotFull) {
        dataFifo.io.pop.ready := True
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
}

class StreamToMemoryFormalTest extends AnyFunSuite with FormalTestSuite {
  formalTests().foreach(t => test(t._1) { t._2() })

  override def defaultDepth() = 10
  override def generateRtl() = Seq(("Basic", () => new GeneralFormalDut(() => new StreamToMemory())))
}
