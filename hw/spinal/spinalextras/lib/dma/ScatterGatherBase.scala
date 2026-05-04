package spinalextras.lib.dma

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._
import spinal.lib.bus.simple._
import spinal.lib.formal._
import spinal.lib.fsm._
import spinalextras.lib.formal.ComponentWithFormalProperties

// ============================================================
//  Compile-time configuration
// ============================================================
case class DmaConfig(
                      dataWidth   : Int = 32,  // Stream word width in bits
                      addrWidth   : Int = 32,  // Memory address width
                      lengthWidth : Int = 16,  // Transfer length field width (in bytes)
                      burstLen    : Int = 8,   // Max outstanding reads per burst
                      fifoDepth   : Int = 64   // Internal data FIFO depth (words)
                    ) {
  require(isPow2(dataWidth / 8), "dataWidth must be a power-of-2 number of bytes")
  require(isPow2(burstLen),      "burstLen must be a power of 2")
  val byteWidth = dataWidth / 8
}

// ============================================================
//  Linked-list descriptor – lives in system memory
//
//  Word 0  (+0x00): nextPtr
//  Word 1  (+0x04): addr
//  Word 2  (+0x08): flags[31:24] | length[15:0]
//  Word 3  (+0x0C): status write-back (written by engine on completion)
//
//  flags[0] = interruptOnComplete
//  flags[1] = endOfChain
//
//  Status write-back word layout:
//    [0]    = done (always 1)
//    [15:8] = error code (0 = OK)
// ============================================================
case class Descriptor(cfg: DmaConfig) extends Bundle {
  val nextPtr = UInt(cfg.addrWidth bits)
  val addr    = UInt(cfg.addrWidth bits)
  val length  = UInt(cfg.lengthWidth bits)
  val flags   = Bits(8 bits)

  def interruptOnComplete : Bool = flags(0)
  def endOfChain          : Bool = flags(1)
  def lastFrame           : Bool = flags(2)
}

// ============================================================
//  Completion write-back payload
// ============================================================
case class DescCompletion(cfg: DmaConfig) extends Bundle {
  val descBase  = UInt(cfg.addrWidth bits)
  val errorCode = Bits(8 bits)
}

// ============================================================
//  CSR register map  (PipelinedMemoryBus slave, word-addressed)
//
//  0x00  CTRL       [0] enable  [1] soft-reset
//  0x04  STATUS     [0] running  [1] idle  [11:4] last error code
//  0x08  IRQ        [0] pending  (W1C)
//  0x0C  IRQ_EN     [0] global interrupt enable
//  0x10  HEAD_PTR   head of descriptor linked list
//  0x14  CUR_PTR    descriptor currently being processed  (read-only)
//  0x18  BYTES_DONE cumulative bytes transferred  (read-only)
// ============================================================

// ============================================================
//  Abstract base component – shared by MemoryToStream and StreamToMemory
//
//  Bus arbitration strategy
//  ─────────────────────────
//  Every logical bus user gets its own PipelinedMemoryBus master
//  port, and PipelinedMemoryBusArbiter muxes them onto io.mem.
//  The base class owns two ports:
//
//    fetchBus     – descriptor reads   (fetch FSM)
//    writeBackBus – completion writes  (write-back FSM)
//
//  Subclasses declare their data bus port(s) by overriding the
//  lazy val `subclassBusAccessors`.  Because the base class's
//  `arbiter` is also a lazy val, it evaluates after the subclass
//  constructor body has run, so subclassBusAccessors is fully
//  defined by the time the arbiter instantiates.  No explicit
//  "call buildArbiter()" step is needed anywhere.
// ============================================================
abstract class ScatterGatherBase(val cfg: DmaConfig) extends ComponentWithFormalProperties {

  val io = new Bundle {
    val ctrl = slave(PipelinedMemoryBus(cfg.addrWidth, 32))
    val mem  = master(PipelinedMemoryBus(cfg.addrWidth, cfg.dataWidth))
    val irq  = out Bool ()
  }

  // ----------------------------------------------------------
  //  CSRs via BusSlaveFactory
  // ----------------------------------------------------------
  val busFactory = new PipelinedMemoryBusSlaveFactory(io.ctrl)

  val regEnable    = busFactory.createReadWrite(Bool(),                   0x00, 0) init False
  val regSoftReset = busFactory.createReadWrite(Bool(),                   0x00, 1) init False
  val regIrqEnable = busFactory.createReadWrite(Bool(),                   0x0C, 0) init False
  val regHeadPtr   = busFactory.createReadWrite(UInt(cfg.addrWidth bits), 0x10, 0) init 0

  val regIrqPending = RegInit(False)
  val regStatusErr  = Reg(Bits(8 bits))             init 0
  val regCurPtr     = Reg(UInt(cfg.addrWidth bits)) init 0
  val regBytesDone  = Reg(UInt(32 bits))            init 0

  val isRunning = regEnable && !regSoftReset

  busFactory.read(isRunning,     0x04, 0)
  busFactory.read(!isRunning,    0x04, 1)
  busFactory.read(regStatusErr,  0x04, 4)
  busFactory.read(regIrqPending, 0x08, 0)
  busFactory.read(regCurPtr,     0x14, 0)
  busFactory.read(regBytesDone,  0x18, 0)

  // IRQ is W1C: writing 1 to bit 0 clears the pending flag.
  // Write data is available on io.ctrl.cmd.data during the write cycle.
  busFactory.onWrite(0x08) {
    when(io.ctrl.cmd.data(0)) { regIrqPending := False }
  }

  // Soft-reset side-effects
  busFactory.onWrite(0x00) {
    when(io.ctrl.cmd.data(1)) {
      regBytesDone  := 0
      regIrqPending := False
      regStatusErr  := 0
    }
  }

  io.irq := regIrqPending && regIrqEnable

  // ----------------------------------------------------------
  //  Bus ports owned by the base class
  // ----------------------------------------------------------
  val fetchBus     = PipelinedMemoryBus(cfg.addrWidth, cfg.dataWidth)
  val writeBackBus = PipelinedMemoryBus(cfg.addrWidth, cfg.dataWidth)

  // ----------------------------------------------------------
  //  Subclass bus ports (override as a lazy val in each subclass)
  //
  //  lazy val ensures the subclass body has executed and its bus
  //  vals exist before this sequence is read by the arbiter below.
  // ----------------------------------------------------------
  def subclassBusAccessors: Seq[PipelinedMemoryBus]

  val allBuses = Seq(fetchBus, writeBackBus) ++ subclassBusAccessors
  io.mem <> PipelinedMemoryBusArbiter(allBuses, 8, true, true)

  // ----------------------------------------------------------
  //  Completion write-back stream
  //
  //  Subclass data-mover FSMs push a completion here when a
  //  descriptor finishes.  The write-back FSM issues the memory
  //  write and raises regIrqPending if requested.
  // ----------------------------------------------------------
  val completionIn = Stream(new Bundle {
    val completion = DescCompletion(cfg)
    val desc       = Descriptor(cfg)
  })

  val writeBackFsm = new StateMachine {
    val IDLE  = new State with EntryPoint
    val WRITE = new State
    val WAIT  = new State

    val latchedCompletion = Reg(DescCompletion(cfg))
    val latchedDesc       = Reg(Descriptor(cfg))

    // Payload permanently wired from registers — stable for as long as
    // writeBackBus.cmd.valid is asserted.  FSM only needs to drive valid.
    // Status word layout: errorCode[15:8] | 7'b0 | done[0]
    writeBackBus.cmd.valid   := False
    writeBackBus.cmd.write   := True
    writeBackBus.cmd.address := latchedCompletion.descBase + 0x0C
    writeBackBus.cmd.data    :=
      (latchedCompletion.errorCode ## B(0, 7 bits) ## True).asBits.resized
    writeBackBus.cmd.mask    := B(cfg.byteWidth bits, default -> true)

    completionIn.ready := False

    IDLE.whenIsActive {
      completionIn.ready := True
      when(completionIn.fire) {
        latchedCompletion := completionIn.payload.completion
        latchedDesc       := completionIn.payload.desc
        goto(WRITE)
      }
    }

    WRITE.whenIsActive {
      writeBackBus.cmd.valid := True
      when(writeBackBus.cmd.fire) { goto(WAIT) }
    }

    WAIT.whenIsActive {
      when(latchedDesc.interruptOnComplete) { regIrqPending := True }
      goto(IDLE)
    }
  }

  // ----------------------------------------------------------
  //  Descriptor fetch FSM
  //
  //  Drives fetchBus exclusively.  Payload fields are always sourced
  //  from registers so they are stable for as long as cmd.valid is
  //  high, regardless of how long the arbiter stalls cmd.ready.
  // ----------------------------------------------------------
  val fetchedDesc    = Stream(Descriptor(cfg))
  val fetchedDescReg = Reg(Descriptor(cfg))

  val fetchNextPtr = Reg(UInt(cfg.addrWidth bits))
  val fetchAddr    = Reg(UInt(cfg.addrWidth bits))
  val fetchBase    = Reg(UInt(cfg.addrWidth bits))

  fetchBus.cmd.write   := False
  fetchBus.cmd.mask    := B(cfg.byteWidth bits, default -> true)
  fetchBus.cmd.data    := 0
  fetchBus.cmd.address := fetchBase   // default (word 0); overridden for W1/W2
  fetchBus.cmd.valid   := False

  fetchedDesc.valid   := False
  fetchedDesc.payload := fetchedDescReg

  val fetchFsm = new StateMachine {

    val IDLE     = new State with EntryPoint
    val FETCH_W0 = new State
    val WAIT_W0  = new State
    val FETCH_W1 = new State
    val WAIT_W1  = new State
    val FETCH_W2 = new State
    val WAIT_W2  = new State
    val PRESENT  = new State

    IDLE.whenIsActive {
      when(isRunning) {
        fetchBase := regHeadPtr
        goto(FETCH_W0)
      }
    }

    // ---- Word 0: nextPtr  (address = fetchBase) ----
    FETCH_W0.whenIsActive {
      fetchBus.cmd.valid := True
      when(fetchBus.cmd.fire) { goto(WAIT_W0) }
    }
    WAIT_W0.whenIsActive {
      when(fetchBus.rsp.valid) {
        fetchNextPtr := fetchBus.rsp.data.asUInt.resized
        goto(FETCH_W1)
      }
    }

    // ---- Word 1: addr  (address = fetchBase + 4) ----
    FETCH_W1.whenIsActive {
      fetchBus.cmd.valid   := True
      fetchBus.cmd.address := fetchBase + 4
      when(fetchBus.cmd.fire) { goto(WAIT_W1) }
    }
    WAIT_W1.whenIsActive {
      when(fetchBus.rsp.valid) {
        fetchAddr := fetchBus.rsp.data.asUInt.resized
        goto(FETCH_W2)
      }
    }

    // ---- Word 2: flags[31:24] | length[15:0]  (address = fetchBase + 8) ----
    FETCH_W2.whenIsActive {
      fetchBus.cmd.valid   := True
      fetchBus.cmd.address := fetchBase + 8
      when(fetchBus.cmd.fire) { goto(WAIT_W2) }
    }
    WAIT_W2.whenIsActive {
      when(fetchBus.rsp.valid) {
        val word = fetchBus.rsp.data
        fetchedDescReg.nextPtr := fetchNextPtr
        fetchedDescReg.addr    := fetchAddr
        fetchedDescReg.length  := word(cfg.lengthWidth - 1 downto 0).asUInt
        fetchedDescReg.flags   := word(31 downto 24)
        goto(PRESENT)
      }
    }

    // ---- Offer assembled descriptor to the subclass data-mover ----
    // fetchedDesc.payload is wired to fetchedDescReg so it is held
    // stable for as long as valid is asserted.
    PRESENT.whenIsActive {
      fetchedDesc.valid := True
      when(fetchedDesc.fire) {
        regCurPtr := fetchBase
        when(!isRunning || fetchedDescReg.endOfChain) {
          regEnable := False
          goto(IDLE)
        } otherwise {
          fetchBase := fetchedDescReg.nextPtr
          goto(FETCH_W0)
        }
      }
    }
  }

}