package spinalextras.lib.bus.simple

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config, Axi4W}
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusConfig}
import spinal.lib.fsm._
import spinalextras.lib.formal.fillins.PipelinedMemoryBusFormal.PipelinedMemoryBusFormalExt
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties, FormalProperty}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut} // StateMachine is part of this package

/**
 * Configuration for the Axi4ToPipelinedMemoryBus bridge.
 *
 * @param axiConfig The AXI4 configuration for the slave port.
 * @param pmbConfig The PipelinedMemoryBus configuration for the master port.
 * @param readResponseFifoDepth Depth of the FIFO for buffering PMB read responses.
 */
case class Axi4ToPipelinedMemoryBusConfig(
                                           axiConfig: Axi4Config,
                                           readResponseFifoDepth: Int = 8 // Sensible default, can be configured
                                         ) {
  val pmbConfig = PipelinedMemoryBusConfig(axiConfig.addressWidth, axiConfig.dataWidth)
}

/**
 * An AXI4 Slave to PipelinedMemoryBus Master bridge with INCR Burst Support.
 *
 * Translates AXI4 read/write transactions (single beat and INCR bursts)
 * into PipelinedMemoryBus commands using SpinalHDL's StateMachine.
 *
 * Current Limitations:
 * - Only INCR bursts (AWBURST/ARBURST = 01) are supported. FIXED and WRAP are not.
 * - Assumes AXI data width and PMB data width are the same.
 * - AXI AxLOCK, AxCACHE, AxPROT, AxQOS, AxREGION are ignored.
 * - Error responses (BRESP/RRESP other than OKAY) are not generated from PMB.
 * - Processes one AXI transaction (burst or single beat) at a time.
 * - Address increment for INCR assumes AxSIZE correctly reflects bytes per transfer.
 */
class Axi4ToPipelinedMemoryBus(config: Axi4ToPipelinedMemoryBusConfig) extends ComponentWithFormalProperties {
  val io = new Bundle {
    val axi = slave(Axi4(config.axiConfig))
    val pmb = master(PipelinedMemoryBus(config.pmbConfig))
  }

  // Ensure data widths are compatible
  assert(config.axiConfig.dataWidth == config.pmbConfig.dataWidth,
    "AXI4 and PipelinedMemoryBus data widths must be equal for this bridge.")
  assert(config.axiConfig.useBurst, "AXI4 config must have useBurst=true for this bridge.")

  // --- Default AXI Slave Output Values ---
  // These are set here and overridden by the FSM where necessary.
  // AW Channel
  io.axi.aw.ready := False
  // W Channel
  io.axi.w.ready  := False
  // B Channel
  io.axi.b.valid  := False
  io.axi.b.payload.id   := 0
  io.axi.b.payload.resp := Axi4.resp.OKAY
  // AR Channel
  io.axi.ar.ready := False
  // R Channel
  io.axi.r.valid  := False
  io.axi.r.payload.id   := 0
  io.axi.r.payload.data := 0
  io.axi.r.payload.resp := Axi4.resp.OKAY
  io.axi.r.payload.last := False

  // --- Default PMB Master Output Values ---
  io.pmb.cmd.valid   := False
  io.pmb.cmd.payload.address := 0
  io.pmb.cmd.payload.write   := False
  io.pmb.cmd.payload.data    := 0
  io.pmb.cmd.payload.mask    := 0

  // --- Write Path Registers ---
  val aw_id_reg      = Reg(config.axiConfig.idType)
  val aw_addr_reg    = Reg(config.axiConfig.addressType) // Start address of burst
  val aw_len_reg     = Reg(config.axiConfig.lenType)     // Number of transfers - 1
  val aw_size_reg    = RegInit(io.axi.aw.size)    // log2(bytes per transfer)
  val aw_burst_reg   = RegInit(io.axi.aw.burst)

  val current_write_addr  = Reg(config.axiConfig.addressType) // Address for current beat
  val write_beats_done    = Reg(config.axiConfig.lenType)     // Beats sent to PMB
  val w_payload_reg       = Reg(Axi4W(config.axiConfig))      // Buffer for one W beat
  val w_payload_valid_reg = Reg(Bool()) init(False)           // Indicates if w_payload_reg is valid

  // --- Read Path Registers ---
  val ar_id_reg      = Reg(config.axiConfig.idType)
  val ar_addr_reg    = Reg(config.axiConfig.addressType) // Start address of burst
  val ar_len_reg     = Reg(config.axiConfig.lenType)     // Number of transfers - 1
  val ar_size_reg    = RegInit(io.axi.ar.size)    // log2(bytes per transfer)
  val ar_burst_reg   = RegInit(io.axi.ar.burst)

  val current_read_addr      = Reg(config.axiConfig.addressType) // Address for current PMB command
  val read_pmb_cmds_sent     = Reg(config.axiConfig.lenType)     // PMB Read commands sent
  val read_r_beats_sent      = Reg(config.axiConfig.lenType)     // AXI R beats sent to master

  // FIFO for buffering PMB read responses
  val pmb_rsp_fifo = StreamFifo(
    dataType = cloneOf(io.pmb.rsp.payload.data),
    depth    = config.readResponseFifoDepth
  )
  pmb_rsp_fifo.io.pop.setBlocked()
  pmb_rsp_fifo.io.push << io.pmb.rsp.map(_.data).toStream // Connect PMB response to FIFO input

  val inFlightCounter = CounterUpDown(pmb_rsp_fifo.depth,
    incWhen = io.pmb.cmd.fire && ~io.pmb.cmd.write,
    decWhen = pmb_rsp_fifo.io.pop.fire)

  val readStall = inFlightCounter.willOverflowIfInc

  // --- Address Increment Calculation ---
  def get_addr_increment(axSize: UInt): UInt = {
    val increment = UInt(config.axiConfig.addressWidth bits)
    increment := (U(1) << axSize).resized
    return increment
  }

  // --- State Machine Definition ---
  val fsm = new StateMachine {
    // Define States
    val sIdle                = new State with EntryPoint
    val sWriteBurstReceiveW  = new State
    val sWriteBurstSendPmb   = new State
    val sWriteBurstSendBresp = new State
    val sReadBurstSendPmb    = new State
    val sReadBurstWaitRsp    = new State // May not be strictly needed if SEND_R handles waiting
    val sReadBurstSendR      = new State

    // State: S_IDLE
    sIdle.whenIsActive {
      io.axi.aw.ready := True
      io.axi.ar.ready := True
      w_payload_valid_reg := False // Clear any buffered write data

      when(io.axi.aw.fire) { // Write Address received
        aw_id_reg    := io.axi.aw.payload.id
        aw_addr_reg  := io.axi.aw.payload.addr
        aw_len_reg   := io.axi.aw.payload.len
        aw_size_reg  := io.axi.aw.payload.size
        aw_burst_reg := io.axi.aw.payload.burst
        current_write_addr := io.axi.aw.payload.addr
        write_beats_done   := 0
        assume(io.axi.aw.payload.burst === Axi4.burst.INCR)//, "Bridge only supports INCR bursts for writes")
        goto(sWriteBurstReceiveW)
      }
        .elsewhen(io.axi.ar.fire) { // Read Address received
          ar_id_reg    := io.axi.ar.payload.id
          ar_addr_reg  := io.axi.ar.payload.addr
          ar_len_reg   := io.axi.ar.payload.len
          ar_size_reg  := io.axi.ar.payload.size
          ar_burst_reg := io.axi.ar.payload.burst
          current_read_addr  := io.axi.ar.payload.addr
          read_pmb_cmds_sent := 0
          read_r_beats_sent  := 0
          assume(io.axi.ar.payload.burst === Axi4.burst.INCR)//, "Bridge only supports INCR bursts for reads")
          goto(sReadBurstSendPmb)
        }
    }

    // State: S_WRITE_BURST_RECEIVE_W
    sWriteBurstReceiveW.whenIsActive {
      io.axi.w.ready := !w_payload_valid_reg // Ready if internal buffer is free

      when(io.axi.w.fire) {
        w_payload_reg       := io.axi.w.payload
        w_payload_valid_reg := True
        goto(sWriteBurstSendPmb)
      }
    }

    // State: S_WRITE_BURST_SEND_PMB
    sWriteBurstSendPmb.whenIsActive {
      when(w_payload_valid_reg) { // We have write data to send
        io.pmb.cmd.valid           := True
        io.pmb.cmd.payload.address := current_write_addr
        io.pmb.cmd.payload.write   := True
        io.pmb.cmd.payload.data    := w_payload_reg.data
        io.pmb.cmd.payload.mask    := w_payload_reg.strb

        when(io.pmb.cmd.fire) { // PMB accepted the write command
          w_payload_valid_reg := False // Consume the buffered write data
          write_beats_done    := write_beats_done + 1
          current_write_addr  := current_write_addr + get_addr_increment(aw_size_reg)

          when (write_beats_done === (aw_len_reg + 1)) { // All beats of burst sent to PMB
            goto(sWriteBurstSendBresp)
          } otherwise { // More beats to come for this burst
            goto(sWriteBurstReceiveW)
          }
        }
      }
    }

    // State: S_WRITE_BURST_SEND_BRESP
    sWriteBurstSendBresp.whenIsActive {
      io.axi.b.valid        := True
      io.axi.b.payload.id   := aw_id_reg
      io.axi.b.payload.resp := Axi4.resp.OKAY

      when(io.axi.b.fire) { // B response accepted by master
        goto(sIdle)
      }
    }

    // State: S_READ_BURST_SEND_PMB
    sReadBurstSendPmb.whenIsActive {
      val all_pmb_cmds_sent = read_pmb_cmds_sent === (ar_len_reg + 1)

      when(!all_pmb_cmds_sent) {
        io.pmb.cmd.valid           := !readStall // PMB command is valid
        io.pmb.cmd.payload.address := current_read_addr
        io.pmb.cmd.payload.write   := False // It's a read command

        when(io.pmb.cmd.fire) { // PMB accepted the read command
          read_pmb_cmds_sent := read_pmb_cmds_sent + 1
          current_read_addr  := current_read_addr + get_addr_increment(ar_size_reg)

          // If this was the last PMB command to send for the burst, move to wait for/send responses
          when ((read_pmb_cmds_sent + 1) === (ar_len_reg + 1)) { // Check for next count
            goto(sReadBurstWaitRsp) // Or directly to sReadBurstSendR if FIFO is expected to have data soon
          }
          // else, stay in this state to send next PMB command in next cycle if PMB is ready
        }
      } otherwise { // All PMB commands for the burst have been sent
        goto(sReadBurstWaitRsp) // Or directly to sReadBurstSendR
      }
    }

    // State: S_READ_BURST_WAIT_RSP
    // This state ensures all PMB commands are issued before focusing on AXI R.
    // It also acts as a waiting point if the R channel is not ready or FIFO is empty.
    sReadBurstWaitRsp.whenIsActive {
      val all_r_beats_sent = read_r_beats_sent === (ar_len_reg + 1)
      when (all_r_beats_sent) {
        goto(sIdle) // All done
      } otherwise {
        // If there's data in FIFO, or we expect data, move to try sending it
        goto(sReadBurstSendR)
      }
    }

    // State: S_READ_BURST_SEND_R
    sReadBurstSendR.whenIsActive {
      val total_beats_expected = ar_len_reg + 1
      val is_last_beat_to_send = read_r_beats_sent + 1 === total_beats_expected

      // Connect AXI R channel to FIFO output
      io.axi.r.valid := pmb_rsp_fifo.io.pop.valid // Valid if FIFO has data
      pmb_rsp_fifo.io.pop.ready := io.axi.r.ready  // Pop from FIFO when AXI master is ready

      io.axi.r.payload.id   := ar_id_reg
      io.axi.r.payload.data := pmb_rsp_fifo.io.pop.payload
      io.axi.r.payload.resp := Axi4.resp.OKAY
      io.axi.r.payload.last := pmb_rsp_fifo.io.pop.valid && is_last_beat_to_send // Assert RLAST on the last beat

      when(pmb_rsp_fifo.io.pop.fire) { // Data was sent on AXI R channel
        read_r_beats_sent := read_r_beats_sent + 1
        when (is_last_beat_to_send) {
          goto(sIdle) // All R beats sent
        } otherwise {
          // Stay in this state to send more R beats. If FIFO becomes empty,
          // io.axi.r.valid will go low, and we'll wait for more PMB responses.
          // No explicit transition to WAIT_RSP needed here as this state handles waiting for FIFO data.
        }
      } .elsewhen (read_r_beats_sent === total_beats_expected && total_beats_expected =/= U(0)) { // Safety check if all beats sent but not transitioned
        goto(sIdle)
      } .elsewhen (!pmb_rsp_fifo.io.pop.valid && read_r_beats_sent < total_beats_expected) {
        // If FIFO is empty and we still need to send R beats,
        // we must wait for PMB responses to arrive in the FIFO.
        // We also need to ensure all PMB commands have been sent.
        val all_pmb_cmds_sent_check = read_pmb_cmds_sent === (ar_len_reg + 1)
        when (!all_pmb_cmds_sent_check) {
          goto(sReadBurstSendPmb) // Should not happen if logic is correct, but as a fallback
        }
        // Otherwise, stay in this state (sReadBurstSendR), io.axi.r.valid is already low.
      }
    }
  }

  override def formalComponentProperties() = new FormalProperties {
    addFormalProperty(io.pmb.contract.outstandingReads +^ pmb_rsp_fifo.io.occupancy === inFlightCounter, "Flight counter should account for outstanding reads and items in the fifo")
  }
}

object Axi4ToPipelinedMemoryBusVerilogWithBurstFSM {
  def main(args: Array[String]): Unit = {
    val axiConf = Axi4Config(
      addressWidth = 32,
      dataWidth    = 32,
      idWidth      = 4,
      useLock      = false,
      useRegion    = false,
      useCache     = false,
      useProt      = false,
      useQos       = false,
      useLen       = true, // Enable LEN for burst
      useBurst     = true  // Enable BURST field
    )

    val bridgeConfig = Axi4ToPipelinedMemoryBusConfig(
      axiConfig = axiConf,
      readResponseFifoDepth = 8 // Example FIFO depth
    )

    SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC),
      targetDirectory = "rtl/Axi4ToPmbBridgeWithBurstFSM"
    ).generateVerilog(new Axi4ToPipelinedMemoryBus(bridgeConfig))

    println("Verilog generated in rtl/Axi4ToPmbBridgeWithBurstFSM")
  }
}


class Axi4ToPipelinedMemoryBusTester extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 15

  formalTests().foreach(t => test(t._1) {
    t._2()
  })

  override def generateRtl() = {
    for (
      config <- Seq(Axi4ToPipelinedMemoryBusConfig(Axi4Config(32, 32, idWidth = 8)))
    ) yield {
      (s"${suiteName}", () =>
        GeneralFormalDut(() => new Axi4ToPipelinedMemoryBus(config)))
    }
  }
}


