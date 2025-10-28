package spinalextras.lib.bus.simple

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim.{SimBaseTypePimper, SimBoolPimper, SimClockDomainHandlePimper, SimEquivBitVectorBigIntPimper, SimTimeout, simCompiled}
import spinal.lib._
import spinal.lib.bus.amba4.axi.Axi4.resp.OKAY
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config, Axi4R, Axi4W}
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusConfig}
import spinal.lib.fsm._
import spinalextras.lib.formal.fillins.Axi4Formal.Axi4FormalExt
import spinalextras.lib.{Config, Memories, MemoryRequirement}
import spinalextras.lib.formal.fillins.PipelinedMemoryBusFormal.PipelinedMemoryBusFormalExt
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties, FormalProperty, HasFormalProperties}
import spinalextras.lib.logging.{GlobalLogger, SignalLogger}
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
                                           readResponseFifoDepth: Int = 4, // Sensible default, can be configured
                                           readResponseFifoLatency: Int = 2,
                                           readResponseFifoForFMax: Boolean = false
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

  io.axi.b.setIdle()

  // AR Channel
  io.axi.ar.ready := False

  // --- Address Increment Calculation ---
  def get_addr_increment(axSize: UInt): UInt = {
    (U(1) << axSize).resized
  }

  val addr_increment = RegInit(U(0, log2Up(1 << io.axi.ar.size.maxValue.toInt) bits))

  val write_area = new Area {
    // --- Write Path Registers ---
    val aw_reg = Reg(cloneOf(io.axi.aw))
    val current_write_addr = Reg(config.axiConfig.addressType) // Address for current beat

    val writeMode = Bool()
    writeMode := False

    io.axi.w.continueWhen(writeMode).map(axi_w => {
      val write = cloneOf(io.pmb.cmd.payload)
      write.address := current_write_addr
      write.data := axi_w.data
      write.mask := axi_w.strb
      write.write := True
      write
    }) <> io.pmb.cmd

    when(io.axi.w.fire) {
      current_write_addr  := current_write_addr + addr_increment
    }
  }
  // --- Read Path Registers ---

  val read_area = new Area {
    val readMode = False
    val ar_reg = Reg(cloneOf(io.axi.ar)) init({
      val init = cloneOf(io.axi.ar)
      init.clearAll()
      init
    })
    val read_beats_remaining, rsp_beats_remaining = Reg(cloneOf(io.axi.ar.len +^ 1)) init(0)

    val current_read_addr = Reg(config.axiConfig.addressType) init(0)// Address for current PMB command
    val read_pmb_cmds_sent = Reg(UInt(9 bits)) init(0) // PMB Read commands sent

    // FIFO for buffering PMB read responses
    val pmb_rsp_fifo = StreamFifo(
      dataType = cloneOf(io.pmb.rsp.payload.data),
      depth = config.readResponseFifoDepth,
      latency = config.readResponseFifoLatency,
      forFMax = config.readResponseFifoForFMax
    )

    val rspCount = Counter(8 bits)

    pmb_rsp_fifo.io.push << io.pmb.rsp.map(_.data).toStream // Connect PMB response to FIFO input
    pmb_rsp_fifo.io.pop.map(rsp => {
      val r = cloneOf(io.axi.r.payload)
      r.last := rsp_beats_remaining === 1
      r.resp := Axi4.resp.OKAY
      r.data := rsp
      r.id := ar_reg.id
      r
    }) <> io.axi.r

    assert(ar_reg.len -^ rspCount +^ ar_reg.valid.asUInt === rsp_beats_remaining)
    when(io.axi.r.fire) {
      rspCount.increment()
      rsp_beats_remaining := rsp_beats_remaining - 1
    }

    val inFlightCounter = CounterUpDown(pmb_rsp_fifo.depth,
      incWhen = io.pmb.cmd.fire && ~io.pmb.cmd.write,
      decWhen = pmb_rsp_fifo.io.pop.fire)

    val readStall = inFlightCounter.willOverflowIfInc
  }

  // --- State Machine Definition ---
  val fsm = new StateMachine {
    // Define States
    val sIdle                = new State with EntryPoint
    val sWriteBurstReceiveW  = new State
    val sWriteBurstSendBresp = new State

    val sReadBurstSendPmb    = new State
    val sReadBurstWaitRsp    = new State // May not be strictly needed if SEND_R handles waiting

    // State: S_IDLE
    sIdle.whenIsActive {
      io.axi.aw.ready := True
      io.axi.ar.ready := !io.axi.aw.valid

      when(io.axi.aw.fire) { // Write Address received
        write_area.aw_reg := io.axi.aw
        addr_increment := get_addr_increment(io.axi.aw.size)

        write_area.current_write_addr := io.axi.aw.payload.addr
        assert(io.axi.aw.payload.burst === Axi4.burst.INCR)//, "Bridge only supports INCR bursts for writes")
        assume(io.axi.aw.payload.burst === Axi4.burst.INCR)//, "Bridge only supports INCR bursts for writes")
        goto(sWriteBurstReceiveW)
      }.elsewhen(io.axi.ar.fire) { // Read Address received
        read_area.ar_reg := io.axi.ar
        addr_increment := get_addr_increment(io.axi.aw.size)

        read_area.read_beats_remaining := io.axi.ar.len +^ 1
        read_area.rsp_beats_remaining := io.axi.ar.len +^ 1

        read_area.current_read_addr  := io.axi.ar.payload.addr
        read_area.read_pmb_cmds_sent := 0
        assert(io.axi.ar.payload.burst === Axi4.burst.INCR)//, "Bridge only supports INCR bursts for reads")
        assume(io.axi.ar.payload.burst === Axi4.burst.INCR)//, "Bridge only supports INCR bursts for reads")
        goto(sReadBurstSendPmb)
      }
    }

    // State: S_WRITE_BURST_RECEIVE_W
    sWriteBurstReceiveW.whenIsActive {
      write_area.writeMode := True
      when(io.axi.w.last && io.axi.w.fire) {
        goto(sWriteBurstSendBresp)
      }
    }

    sWriteBurstSendBresp.whenIsActive {
      io.axi.b.valid := True
      io.axi.b.resp := OKAY
      io.axi.b.id := write_area.aw_reg.id

      when(io.axi.b.fire) {
        goto(sIdle)
      }
    }

    // State: S_READ_BURST_SEND_PMB
    sReadBurstSendPmb.whenIsActive {
      read_area.readMode := True
      io.pmb.cmd.valid           := !read_area.readStall // PMB command is valid
      io.pmb.cmd.payload.address := read_area.current_read_addr
      io.pmb.cmd.payload.write   := False // It's a read command

      assert(read_area.read_pmb_cmds_sent <= (read_area.ar_reg.len))

      when(io.pmb.cmd.fire) { // PMB accepted the read command
        read_area.read_beats_remaining := read_area.read_beats_remaining - 1
        read_area.read_pmb_cmds_sent := read_area.read_pmb_cmds_sent + 1
        read_area.current_read_addr  := read_area.current_read_addr + addr_increment

        // If this was the last PMB command to send for the burst, move to wait for/send responses
        when(read_area.read_beats_remaining === 1) {
          goto(sReadBurstWaitRsp) // Or directly to sReadBurstSendR if FIFO is expected to have data soon
        }
      }
    }

    sReadBurstWaitRsp.whenIsActive {
      read_area.readMode := True
      when(io.axi.r.fire && io.axi.r.last) {
        read_area.rspCount.clear()
        read_area.read_pmb_cmds_sent := 0
        read_area.ar_reg.clearAll()

        goto(sIdle)
      }
    }
  }

  override def covers(): Seq[FormalProperty] = Seq(io.axi.r.last && io.axi.r.fire)

  override def formalComponentProperties() = new FormalProperties {
    addFormalProperty(io.pmb.contract.outstandingReads +^ read_area.pmb_rsp_fifo.io.occupancy === read_area.inFlightCounter, "Flight counter should account for outstanding reads and items in the fifo")

    addFormalProperty((read_area.rspCount +^ read_area.inFlightCounter.value) === read_area.read_pmb_cmds_sent, "In flight + rsp inc should equal pmb sents")
    addFormalProperty(read_area.read_pmb_cmds_sent <= (read_area.ar_reg.len +^ 1), "read pmbs sent cant exceed the request length")

    //addFormalProperty(io.axi.readContract.outstandingReads >= read_area.pmb_rsp_fifo.io.occupancy +^ io.pmb.contract.outstandingReads)
    //addFormalProperty(((read_area.read_beats_remaining +^ read_area.inFlightCounter) <= read_area.rsp_beats_remaining), "Maintain relationship between read and rsp")

    addFormalProperty(io.axi.readContract.outstandingReadsPerId(read_area.ar_reg.id) === read_area.rsp_beats_remaining, "Outstanding read counts")
    addFormalProperty(io.axi.readContract.outstandingBurstsPerId(read_area.ar_reg.id) <= 1, "Current ID should have at most one active burst")

    io.axi.readContract.outstandingReadsPerId.zipWithIndex.map(x => {
      when(x._2 =/= read_area.ar_reg.id || !read_area.ar_reg.valid) {
        addFormalProperty(io.axi.readContract.outstandingBurstsPerId(x._2) === 0, s"Inactive channel ${x._2} should have no bursts")
        addFormalProperty(io.axi.readContract.outstandingReadsPerId(x._2) === U(0), s"Inactive channel ${x._2} should be empty")
      }
    })

    // # Cmds to read + # Reads in flight -
//    val expectedAxiReadCount = (read_area.inFlightCounter +^ read_area.read_beats_remaining)
//    addFormalProperty(io.axi.readContract.outstandingReads === expectedAxiReadCount, "Maintain read counters between PMB and AXI")

    when(read_area.readMode) {
      addFormalProperty(read_area.rsp_beats_remaining =/= 0, "rsp remaining should be nonzero 0 when in readmode")
      addFormalProperty((read_area.read_beats_remaining +^ read_area.read_pmb_cmds_sent) === (read_area.ar_reg.len +^ 1))
    } otherwise {
      //addFormalProperty(expectedAxiReadCount === 0, "Expected reads should be zero")
      addFormalProperty(read_area.rsp_beats_remaining === 0, "rsp remaining should be 0 outside of readmode")
      addFormalProperty(read_area.rspCount === 0, "rsp count should be 0 outside of readmode")
      addFormalProperty(read_area.inFlightCounter.value === 0, "If there are reads in flight, we need to be in one of the read modes")
      addFormalProperty(read_area.read_pmb_cmds_sent === 0)
    }
  }

  fsm.build()

  GlobalLogger(
    Set("axi4-to-pmb"),
    SignalLogger.concat("Axi4ToPmb",
      RegNext(fsm.stateReg, init = fsm.stateReg.clone().clearAll()).setName("stateReg"),
      addr_increment
    )
  )
}

object Axi4ToPipelinedMemoryBus {
  def apply(cfg : Axi4Config): Axi4ToPipelinedMemoryBus = {
    new Axi4ToPipelinedMemoryBus(Axi4ToPipelinedMemoryBusConfig(cfg))
  }
  def apply(cfg : Axi4ToPipelinedMemoryBusConfig): Axi4ToPipelinedMemoryBus = {
    new Axi4ToPipelinedMemoryBus(cfg)
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

class Axi4ToPipelinedMemoryBusTestbench extends Component {
  val axiConfig = Axi4Config(
    addressWidth = 32,
    dataWidth    = 64,
    idWidth      = 4,
  )

  val io = new Bundle {
    val axi = slave(Axi4(axiConfig))
  }

  val dataMemory = Memories(MemoryRequirement(Bits(64 bits), num_elements = 1 << 17, numReadWritePorts = 1, numReadPorts = 0, numWritePorts = 0, needsMask = false))

  val axi2pmb = Axi4ToPipelinedMemoryBus(Axi4ToPipelinedMemoryBusConfig(axiConfig, 4, readResponseFifoLatency = 0))
  axi2pmb.io.pmb >> dataMemory.pmbs().head
  axi2pmb.io.axi <> io.axi

  //axi2pmb.formalAssertProperties()
  //HasFormalProperties.printFormalAssertsReport()
}

class Axi4ToPipelinedMemoryBusTester extends AnyFunSuite {
  test("basic") {
    Config.sim.doSim(new Axi4ToPipelinedMemoryBusTestbench()) { dut =>
      SimTimeout(5000 us)

      dut.clockDomain.forkStimulus(100 MHz)

      dut.clockDomain.waitSampling(1)
      Seq(dut.io.axi.aw, dut.io.axi.ar, dut.io.axi.w).foreach(x => {
        x.valid #= false
      })
      Seq(dut.io.axi.r, dut.io.axi.b).foreach(x => {
        x.ready #= false
      })
      dut.clockDomain.waitSampling(10)

      dut.io.axi.w.strb #= 0xff

      for(len <- Seq(32, 1)) {
        for (i <- Array.range(0, 8)) {
          dut.io.axi.aw.addr #= (32 * i) * 8
          dut.io.axi.aw.len #= len - 1
          dut.io.axi.aw.size #= 3
          dut.io.axi.aw.burst #= 1
          dut.io.axi.aw.valid #= true

          dut.clockDomain.waitSamplingWhere(dut.io.axi.aw.ready.toBoolean)
          dut.io.axi.aw.valid #= false

          for (j <- Array.range(0, len)) {
            dut.io.axi.w.valid #= true
            dut.io.axi.w.payload.data #= j + i * 32
            dut.io.axi.w.last #= j == (len - 1)
            dut.clockDomain.waitSamplingWhere(dut.io.axi.w.ready.toBoolean)
          }

          dut.clockDomain.waitSamplingWhere(dut.io.axi.b.valid.toBoolean)
          dut.io.axi.b.ready #= true
          dut.clockDomain.waitSampling()
          dut.io.axi.b.ready #= false
        }
      }

      for(i <- Array.range(0, 8)) {
        dut.io.axi.ar.addr #= (32 * i) * 8
        dut.io.axi.ar.len #= 31
        dut.io.axi.ar.size #= 3
        dut.io.axi.ar.burst #= 1
        dut.io.axi.ar.valid #= true

        dut.clockDomain.waitSamplingWhere(dut.io.axi.ar.ready.toBoolean)
        dut.io.axi.ar.valid #= false

        for(j <- Array.range(0, 32)) {
          dut.io.axi.r.ready #= true
          dut.clockDomain.waitSamplingWhere(dut.io.axi.r.valid.toBoolean)
          println(dut.io.axi.r.payload.data.toBigInt)
        }
      }

      dut.clockDomain.waitSampling(1000)
    }
  }

  test("aw-write-same-time") {
    Config.sim.doSim(new Axi4ToPipelinedMemoryBusTestbench()) { dut =>
      SimTimeout(5000 us)

      dut.clockDomain.forkStimulus(100 MHz)

      Seq(dut.io.axi.aw, dut.io.axi.ar, dut.io.axi.w).foreach(x => {
        x.valid #= false
      })
      Seq(dut.io.axi.r, dut.io.axi.b).foreach(x => {
        x.ready #= false
      })
      dut.clockDomain.waitSampling(10)

      dut.io.axi.w.strb #= 0xff

      var wait_aw, wait_w, wait_b = true
      def check_flags(): Unit = {
        if( dut.io.axi.aw.valid.toBoolean && dut.io.axi.aw.ready.toBoolean) {
          dut.io.axi.aw.valid #= false
          wait_aw = false
        }
        if(dut.io.axi.w.valid.toBoolean && dut.io.axi.w.ready.toBoolean) {
          dut.io.axi.w.valid #= false
          wait_w = false
        }
        if(dut.io.axi.b.valid.toBoolean && dut.io.axi.b.ready.toBoolean) {
          dut.io.axi.b.valid #= false
          wait_b = false
        }
      }

      dut.io.axi.aw.addr #= 4
      dut.io.axi.aw.len #= 0
      dut.io.axi.aw.size #= 3
      dut.io.axi.aw.burst #= 1

      dut.io.axi.w.valid #= true
      dut.io.axi.w.payload.data #= 0x1234
      dut.io.axi.w.last #= true

      dut.io.axi.b.ready #= true
      check_flags()

      dut.clockDomain.waitSampling()
      dut.io.axi.aw.valid #= true

      while(wait_aw || wait_w || wait_b) {
        check_flags()
        dut.clockDomain.waitSamplingWhere(dut.io.axi.aw.ready.toBoolean || dut.io.axi.w.ready.toBoolean || dut.io.axi.b.valid.toBoolean)
      }

      dut.clockDomain.waitSampling(10)
    }
  }
}

class Axi4ToPipelinedMemoryBusFormalTester extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 3
  override def CoverConfig() = formalConfig.withCover(10)

  formalTests().foreach(t => test(t._1) {
    t._2()
  })

  override def generateRtl() = {
    for (
      config <- Seq(Axi4ToPipelinedMemoryBusConfig(Axi4Config(32, 32, idWidth = 1)))
    ) yield {
      (s"${suiteName}", () =>
        GeneralFormalDut(() => new Axi4ToPipelinedMemoryBus(config)))
    }
  }
}


