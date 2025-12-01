package spinalextras.lib.peripherals.i2c

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.io.TriState

class I2cMasterBitCtrl extends Component {
  val io = new Bundle {
    val ena = in Bool()
    val clkCnt = in UInt (16 bits)

    val cmd = slave Stream(I2cCmd())

    val busy = out Bool()
    val al = out Bool()

    val din = in Bool()
    val dout = out Bool()

    val scl = master(TriState(Bool()))
    val sda = master(TriState(Bool()))
  }

  val scl_i = io.scl.read
  val scl_o = io.scl.write
  val scl_oen = Bool()
  io.scl.writeEnable := !scl_oen

  val sda_i = io.sda.read
  val sda_o = io.sda.write
  val sda_oen = Bool()
  io.sda.writeEnable := !sda_oen

  val cSCL = RegInit(U(0, 2 bits)) // 2-sample capture
  val cSDA = RegInit(U(0, 2 bits))
  val fSCL = RegInit(U(0x7, 3 bits)) // filter shift reg
  val fSDA = RegInit(U(0x7, 3 bits))
  val sSCL = RegInit(True) // filtered/synchronized SCL
  val sSDA = RegInit(True) // filtered/synchronized SDA

  val sda_chk = RegInit(False)

  val cnt = RegInit(U(0, 16 bits))
  val filterCnt = RegInit(U(0, 14 bits))

  val cmdAckReg = RegInit(False)

  val alReg = RegInit(False)
  val doutReg = RegInit(False)

  val sclOenReg = RegInit(True) // note: 1 means released (open-drain)
  val sdaOenReg = RegInit(True)

  // expose outputs
  io.cmd.ready := cmdAckReg

  io.al := alReg
  io.dout := doutReg

  // scl_o and sda_o are tied low (open-drain)
  scl_o := False
  sda_o := False

  scl_oen := sclOenReg
  sda_oen := sdaOenReg

  // --- small helpers ---
  val sclSync = Bool()

  // --- dscl_oen delay (one cycle) to detect slave pulling SCL low ---
  val dscl_oen = RegNext(sclOenReg, False)

  // --- slave_wait: remains set while slave holds SCL low ---
  val slaveWait = RegInit(False)
  slaveWait := ((sclOenReg && !dscl_oen && !sSCL) || (slaveWait && !sSCL))

  // --- clock divider: generate clkEn exactly like original ---

  val clkEn = RegInit(True)
  when(cnt === 0 || !io.ena || sclSync) {
    cnt := io.clkCnt
    clkEn := True
  } elsewhen (slaveWait) {
    // hold counter, do not assert clkEn
    clkEn := False
  } otherwise {
    cnt := cnt - 1
    clkEn := False
  }

  // If user asserts reset externally, use RegInit to clear cnt/clkEn on reset
  // (RegInit already handled)

  // --- capture SCL/SDA (metastability reduction) ---
  cSCL := (cSCL(0) ## scl_i).asUInt
  cSDA := (cSDA(0) ## sda_i).asUInt

  // --- filter counter --- (16x bus frequency window in original)
  when(!io.ena) {
    filterCnt := 0
  } elsewhen(filterCnt === 0) {
    // avoid shift by zero; mimic original: filterCnt := clk_cnt >> 2
    filterCnt := (io.clkCnt >> 2).resized
  } otherwise {
    filterCnt := filterCnt - 1
  }

  // --- filter shift registers update on filter boundary ---
  when(filterCnt === 0) {
    fSCL := (fSCL(1 downto 0) ## cSCL(1)).asUInt
    fSDA := (fSDA(1 downto 0) ## cSDA(1)).asUInt
  }

  // --- produce filtered signals (approximate original boolean formula) ---
  // original: sSCL <= &fSCL[2:1] | &fSCL[1:0] | (fSCL[2] & fSCL[0]);
  val all21SCL = fSCL(2) & fSCL(1)
  val all10SCL = fSCL(1) & fSCL(0)
  val edgeCaseSCL = fSCL(2) & fSCL(0)
  sSCL := (all21SCL || all10SCL || edgeCaseSCL)

  val all21SDA = fSDA(2) & fSDA(1)
  val all10SDA = fSDA(1) & fSDA(0)
  val edgeCaseSDA = fSDA(2) & fSDA(0)
  sSDA := (all21SDA || all10SDA || edgeCaseSDA)

  // delayed versions
  val dSCL = RegNext(sSCL, True)
  val dSDA = RegNext(sSDA, True)

  sclSync := dSCL && !sSCL && sclOenReg // master wants to drive high but bus low

  // --- start / stop detection (fall/rise on SDA while SCL high) ---
  val startCondition = RegNext(!sSDA && dSDA && sSCL, init = False)
  val stopCondition = RegNext(sSDA && !dSDA && sSCL, init = False)

  // busy: set on start, clear on stop
  val busyReg = RegInit(False)
  io.busy := busyReg
  busyReg := (startCondition || busyReg) && !stopCondition

  // --- cmd_stop tracking (original: sampled on clk_en) ---
  val cmdStopReg = RegInit(False)
  when(clkEn) {
    cmdStopReg := io.cmd.valid && (io.cmd.payload === I2cCmd.STOP)
  }

  // --- dout: sample SDA on rising edge of SCL ---
  when(sSCL && !dSCL) {
    doutReg := sSDA
  }

  // --- arbitration lost (AL) ---
  // original: al <= (sda_chk & ~sSDA & sda_oen) | (|c_state & sto_condition & ~cmd_stop);
  // we'll compute the state-active term later in the FSM (stateActive)
  val stateActive = CombInit(True)
  alReg := (sda_chk && !sSDA && sdaOenReg) || (stateActive && stopCondition && !cmdStopReg)

  // --- prepare for StateMachine actions ---
  cmdAckReg := False // default pulse behavior; set true where appropriate
  sda_chk := sda_chk // will be updated in FSM
  // sclOenReg and sdaOenReg are also modified in FSM states

  when(io.al) {
    sclOenReg := True
    sdaOenReg := True
    sda_chk := False
    io.cmd.ready := True
  }

  // --- FSM: Sequence through microstates for each command ---
  val fsm = new StateMachine {

    val idle = new State with EntryPoint

    def create_transition(n_scl_oen : Bool, n_sda_oen: Bool, n_sda_chk : Bool, next : => State) : State = new State {
      whenIsActive {
        when(io.al) {
          goto(idle)
        } elsewhen(clkEn) {
          sclOenReg := n_scl_oen
          sdaOenReg := n_sda_oen
          sda_chk := n_sda_chk
          goto(next)
        }
      }
    }
    // START sequence: maps start_a..start_e
    val startA = create_transition(
      scl_oen,
      True, // release SDA (1 = tri-state)
      False,
      startB
    )

    val startB = create_transition(
      True,  // set SCL high (release)
      True, // keep SDA tri-stated
      False,
      startC
    )

    val  startC = create_transition(
      True,
      False, // drive SDA low
      False,
      startD
    )

    val startD = create_transition(
      True,
      False, // drive SDA low
      False,
      startE
    )

    val  startE = create_transition(
      False,
      False, // drive SDA low
      False,
      idle
    ).onExit {
      cmdAckReg := True
    }

    // STOP sequence
    val  stopA = create_transition(
      n_scl_oen = False,
      n_sda_oen = False,
      n_sda_chk = False,
      stopB
    )

    val  stopB = create_transition(
      n_scl_oen = True,
      n_sda_oen = False,
      n_sda_chk = False,
      stopC
    )

    val  stopC = create_transition(
      // keep SCL high, prepare to release SDA
      n_scl_oen = True,
      n_sda_oen = False,
      n_sda_chk = False,
      stopD
    )

    val  stopD = create_transition(
      // keep SCL high, prepare to release SDA
      n_scl_oen = True,
      n_sda_oen = True,
      n_sda_chk = False,
      idle
    ).onExit {
      cmdAckReg := True
    }

    // READ sequence (rd_a..rd_d)
    val  rdA = create_transition(
      n_scl_oen = False,
      n_sda_oen = True,
      n_sda_chk = False,
      rdB
    )

    val  rdB = create_transition(
      n_scl_oen = True,
      n_sda_oen = True,
      n_sda_chk = False,
      rdC
    )

    val  rdC = create_transition(
      n_scl_oen = True,
      n_sda_oen = True,
      n_sda_chk = False,
      rdD
    )

    val  rdD = create_transition(
      n_scl_oen = False,
      n_sda_oen = True,
      n_sda_chk = False,
      idle
    ).onExit {
      cmdAckReg := True
    }

    // WRITE sequence (wr_a..wr_d)
    val  wrA = create_transition(
      n_scl_oen = False,
      n_sda_oen = io.din,
      n_sda_chk = False,
      wrB
    )

    val  wrB = create_transition(
      n_scl_oen = True,
      n_sda_oen = io.din,
      n_sda_chk = False,
      wrC
    )

    val  wrC = create_transition(
      n_scl_oen = True,
      n_sda_oen = io.din,
      n_sda_chk = True,
      wrD
    )

    val  wrD = create_transition(
      n_scl_oen = False,
      n_sda_oen = io.din,
      n_sda_chk = False,
      idle
    ).onExit {
      cmdAckReg := True
    }


    // Default outputs each cycle (unless overridden in the state's whenIsActive)
    // We already set default cmdAck false outside; ensure default sda_chk cleared unless set by state
    idle.whenIsActive {
      stateActive := False
      // On idle we wait for cmd; nothing changes unless clkEn and a valid command
      when(clkEn && !io.al) {
        sda_chk := False

        when(io.cmd.valid) {
          switch(io.cmd.payload) {
            is(I2cCmd.START) {
              goto(startA)
            }
            is(I2cCmd.STOP) {
              goto(stopA)
            }
            is(I2cCmd.WRITE) {
              goto(wrA)
            }
            is(I2cCmd.READ) {
              goto(rdA)
            }
          }
        }
      }
    }

  } // end FSM
}
