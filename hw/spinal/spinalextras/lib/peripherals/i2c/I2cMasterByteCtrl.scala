package spinalextras.lib.peripherals.i2c

import spinal.core._
import spinal.lib._
import spinal.lib.fsm.{EntryPoint, State, StateMachine}
import spinal.lib.io.TriState

object I2cCmd extends SpinalEnum {
  val START, STOP, READ, WRITE  = newElement()
}

case class I2cMasterByteCtrlCmd() extends Bundle {
  val start  = Bool()
  val stop   = Bool()
  val read   = Bool()
  val write  = Bool()
  val ackIn  = Bool()
  val dataIn = Bits(8 bits)
}

case class I2cMasterByteCtrlRsp() extends Bundle {
  val cmdAck  = Bool()
  val ackOut  = Bool()
  val dataOut = Bits(8 bits)
  val busy    = Bool()
  val al      = Bool()
}

class I2cMasterByteCtrl extends Component {
  val io = new Bundle {
    val enable   = in Bool()
    val clkCnt   = in UInt(16 bits)

    val cmd      = in(I2cMasterByteCtrlCmd())
    val rsp      = out(I2cMasterByteCtrlRsp())

    val scl = master (TriState(Bool()))
    val sda = master (TriState(Bool()))
  }

  // -------------------------------------------------------------------------
  // Bit controller sub-block
  // -------------------------------------------------------------------------

  val bitCtrl = new I2cMasterBitCtrl()
  bitCtrl.io.ena   := io.enable
  bitCtrl.io.clkCnt   := io.clkCnt

  io.rsp.busy := bitCtrl.io.busy
  io.rsp.al := bitCtrl.io.al

  bitCtrl.io.scl <> io.scl
  bitCtrl.io.sda <> io.sda

  val core_cmd = RegInit(I2cCmd.START)
  val core_cmd_valid = RegInit(False) clearWhen(bitCtrl.io.cmd.fire)
  bitCtrl.io.cmd.payload := core_cmd
  bitCtrl.io.cmd.valid := core_cmd_valid

  def pushCmd(cmd : SpinalEnumCraft[I2cCmd.type]): Unit = {
    core_cmd := cmd
    core_cmd_valid := True
  }

  val core_ack = bitCtrl.io.cmd.ready
  val core_txd = RegInit(False)
  bitCtrl.io.din := core_txd

  //---------------------------------------------------------------------------
  // Byte shift register and counter
  //---------------------------------------------------------------------------
  val shreg  = Reg(Bits(8 bits)) init(0)
  val dcnt = Reg(UInt(3 bits)) init(0)
  val cntDone = CombInit(dcnt === 0)

  val loadByte  = RegInit(False)
  val shiftBit  = RegInit(False)

  loadByte := False
  shiftBit := False

  when(loadByte) {
    shreg := io.cmd.dataIn
    dcnt := 7
  } elsewhen(shiftBit) {
    shreg := shreg(6 downto 0) ## bitCtrl.io.dout
    dcnt := dcnt - 1
  }

  io.rsp.dataOut := shreg
  core_txd := shreg(7)

  //---------------------------------------------------------------------------
  // Command/ACK signals
  //---------------------------------------------------------------------------
  val cmdAck = RegInit(False)
  io.rsp.cmdAck := cmdAck
  cmdAck := False

  val ackOut = RegInit(False)
  io.rsp.ackOut := ackOut

  val go = (io.cmd.stop || io.cmd.read || io.cmd.write) && !cmdAck

  //---------------------------------------------------------------------------
  // FSM using spinal.lib.fsm.StateMachine
  //---------------------------------------------------------------------------
  val fsm = new StateMachine {

    val idle  : State = new State with EntryPoint
    val start : State = new State
    val write : State = new State
    val read  : State = new State
    val ack   : State = new State
    val stop  : State = new State

    // -----------------------------------------------------------------------
    // IDLE
    // -----------------------------------------------------------------------
    idle.whenIsActive {
      when(go) {
        when(io.cmd.start) {
          pushCmd(I2cCmd.START)
          goto(start)
        } elsewhen(io.cmd.read) {
          pushCmd(I2cCmd.READ)
          goto(read)
        } elsewhen(io.cmd.write) {
          pushCmd(I2cCmd.WRITE)
          goto(write)
        } otherwise {
          pushCmd(I2cCmd.STOP)
          goto(stop)
        }

        loadByte := True
      }
    }

    // -----------------------------------------------------------------------
    // START
    // -----------------------------------------------------------------------
    start.whenIsActive {
      when(core_ack) {
        loadByte := True
        when(io.cmd.read) {
          pushCmd(I2cCmd.READ)
          goto(read)
        } otherwise {
          pushCmd(I2cCmd.WRITE)
          goto(write)
        }
      }
    }

    // -----------------------------------------------------------------------
    // WRITE BYTE
    // -----------------------------------------------------------------------
    write.whenIsActive {
      when(core_ack) {
        when(cntDone) {
          // Go read the ACK bit from the slave
          pushCmd(I2cCmd.READ)
          goto(ack)
        } otherwise {
          pushCmd(I2cCmd.WRITE)
          shiftBit := True
          goto(write)
        }
      }
    }

    // -----------------------------------------------------------------------
    // READ BYTE
    // -----------------------------------------------------------------------
    read.whenIsActive {
      when(core_ack) {
        when(cntDone) {
          // Now drive our ACK/NACK bit
          pushCmd(I2cCmd.WRITE)
          goto(ack)
        } otherwise {
          pushCmd(I2cCmd.READ)
          goto(read)
        }

        shiftBit := True
        core_txd := io.cmd.ackIn
      }
    }

    // -----------------------------------------------------------------------
    // ACK state (after READ or WRITE)
    // -----------------------------------------------------------------------
    ack.whenIsActive {
      when(core_ack) {
        when(io.cmd.stop) {
          pushCmd(I2cCmd.STOP)
          goto(stop)
        } otherwise {
          cmdAck := True
          //assert(core_cmd_valid === False, "Assert cmd valid should not be true here")
          goto(idle)
        }

        ackOut := bitCtrl.io.dout
        core_txd := True
      } otherwise {
        core_txd := io.cmd.ackIn
      }
    }

    // -----------------------------------------------------------------------
    // STOP CONDITION
    // -----------------------------------------------------------------------
    stop.whenIsActive {
      when(core_ack) {
        cmdAck := True
        goto(idle)
      }
    }
  }

  fsm.build()
  when(bitCtrl.io.al) {
    fsm.goto(fsm.idle)
    core_cmd_valid := False
    loadByte := False
    cmdAck := False
    ackOut := False
    shiftBit := False
  }
}