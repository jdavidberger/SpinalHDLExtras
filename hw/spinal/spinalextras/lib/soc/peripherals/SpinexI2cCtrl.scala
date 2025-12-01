package spinalextras.lib.soc.peripherals


import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.{BusSlaveFactory, BusSlaveFactoryAddressWrapper}
import spinal.lib.com.i2c._
import spinal.lib.fsm._

object I2cOpencoresCompat {

  /**
   * Drop-in replacement for driveI2cSlaveIo that generates an OpenCores I2C compatible register map.
   * This is compatible with the Linux i2c-ocores driver used by SiFive FU540/FU740 SoCs.
   *
   * Compatible string: "sifive,fu540-c000-i2c", "sifive,i2c0"
   *
   * Register Map (OpenCores I2C specification):
   * 0x00 - PRESCALER_LO (R/W) : Clock prescaler low byte
   * 0x04 - PRESCALER_HI (R/W) : Clock prescaler high byte
   * 0x08 - CONTROL      (R/W) : Control register
   *        bit 7: EN  - Core enable
   *        bit 6: IEN - Interrupt enable
   * 0x0C - DATA         (R/W) : Transmit/Receive data
   * 0x10 - CMD/STATUS   (W/R) : Command (write) / Status (read)
   *        Command bits (write):
   *          bit 7: STA  - Generate START condition
   *          bit 6: STO  - Generate STOP condition
   *          bit 5: RD   - Read from slave
   *          bit 4: WR   - Write to slave
   *          bit 3: ACK  - Acknowledge (0 = ACK, 1 = NACK)
   *          bit 0: IACK - Interrupt acknowledge
   *        Status bits (read):
   *          bit 7: RxACK - Received acknowledge from slave
   *          bit 6: BUSY  - Bus busy
   *          bit 5: AL    - Arbitration lost
   *          bit 1: TIP   - Transfer in progress
   *          bit 0: IF    - Interrupt flag
   *
   * Usage: Simply replace the driveI2cSlaveIo call with this function to generate
   * SiFive-compatible I2C hardware that works with the standard Linux i2c-ocores driver.
   */
  def driveI2cSlaveIo(io: I2cSlaveIo, busCtrl: BusSlaveFactory, baseAddress: BigInt)(generics: I2cSlaveMemoryMappedGenerics) = new Area {

    import generics._
    import io._

    val busCtrlWithOffset = new BusSlaveFactoryAddressWrapper(busCtrl, baseAddress)

    // I2C buffer to interface with physical pins
    val i2cBuffer = I2c()
    i2cBuffer <> i2c

    // =========================================================================
    // REGISTER 0x00 - PRESCALER LOW (bits 7:0)
    // =========================================================================
    val prescalerLow = busCtrlWithOffset.createReadAndWrite(Bits(8 bits), address = 0x00, bitOffset = 0) init(0)

    // =========================================================================
    // REGISTER 0x04 - PRESCALER HIGH (bits 15:8)
    // =========================================================================
    val prescalerHigh = busCtrlWithOffset.createReadAndWrite(Bits(8 bits), address = 0x04, bitOffset = 0) init(0)

    // Combined 16-bit prescaler value
    val prescaler = prescalerHigh ## prescalerLow

    // =========================================================================
    // REGISTER 0x08 - CONTROL
    // =========================================================================
    val control = new Area {
      val enable = busCtrlWithOffset.createReadAndWrite(Bool(), address = 0x08, bitOffset = 7) init(False)  // EN
      val intEnable = busCtrlWithOffset.createReadAndWrite(Bool(), address = 0x08, bitOffset = 6) init(False) // IEN
    }

    // =========================================================================
    // REGISTER 0x0C - TRANSMIT/RECEIVE DATA
    // =========================================================================
    val txData = Reg(Bits(8 bits))
    val rxData = Reg(Bits(8 bits))

    busCtrlWithOffset.write(txData, address = 0x0C, bitOffset = 0)
    busCtrlWithOffset.read(rxData, address = 0x0C, bitOffset = 0)

    // =========================================================================
    // REGISTER 0x10 - COMMAND (Write) / STATUS (Read)
    // =========================================================================

    // Command register (write-only)
    val cmd = new Area {
      val cr = busCtrl.createWriteOnly(Bits(8 bits), 0x10, documentation = "CR reg")

      val start = cr(7)  // STA - Generate START
      val stop = cr(6)   // STO - Generate STOP
      val read = cr(5)   // RD  - Read from slave
      val write = cr(4)  // WR  - Write to slave
      val ack = cr(3)    // ACK - Send ACK (0) or NACK (1)
      val iack = cr(0)   // IACK - Interrupt acknowledge
    }

    // Status register (read-only)
    val status = new Area {
      val rxAck = RegInit(False)           // RxACK - Received ACK from slave (bit 7)
      val busy = RegInit(False)            // BUSY  - Bus busy (bit 6)
      val arbLost = RegInit(False)         // AL    - Arbitration lost (bit 5)
      val transferInProgress = RegInit(False) // TIP - Transfer in progress (bit 1)
      val interruptFlag = RegInit(False)   // IF    - Interrupt pending (bit 0)

      busCtrlWithOffset.read(
        rxAck ## busy ## arbLost ## B"00" ## transferInProgress ## interruptFlag,
        address = 0x10,
        bitOffset = 0
      )
    }

    // Clear interrupt flag on IACK
    when(cmd.iack) {
      status.interruptFlag := False
    }

    // =========================================================================
    // STATE MACHINE - I2C Core Logic
    // =========================================================================

    val bitCounter = Reg(UInt(3 bits)) init(0)
    val shiftReg = Reg(Bits(8 bits))
    val receivedAck = Reg(Bool())

    // Connect sampling clock divider from prescaler
    config.samplingClockDivider := prescaler.asUInt

    // I2C bus state tracking
    val frameReset = False

    // FSM states
    val fsm = new StateMachine {
      val IDLE = new State with EntryPoint
      val START = new State
      val ADDRESS = new State
      val DATA = new State
      val ACK = new State
      val STOP = new State

      IDLE.whenIsActive {
        status.busy := False
        status.transferInProgress := False

        when(control.enable) {
          when(cmd.start) {
            goto(START)
          }
        }
      }

      START.whenIsActive {
        status.busy := True
        status.transferInProgress := True

        // Generate START condition via low-level interface
        // Map to I2cSlaveCmdMode.START
        when(internals.inFrame) {
          when(cmd.write) {
            shiftReg := txData
            bitCounter := 0
            goto(ADDRESS)
          } elsewhen(cmd.read) {
            bitCounter := 0
            goto(DATA)
          } elsewhen(cmd.stop) {
            goto(STOP)
          }
        }
      }

      ADDRESS.whenIsActive {
        status.transferInProgress := True

        // Shift out address/data bits
        when(bitCounter === 7) {
          bitCounter := 0
          goto(ACK)
        } otherwise {
          bitCounter := bitCounter + 1
        }
      }

      DATA.whenIsActive {
        status.transferInProgress := True

        // Shift in/out data bits
        when(cmd.read) {
          when(bitCounter === 7) {
            rxData := shiftReg
            bitCounter := 0
            goto(ACK)
          } otherwise {
            bitCounter := bitCounter + 1
          }
        } elsewhen(cmd.write) {
          when(bitCounter === 7) {
            bitCounter := 0
            goto(ACK)
          } otherwise {
            bitCounter := bitCounter + 1
          }
        }
      }

      ACK.whenIsActive {
        status.transferInProgress := True

        // Handle ACK/NACK phase
        status.rxAck := receivedAck
        status.interruptFlag := True

        when(cmd.write) {
          shiftReg := txData
          bitCounter := 0
          goto(DATA)
        } elsewhen(cmd.read) {
          bitCounter := 0
          goto(DATA)
        } elsewhen(cmd.stop) {
          goto(STOP)
        } otherwise {
          goto(IDLE)
        }
      }

      STOP.whenIsActive {
        status.transferInProgress := True

        // Generate STOP condition
        when(!internals.inFrame) {
          status.interruptFlag := True
          goto(IDLE)
        }
      }
    }

    // =========================================================================
    // LOW-LEVEL I2C INTERFACE MAPPING
    // =========================================================================

    // Map OpenCores commands to I2cSlaveIo bus interface
    val inAckState = fsm.isActive(fsm.ACK)

    when(fsm.isActive(fsm.START)) {
      // Assert START condition
      i2cBuffer.scl.write := True
      i2cBuffer.sda.write := False
    } elsewhen(fsm.isActive(fsm.STOP)) {
      // Assert STOP condition
      i2cBuffer.scl.write := True
      i2cBuffer.sda.write := True
    } elsewhen(fsm.isActive(fsm.ADDRESS) || fsm.isActive(fsm.DATA)) {
      // Drive SDA with current bit during address/data phases
      when(!inAckState) {
        val currentBit = shiftReg(7 - bitCounter)
        bus.rsp.valid := True
        bus.rsp.enable := True
        bus.rsp.data := currentBit
      }
    } elsewhen(inAckState) {
      // Release SDA to receive ACK in master mode, or drive ACK in slave mode
      bus.rsp.valid := True
      bus.rsp.enable := cmd.ack  // Drive NACK if cmd.ack is set
      bus.rsp.data := cmd.ack
    } otherwise {
      // Idle - release bus
      bus.rsp.valid := False
      bus.rsp.enable := False
      bus.rsp.data := True
    }

    // Capture received bits
    when(bus.cmd.kind === I2cSlaveCmdMode.READ) {
      when(fsm.isActive(fsm.DATA) || fsm.isActive(fsm.ADDRESS)) {
        shiftReg := shiftReg(6 downto 0) ## bus.cmd.data
      } elsewhen(inAckState) {
        receivedAck := bus.cmd.data
      }
    }

    // Handle START/STOP/DROP events from low-level interface
    when(bus.cmd.kind === I2cSlaveCmdMode.START) {
      frameReset := True
    }
    when(bus.cmd.kind === I2cSlaveCmdMode.STOP) {
      frameReset := True
    }
    when(bus.cmd.kind === I2cSlaveCmdMode.DROP) {
      frameReset := True
      status.arbLost := True
      status.interruptFlag := True
    }

    when(frameReset) {
      bitCounter := 0
    }

    // =========================================================================
    // INTERRUPT OUTPUT
    // =========================================================================

    // Generate interrupt when enabled and flag is set
    val interrupt = status.interruptFlag && control.intEnable

    // Expose status for monitoring
    busCtrlWithOffset.read(
      internals.inFrame ## internals.sdaRead ## internals.sclRead,
      address = 0x14, // Additional status register (optional)
      bitOffset = 0
    )

    // Timeout handling
    val timeoutClear = RegNext(False)
    config.timeoutClear := timeoutClear
    config.timeout := 0xFFFF // Set reasonable default
    config.tsuData := 1 // Setup time

    // Override controls (for debugging/testing)
    val slaveOverride = new Area {
      val sda = busCtrlWithOffset.createReadAndWrite(Bool(), 0x18, 1) init(True)
      val scl = busCtrlWithOffset.createReadAndWrite(Bool(), 0x18, 2) init(True)
      i2cBuffer.sda.write clearWhen(!sda)
      i2cBuffer.scl.write clearWhen(!scl)
    }
  }
}

class SpinexI2cCtrl(config: I2cSlaveGenerics) extends Component {
  val io = new I2cSlaveIo(config)

  def driveI2cSlaveIo(busCtrl: BusSlaveFactory, baseAddress: BigInt = 0, config: I2cSlaveMemoryMappedGenerics) =
    I2cOpencoresCompat.driveI2cSlaveIo(io, busCtrl, baseAddress)(config)

}