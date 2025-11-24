package spinalextras.lib.soc.peripherals


import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.{BusSlaveFactory, BusSlaveFactoryAddressWrapper}
import spinal.lib.com.i2c._
import spinal.lib.fsm._

/**
 * Drop-in replacement for driveI2cSlaveIo that generates OpenCores I2C-compatible
 * registers which work with the Linux i2c-ocores.c driver without any C code changes.
 *
 * Register Map (OpenCores I2C compatible):
 * 0x00 - PRERlo   (Prescaler Low Byte)  - R/W
 * 0x01 - PRERhi   (Prescaler High Byte) - R/W
 * 0x02 - CTR      (Control Register)    - R/W
 * 0x03 - TXR/RXR  (TX Data / RX Data)   - W/R (same address)
 * 0x04 - CR/SR    (Command / Status)    - W/R (same address)
 *
 * Compatible with: sifive,i2c0, opencores,i2c-ocores
 */
object I2cOpencoresCompat {

  def driveI2cSlaveIo(io: I2cSlaveIo, busCtrl: BusSlaveFactory, baseAddress: BigInt)
                     (generics: I2cSlaveMemoryMappedGenerics) = new Area {

    import generics._
    import io._

    val busCtrlWithOffset = new BusSlaveFactoryAddressWrapper(busCtrl, baseAddress)

    // I2C buffer
    val i2cBuffer = I2c()
    i2cBuffer <> io.i2c

    // Prescaler Register (0x00 and 0x01)
    // prescaler = (input_clock_freq / (5 * desired_scl_freq)) - 1
    val prescalerLo = Reg(UInt(8 bits)) init(0xFF)
    val prescalerHi = Reg(UInt(8 bits)) init(0xFF)
    val prescaler = prescalerHi @@ prescalerLo

    busCtrlWithOffset.write(prescalerLo, address = 0x00, bitOffset = 0)
    busCtrlWithOffset.read(prescalerLo,  address = 0x00, bitOffset = 0)

    busCtrlWithOffset.write(prescalerHi, address = 0x01, bitOffset = 0)
    busCtrlWithOffset.read(prescalerHi,  address = 0x01, bitOffset = 0)

    // Control Register (0x02)
    // Bit 7: EN  - Core Enable
    // Bit 6: IEN - Interrupt Enable
    val coreEnable = RegInit(False)
    val interruptEnable = RegInit(False)

    busCtrlWithOffset.read(interruptEnable, address = 0x02, bitOffset = 6)
    busCtrlWithOffset.read(coreEnable,      address = 0x02, bitOffset = 7)
    busCtrlWithOffset.write(interruptEnable, address = 0x02, bitOffset = 6)
    busCtrlWithOffset.write(coreEnable,      address = 0x02, bitOffset = 7)

    // Transmit Register (0x03 write)
    val txData = Reg(Bits(8 bits))
    busCtrlWithOffset.write(txData, address = 0x03, bitOffset = 0)

    // Receive Register (0x03 read)
    val rxData = Reg(Bits(8 bits))
    busCtrlWithOffset.read(rxData, address = 0x03, bitOffset = 0)

    // Command Register (0x04 write)
    // Bit 7: STA  - Generate START condition
    // Bit 6: STO  - Generate STOP condition
    // Bit 5: RD   - Read from slave
    // Bit 4: WR   - Write to slave
    // Bit 3: ACK  - ACK/NACK to be sent (0=ACK, 1=NACK)
    // Bit 0: IACK - Interrupt acknowledge
    val cmdStart = RegNext(False) init(False)
    val cmdStop  = RegNext(False) init(False)
    val cmdRead  = RegNext(False) init(False)
    val cmdWrite = RegNext(False) init(False)
    val cmdAck   = RegNext(True) init(True)  // Default to NACK
    val cmdIack  = RegNext(False) init(False)

    busCtrlWithOffset.write(0x04,
      0 -> cmdIack,
      3 -> cmdAck,
      4 -> cmdWrite,
      5 -> cmdRead,
      6 -> cmdStop,
      7 -> cmdStart
    )

    // Status Register (0x04 read)
    // Bit 7: RxACK - Received ACK from slave (0=ACK, 1=NACK)
    // Bit 6: Busy  - Bus busy
    // Bit 5: AL    - Arbitration lost
    // Bit 1: TIP   - Transfer in progress
    // Bit 0: IF    - Interrupt flag
    val statusRxAck = Reg(Bool()) init(False)
    val statusBusy  = Reg(Bool()) init(False)
    val statusAL    = Reg(Bool()) init(False)
    val statusTIP   = Reg(Bool()) init(False)
    val statusIF    = Reg(Bool()) init(False)

    busCtrlWithOffset.read(
      address = 0x04,
      0 -> statusIF,
      1 -> statusTIP,
      5 -> statusAL,
      6 -> statusBusy,
      7 -> statusRxAck
    )

    // Clear interrupt flag on IACK
    when(cmdIack) {
      statusIF := False
    }

    // Sampling clock divider (derived from prescaler)
    io.config.samplingClockDivider := prescaler.resized
    io.config.timeout := 0  // Disable timeout for compatibility
    io.config.tsuData := 0

    // Clock generation for master mode
    val clockGen = genMaster generate new Area {
      val counter = Reg(UInt(16 bits)) init(0)
      val sclTick = counter === 0

      when(coreEnable && (statusTIP || cmdStart || cmdRead || cmdWrite)) {
        counter := counter - 1
        when(sclTick) {
          counter := prescaler
        }
      } otherwise {
        counter := prescaler
      }
    }

    // Data shift register
    val shiftReg = Reg(Bits(8 bits))
    val bitCounter = Reg(UInt(3 bits)) init(0)
    val inAckState = RegInit(False)

    // FSM for I2C transactions
    val fsm = new StateMachine {
      val IDLE: State = new State with EntryPoint {
        whenIsActive {
          statusTIP := False
          statusBusy := False
          bitCounter := 0
          inAckState := False

          when(cmdStart && coreEnable) {
            statusBusy := True
            statusTIP := True
            goto(START)
          }
        }
      }

      val START: State = new State {
        whenIsActive {
          // Generate START condition
          i2cBuffer.sda.write := False
          i2cBuffer.scl.write := True

          when(cmdWrite) {
            shiftReg := txData
            goto(WRITE_DATA)
          } elsewhen (cmdRead) {
            goto(READ_DATA)
          } otherwise {
            goto(IDLE)
          }
        }
      }

      val WRITE_DATA: State = new State {
        onEntry {
          bitCounter := 0
        }

        whenIsActive {
          // Clock out data bits
          i2cBuffer.sda.write := shiftReg(7)
          i2cBuffer.scl.write := False

          when(genMaster.mux(clockGen.sclTick, True)) {
            i2cBuffer.scl.write := True
            shiftReg := shiftReg(6 downto 0) ## B"0"
            bitCounter := bitCounter + 1

            when(bitCounter === 7) {
              goto(RECV_ACK)
            }
          }
        }
      }

      val READ_DATA: State = new State {
        onEntry {
          bitCounter := 0
        }

        whenIsActive {
          i2cBuffer.sda.write := True  // Release SDA
          i2cBuffer.scl.write := False

          when(genMaster.mux(clockGen.sclTick, True)) {
            i2cBuffer.scl.write := True
            shiftReg := shiftReg(6 downto 0) ## (internals.sdaRead ? B"1" | B"0")
            bitCounter := bitCounter + 1

            when(bitCounter === 7) {
              rxData := shiftReg
              goto(SEND_ACK)
            }
          }
        }
      }

      val RECV_ACK: State = new State {
        whenIsActive {
          i2cBuffer.sda.write := True  // Release SDA
          i2cBuffer.scl.write := False

          when(genMaster.mux(clockGen.sclTick, True)) {
            i2cBuffer.scl.write := True
            statusRxAck := internals.sdaRead  // 0=ACK, 1=NACK
            statusIF := True
            statusTIP := False
            goto(CMD_WAIT)
          }
        }
      }

      val SEND_ACK: State = new State {
        whenIsActive {
          i2cBuffer.sda.write := cmdAck  // Send ACK/NACK
          i2cBuffer.scl.write := False

          when(genMaster.mux(clockGen.sclTick, True)) {
            i2cBuffer.scl.write := True
            statusIF := True
            statusTIP := False
            goto(CMD_WAIT)
          }
        }
      }

      val CMD_WAIT: State = new State {
        whenIsActive {
          when(cmdStop) {
            goto(STOP)
          } elsewhen (cmdStart) {
            goto(START)
          } elsewhen (cmdWrite) {
            shiftReg := txData
            statusTIP := True
            goto(WRITE_DATA)
          } elsewhen (cmdRead) {
            statusTIP := True
            goto(READ_DATA)
          }
        }
      }

      val STOP: State = new State {
        whenIsActive {
          // Generate STOP condition
          i2cBuffer.sda.write := False
          i2cBuffer.scl.write := True

          when(genMaster.mux(clockGen.sclTick, True)) {
            i2cBuffer.sda.write := True
            statusIF := True
            goto(IDLE)
          }
        }
      }
    }

    // Slave mode handling (simplified - monitors bus for slave operations)
    val slaveLogic = new Area {
      when(!Bool(genMaster) || !statusBusy) {
        // Monitor for START condition
        when(bus.cmd.kind === I2cSlaveCmdMode.START) {
          statusBusy := True
          statusIF := True
        }

        // Monitor for data
        when(bus.cmd.kind === I2cSlaveCmdMode.READ) {
          when(!inAckState) {
            rxData := bus.cmd.data ## rxData(7 downto 1)
            bitCounter := bitCounter + 1

            when(bitCounter === 7) {
              inAckState := True
              statusIF := True
            }
          } otherwise {
            statusRxAck := bus.cmd.data
            inAckState := False
            bitCounter := 0
          }
        }

        // Monitor for STOP
        when(bus.cmd.kind === I2cSlaveCmdMode.STOP) {
          statusBusy := False
          statusIF := True
          inAckState := False
        }

        // Provide ACK/data when requested
        when(bus.cmd.kind === I2cSlaveCmdMode.DRIVE) {
          when(inAckState) {
            bus.rsp.valid := True
            bus.rsp.enable := True
            bus.rsp.data := !cmdAck  // Inverted: 0=ACK
          } otherwise {
            bus.rsp.valid := cmdWrite
            bus.rsp.enable := cmdWrite
            bus.rsp.data := shiftReg(7)
            when(bus.rsp.valid) {
              shiftReg := shiftReg(6 downto 0) ## B"0"
            }
          }
        } otherwise {
          bus.rsp.valid := False
          bus.rsp.enable := False
          bus.rsp.data := False
        }
      }
    }

    // Interrupt output
    val interrupt = statusIF && interruptEnable

    // Build FSM
    if(genMaster) {
      fsm.build()
    }
  }
}

class SpinexI2cCtrl(config: I2cSlaveGenerics) extends Component {
  val io = new I2cSlaveIo(config)

  def driveI2cSlaveIo(busCtrl: BusSlaveFactory, baseAddress: BigInt = 0, config: I2cSlaveMemoryMappedGenerics) =
    I2cOpencoresCompat.driveI2cSlaveIo(io, busCtrl, baseAddress)(config)

}