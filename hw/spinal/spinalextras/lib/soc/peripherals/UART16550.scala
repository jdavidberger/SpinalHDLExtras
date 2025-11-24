package spinalextras.lib.soc.peripherals

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._
import spinal.lib.com.uart._

/**
 * 16550-Compatible UART Controller Extension for SpinalHDL
 *
 * This provides a drop-in replacement for the standard UartCtrl.driveFrom() function
 * that implements 16550-compatible register mappings instead of the custom SpinalHDL mappings.
 *
 * Reference: PC16550D Universal Asynchronous Receiver/Transmitter with FIFOs
 * Datasheet: https://www.scs.stanford.edu/10wi-cs140/pintos/specs/pc16550d.pdf
 *
 * REGISTER MAPPING COMPARISON:
 *
 * SpinalHDL Standard Mapping:           16550 Compatible Mapping:
 * -------------------------             -------------------------
 * 0x00: Data (R/W)                      0x00: RBR/THR/DLL (R/W, DLAB-dependent)
 * 0x04: Status/Control                  0x01: IER/DLM (R/W, DLAB-dependent)
 * 0x06: Clock/Occupancy (16-bit)        0x02: IIR/FCR (R/W)
 * 0x08: Clock Divider                   0x03: LCR (R/W)
 * 0x0C: Frame Config                    0x04: MCR (R/W)
 * 0x10: Misc/Errors                     0x05: LSR (R)
 *                                       0x06: MSR (R)
 *                                       0x07: SCR (R/W)
 *
 * KEY FEATURES:
 * - Standard 16550 register offsets (0-7)
 * - DLAB bit controls access to divisor latches vs data/interrupt registers
 * - Compatible interrupt identification register (IIR)
 * - FIFO control register (FCR) with trigger levels
 * - Line status register (LSR) with standard error flags
 * - Modem control/status registers (MCR/MSR) for compatibility
 */

case class Uart16550Config(
                            txFifoDepth: Int = 16,
                            rxFifoDepth: Int = 16,
                            initConfig: UartCtrlInitConfig = null
                          )

object Uart16550Ctrl {
  /**
   * Drive a UartCtrl from a bus using 16550-compatible register mappings.
   * This is a drop-in replacement for UartCtrl.driveFrom() that provides
   * compatibility with standard 16550 UART drivers.
   *
   * @param uart The UartCtrl instance to drive
   * @param busCtrl The bus slave factory for register access
   * @param config Configuration for FIFO depths and initialization
   * @param baseAddress Base address offset for the UART registers
   * @return An Area containing all the register logic
   */
  def driveFrom16550(uart: UartCtrlHighRate,
                     busCtrl: BusSlaveFactory,
                     config: Uart16550Config,
                     baseAddress: Int = 0) = new Area {

    val crossClockUart = uart.clockDomain != ClockDomain.current
    val busCtrlWrapped = new BusSlaveFactoryAddressWrapper(busCtrl, baseAddress)
    val g = uart.io.config.g

    // ========================================================================
    // 16550 REGISTER SET
    // ========================================================================

    // Internal configuration register
    val uartConfigReg = Reg(uart.io.config)
    uartConfigReg.clockDivider init(0)
    if(config.initConfig != null) config.initConfig.initReg(uartConfigReg)

    if(crossClockUart) {
      val configStream = Stream(uartConfigReg)
      configStream.valid := True
      configStream.payload := uartConfigReg

      val uartConfig = configStream.ccToggle(ClockDomain.current, uart.clockDomain)
      uart.clockDomain on {
        val uartConfigReg = Reg(uart.io.config)
        uartConfigReg.clockDivider init(0)
        if(config.initConfig != null) config.initConfig.initReg(uartConfigReg)

        uartConfig.ready := True
        when(uartConfig.valid) {
          uartConfigReg := uartConfig.payload
        }
        uart.io.config := uartConfigReg
      }
    } else {
      uart.io.config := uartConfigReg
    }


    // Divisor Latch Registers (DLL/DLM) - accessible when DLAB=1
    val dll = Reg(Bits(8 bits)) init(0)
    val dlm = Reg(Bits(8 bits)) init(0)

    // IER - Interrupt Enable Register (Offset 0x01, DLAB=0)
    // Bit 0: ERBFI - Enable Received Data Available Interrupt
    // Bit 1: ETBEI - Enable Transmitter Holding Register Empty Interrupt
    // Bit 2: ELSI  - Enable Receiver Line Status Interrupt
    // Bit 3: EDSSI - Enable Modem Status Interrupt
    // Bits 4-7: Reserved (always 0)
    val ier = Reg(Bits(8 bits)) init(0)

    // FCR - FIFO Control Register (Offset 0x02, Write Only)
    // Bit 0: FIFO Enable (1=Enable FIFOs, 0=16450 mode)
    // Bit 1: RCVR FIFO Reset (self-clearing)
    // Bit 2: XMIT FIFO Reset (self-clearing)
    // Bit 3: DMA Mode Select
    // Bits 4-5: Reserved
    // Bits 6-7: RCVR Trigger (00=1, 01=4, 10=8, 11=14 bytes)
    val fcrFifoEnable = Reg(Bool()) init(False)
    val fcrDmaMode = Reg(Bool()) init(False)
    val fcrRxTrigger = Reg(UInt(2 bits)) init(0)
    val fcrRxFifoReset = Bool()
    val fcrTxFifoReset = Bool()

    // LCR - Line Control Register (Offset 0x03)
    // Bits 0-1: Word Length (00=5, 01=6, 10=7, 11=8 bits)
    // Bit 2: Stop Bits (0=1, 1=1.5/2)
    // Bit 3: Parity Enable
    // Bit 4: Even Parity Select
    // Bit 5: Stick Parity
    // Bit 6: Set Break
    // Bit 7: DLAB - Divisor Latch Access Bit
    val lcr = Reg(Bits(8 bits)) init(0)

    val dlab = lcr(7) // Divisor Latch Access Bit


    // MCR - Modem Control Register (Offset 0x04)
    // Bit 0: DTR - Data Terminal Ready
    // Bit 1: RTS - Request to Send
    // Bit 2: OUT1
    // Bit 3: OUT2 (often used as interrupt enable)
    // Bit 4: Loop - Loopback mode
    // Bits 5-7: Reserved (always 0)
    val mcr = Reg(Bits(8 bits)) init(0)

    // MSR - Modem Status Register (Offset 0x06)
    // Bits 0-3: Delta bits (CTS, DSR, RI, DCD changed)
    // Bits 4-7: Current state of modem signals
    val msrDeltaBits = Reg(Bits(4 bits)) init(0)

    // SCR - Scratch Register (Offset 0x07)
    // 8-bit scratchpad for software use, no hardware function
    val scr = Reg(Bits(8 bits)) init(0)

    // Apply LCR configuration to UART
    uartConfigReg.frame.dataLength := lcr(1 downto 0).asUInt + 4 // dataLength has an offset of 1
    uartConfigReg.frame.stop := lcr(2).mux(
      UartStopType.TWO,
      UartStopType.ONE
    )

    val lut = Vec(
      UartParityType.NONE.asBits,
      UartParityType.NONE.asBits, // Invalid, default to NONE
      UartParityType.ODD.asBits,
      UartParityType.EVEN.asBits
    )

    uartConfigReg.frame.parity.assignFromBits(lut((lcr(3) ## lcr(4)).asUInt))

    // 16550 is configured as a 16x sample clock
    uart.g.rxSamplePerBit match {
      case 8 => uartConfigReg.clockDivider := ((dlm ## dll) << 1).asUInt.resized
      case 16 => uartConfigReg.clockDivider := ((dlm ## dll)).asUInt.resized
      case _ => throw new Exception("RX sample per bit must be 8 or 16")
    }


    // Break control
    uart.io.writeBreak := bufferToUartClock(lcr(6))

    // ========================================================================
    // TX PATH (Transmitter)
    // ========================================================================
    val write = new Area {
      // Create transmit stream with FIFO
      val streamUnbuffered = Stream(Bits(8 bits))
      val (stream, fifoOccupancy) = streamUnbuffered.queueWithPushOccupancy(config.txFifoDepth, ClockDomain.current, uart.clockDomain)
      uart.io.write << stream

      // THR write is triggered by bus write to address 0 when DLAB=0
      // We'll handle this in the register mapping section below
      streamUnbuffered.setIdle()
    }

    def bufferFromUartClock(x : Bool) = {
      if(crossClockUart) BufferCC(x) else x
    }
    def bufferToUartClock(x : Bool) = {
      if(crossClockUart) uart.clockDomain on { BufferCC(x) } else x
    }
    // ========================================================================
    // RX PATH (Receiver)
    // ========================================================================
    val read = new Area {
      val readIrqTimeout = Counter(4 * 16)

      val readFifo = new StreamFifoCC(uart.io.read.payload, config.rxFifoDepth, uart.clockDomain, ClockDomain.current)
      uart.io.read.throwWhen(uart.io.readBreak) >> readFifo.io.push
      val stream = readFifo.io.pop
      val fifoOccupancy = readFifo.io.popOccupancy

      val streamBreaked = stream.throwWhen(fcrRxFifoReset)
      streamBreaked.ready := False

      when(bufferFromUartClock(uart.io.read.fire) || streamBreaked.fire) {
        readIrqTimeout.clear()
      }

      val tick = bufferFromUartClock(uart.tick)
      when(!readIrqTimeout.willOverflowIfInc && tick) {
        readIrqTimeout.increment()
      }

      // Calculate trigger level for interrupts based on FCR bits 6-7
      val triggerLevel = fcrRxTrigger.mux(
        U(0) -> U(1),
        U(1) -> U(4),
        U(2) -> U(8),
        U(3) -> U(14)
      )

      val triggerReached = fifoOccupancy >= triggerLevel || (readIrqTimeout.willOverflowIfInc && streamBreaked.valid)
    }

    // ========================================================================
    // LSR - Line Status Register (Offset 0x05, Read Only)
    // ========================================================================
    // Bit 0: DR - Data Ready
    // Bit 1: OE - Overrun Error
    // Bit 2: PE - Parity Error
    // Bit 3: FE - Framing Error
    // Bit 4: BI - Break Interrupt
    // Bit 5: THRE - Transmitter Holding Register Empty
    // Bit 6: TEMT - Transmitter Empty
    // Bit 7: Error in RCVR FIFO
    val lsr = Bits(8 bits)
    lsr(0) := read.fifoOccupancy > 0                    // Data Ready
    lsr(1) := bufferFromUartClock(uart.io.read.isStall)                      // Overrun Error
    lsr(2) := bufferFromUartClock(uart.io.readError)                         // Parity Error (simplified)
    lsr(3) := False                                     // Framing Error (simplified)
    lsr(4) := bufferFromUartClock(uart.io.readBreak)                         // Break Interrupt
    lsr(5) := write.fifoOccupancy === 0                 // THRE
    lsr(6) := write.fifoOccupancy === 0 && bufferFromUartClock(!uart.io.write.valid) // TEMT
    lsr(7) := bufferFromUartClock(uart.io.readError || uart.io.read.isStall) // Error in RCVR FIFO

    // ========================================================================
    // IIR - Interrupt Identification Register (Offset 0x02, Read Only)
    // ========================================================================
    // Bit 0: 0=Interrupt Pending, 1=No Interrupt
    // Bits 1-2: Interrupt ID:
    //   00 = Modem Status
    //   01 = Transmitter Holding Register Empty
    //   10 = Received Data Available
    //   11 = Receiver Line Status
    // Bit 3: Timeout Interrupt (FIFO mode only)
    // Bits 4-5: Reserved (0)
    // Bits 6-7: FIFOs Enabled (11 when FCR bit 0 = 1)
    val interruptCtrl = new Area {
      // Individual interrupt sources
      val rxDataAvailable = ier(0) && (fcrFifoEnable ? read.triggerReached | read.streamBreaked.valid)
      val txHoldingEmpty = ier(1) && (write.fifoOccupancy === 0)
      val lineStatus = ier(2) && bufferFromUartClock(uart.io.readError || uart.io.read.isStall || uart.io.readBreak)
      val modemStatus = ier(3) && msrDeltaBits.orR

      // Priority encoding (highest to lowest):
      // 1. Line Status (11)
      // 2. RX Data Available (10)
      // 3. TX Holding Empty (01)
      // 4. Modem Status (00)
      val iir = Bits(8 bits)
      when(lineStatus) {
        iir := B"8'b00000110" // Priority 1: Line Status
      } elsewhen(rxDataAvailable) {
        iir := B"8'b00000100" // Priority 2: RX Data Available
      } elsewhen(txHoldingEmpty) {
        iir := B"8'b00000010" // Priority 3: TX Holding Empty
      } elsewhen(modemStatus) {
        iir := B"8'b00000000" // Priority 4: Modem Status
      } otherwise {
        iir := B"8'b00000001" // No interrupt pending
      }

      // Set FIFO enable bits when FCR0=1
      when(fcrFifoEnable) {
        iir(7 downto 6) := B"2'b11"
      }

      // Interrupt output
      val interrupt = lineStatus || rxDataAvailable || txHoldingEmpty || modemStatus
    }

    // ========================================================================
    // MSR - Modem Status Register (Offset 0x06, Read Only)
    // ========================================================================
    // Note: This is simplified - full implementation would need
    // edge detection for CTS/DSR/RI/DCD inputs
    val msr = Bits(8 bits)
    msr(3 downto 0) := msrDeltaBits
    msr(7 downto 4) := B"4'b0000" // Current modem signal states (simplified)

    // ========================================================================
    // BUS REGISTER INTERFACE
    // ========================================================================

    val wordSize = busCtrlWrapped.busDataWidth / 8

    // Offset 0: RBR (Read, DLAB=0) / THR (Write, DLAB=0) / DLL (R/W, DLAB=1)
    val busDll = cloneOf(dll)
    busCtrlWrapped.readAndWrite(busDll, 0)
    when(dlab) {
      busDll := dll.resized
    } otherwise {
      busDll := read.streamBreaked.payload.resized
    }


    busCtrlWrapped.onRead(0) {
      read.streamBreaked.ready := !dlab
    }

    busCtrlWrapped.onWrite(0) {
      when(dlab) {
        dll := busDll
      } otherwise {
        // Write to THR (Transmit Holding Register)
        write.streamUnbuffered.valid := !fcrTxFifoReset
        write.streamUnbuffered.payload := busDll
      }
    }

    // Offset 1: IER (R/W, DLAB=0) / DLM (R/W, DLAB=1)
    val reg1Flow = busCtrl.createAndDriveFlow(Bits(8 bits), 1 * wordSize)
    when(reg1Flow.valid) {
      when(dlab) {
        dlm := reg1Flow.payload
      } otherwise {
        ier := reg1Flow.payload & B"8'h0F" // Only bits 0-3 writable
      }
    }
    busCtrlWrapped.read(dlab.mux(dlm, ier), 1* wordSize)

    // Offset 2: IIR (Read Only) / FCR (Write Only)
    fcrRxFifoReset := False
    fcrTxFifoReset := False

    busCtrlWrapped.write(2* wordSize,
      0 -> fcrFifoEnable,
      1 -> fcrRxFifoReset,
      2 -> fcrTxFifoReset,
      3 -> fcrDmaMode,
      7 -> fcrRxTrigger
    )

    busCtrlWrapped.read(interruptCtrl.iir, 2* wordSize)

    // Offset 3: LCR (R/W)
    busCtrlWrapped.readAndWrite(lcr, 3* wordSize)

    // Offset 4: MCR (R/W)
    busCtrlWrapped.readAndWrite(mcr, 4* wordSize)

    // Offset 5: LSR (Read Only)
    busCtrlWrapped.read(lsr, 5* wordSize)

    // Offset 6: MSR (Read Only, clears delta bits on read)
    busCtrlWrapped.read(msr, 6* wordSize)
    busCtrlWrapped.onRead(6* wordSize) {
      msrDeltaBits := 0
    }

    // Offset 7: SCR (R/W)
    busCtrlWrapped.readAndWrite(scr, 7* wordSize)
  }
}

/**
 * USAGE EXAMPLE:
 *
 * Replace this:
 *   uartCtrl.driveFrom(busCtrl, config, baseAddress)
 *
 * With this:
 *   Uart16550Ctrl.driveFrom16550(uartCtrl, busCtrl,
 *     Uart16550Config(
 *       txFifoDepth = 16,
 *       rxFifoDepth = 16,
 *       initConfig = UartCtrlInitConfig(
 *         baudrate = 115200,
 *         dataLength = 7,  // 7 = 8 bits
 *         parity = UartParityType.NONE,
 *         stop = UartStopType.ONE
 *       )
 *     ),
 *     baseAddress
 *   )
 *
 * The UART will now be accessible using standard 16550 register offsets
 * and will be compatible with existing 16550 device drivers.
 */

/**
 * KEY BEHAVIORAL DIFFERENCES FROM PC16550D:
 *
 * 1. Modem Control Signals:
 *    - MSR delta bits (DCTS, DDSR, TERI, DDCD) require additional edge
 *      detection logic not fully implemented here
 *    - Loopback mode (MCR bit 4) may not be fully functional
 *
 * 2. Timeout Interrupt:
 *    - The PC16550D generates timeout after 4 character times with no
 *      FIFO activity. This implementation may use simplified timing.
 *
 * 3. FIFO Depth:
 *    - Configurable vs fixed 16 bytes in PC16550D
 *    - Trigger levels may need adjustment if depth differs from 16
 *
 * 4. Error Handling:
 *    - Simplified parity/framing error detection
 *    - Error flags may not be associated with specific FIFO bytes
 *
 * 5. DMA Signals:
 *    - TXRDY/RXRDY pins not implemented
 *    - DMA mode select (FCR bit 3) has no effect
 *
 * Despite these differences, the register interface is fully compatible
 * with the 16550 specification for standard UART operations.
 */

class UartCtrlHighRate(val g : UartCtrlGenerics = UartCtrlGenerics()) extends Component {
  val io = new UartCtrlIo(g)
  val tick = out Bool()
  val tx = new UartCtrlTx(g)
  val rx = new UartCtrlRx(g)

  val busClock = ClockDomain.current

  //Clock divider used by RX and TX
  val clockDivider = new Area {
    val counter = Reg(UInt(g.clockDividerWidth bits)) init(0)
    val tick = counter === 0
    val tickReg = RegNext(tick) init(False)

    counter := counter - 1
    when(tick) {
      counter := io.config.clockDivider
    }
  }

  tick := clockDivider.tickReg

  tx.io.samplingTick := tick
  rx.io.samplingTick := tick

  tx.io.configFrame := io.config.frame
  rx.io.configFrame := io.config.frame

  tx.io.write << io.write.throwWhen(rx.io.break)
  rx.io.read >> io.read

  io.uart.txd <> tx.io.txd
  io.uart.rxd <> rx.io.rxd

  io.readError := rx.io.error
  tx.io.cts := (if (g.ctsGen) BufferCC.withTag(io.uart.cts) else False)
  if (g.rtsGen) io.uart.rts := rx.io.rts
  io.readBreak := rx.io.break
  tx.io.break := io.writeBreak
}
