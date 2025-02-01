package spinalextras.lib.soc.peripherals

import spinal.core.{Area, Bits, Bundle, Component, False, IntToBuilder, Reg, RegNext, UInt, out, widthOf}
import spinal.lib.bus.amba3.apb.{Apb3, Apb3SlaveFactory}
import spinal.lib.bus.misc.{BusSlaveFactory, BusSlaveFactoryAddressWrapper}
import spinal.lib.com.uart.{Apb3UartCtrl, Uart, UartCtrl, UartCtrlMemoryMappedConfig}
import spinal.lib.{StreamFifo, master, slave}
import spinalextras.lib.soc.{CSREventManager, EventSourceProcess}

case class SpinexApb3UartCtrl(config: UartCtrlMemoryMappedConfig) extends Component {
  val io = new Bundle {
    val apb = slave(Apb3(Apb3UartCtrl.getApb3Config))
    val uart = master(Uart(ctsGen = config.uartCtrlConfig.ctsGen, rtsGen = config.uartCtrlConfig.rtsGen))
    val interrupt = out Bool()
  }

  val uartCtrl = new UartCtrl(config.uartCtrlConfig)
  io.uart <> uartCtrl.io.uart

  val busCtrl = Apb3SlaveFactory(io.apb)
  val bridge = driveFrom(busCtrl, config)
  io.interrupt := bridge.interruptCtrl.interrupt

  def driveFrom(busCtrl: BusSlaveFactory, config: UartCtrlMemoryMappedConfig, baseAddress: Int = 0) = new Area {
    val io = uartCtrl.io
    val g = config.uartCtrlConfig

    require(busCtrl.busDataWidth == 16 || busCtrl.busDataWidth == 32)
    val busCtrlWrapped = new BusSlaveFactoryAddressWrapper(busCtrl, baseAddress)
    //Manage config
    val uartConfigReg = Reg(io.config)
    uartConfigReg.clockDivider init (0)
    if (config.initConfig != null) config.initConfig.initReg(uartConfigReg)

    require(!config.busCanWriteClockDividerConfig)
    require(!config.busCanWriteFrameConfig)
    uartConfigReg.clockDivider.allowUnsetRegToAvoidLatch
    uartConfigReg.frame.allowUnsetRegToAvoidLatch

    io.config := uartConfigReg

    def sat255(that: UInt) = if (widthOf(that) > 8) that.min(255).resize(8 bits) else that

    //manage TX
    val write = new Area {
      val streamUnbuffered = busCtrlWrapped.createAndDriveFlow(Bits(g.dataWidthMax bits), address = 0).toStream
      val (stream, fifoOccupancy) = streamUnbuffered.queueWithOccupancy(config.txFifoDepth)
      io.write << stream

      //busCtrlWrapped.read(sat255(config.txFifoDepth - fifoOccupancy),address = 4,bitOffset = 16)
      //streamUnbuffered.ready.allowPruning()
      //busCtrlWrapped.read(stream.valid, address = 4, 15)

      val txfull = !streamUnbuffered.ready
      val txempty = !stream.valid
      val nonFull = !txfull

      busCtrlWrapped.read(txfull, address = 4) // TXFULL
      busCtrlWrapped.read(txempty, address = 0x18) // TXEMPTY
    }

    //manage RX
    val read = new Area {
      val fifo = StreamFifo(io.read.payload, config.rxFifoDepth)
      fifo.io.push << io.read
      val stream = fifo.io.pop
      val fifoOccupancy = fifo.io.occupancy

      val streamBreaked = stream.throwWhen(io.readBreak)
      busCtrlWrapped.readStreamNonBlocking(streamBreaked, address = 0, validBitOffset = 16, payloadBitOffset = 0)

      val rxempty = !fifo.io.pop.valid
      val rxfull = !fifo.io.push.ready
      busCtrlWrapped.read(rxempty, address = 8) // RXEMPTY
      busCtrlWrapped.read(rxfull, address = 0x1c) // RXFULL

      val nonEmpty = !rxempty

      def genCTS(freeThreshold: Int) = RegNext(fifoOccupancy <= config.rxFifoDepth - freeThreshold) init (False) // freeThreshold => how many remaining space should be in the fifo before allowing transfer
    }

    val eventBaseAddr = 0x0C

    val interruptCtrl = new Area {
      val eventManager = new CSREventManager()
      eventManager.add(new EventSourceProcess(write.nonFull.setName("tx")))
      eventManager.add(new EventSourceProcess(read.nonEmpty.setName("rx")))
      val interrupt = eventManager.generateIRQ(busCtrlWrapped, eventBaseAddr)
    }
    io.writeBreak := False
  }

  busCtrl.printDataModel()
}
