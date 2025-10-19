package spinalextras.lib.soc.peripherals

import spinal.core.{Area, Bits, Bundle, Component, False, IntToBuilder, Reg, RegNext, UInt, log2Up, out, widthOf}
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config, Apb3SlaveFactory}
import spinal.lib.bus.misc.{AddressMapping, BusSlaveFactory, BusSlaveFactoryAddressWrapper, SizeMapping}
import spinal.lib.com.uart.{Apb3UartCtrl, Uart, UartCtrl, UartCtrlGenerics, UartCtrlInitConfig, UartCtrlMemoryMappedConfig, UartParityType, UartStopType}
import spinal.lib.{StreamFifo, master, slave}
import spinalextras.lib.soc.spinex.{Spinex, SpinexPlugin, SpinexRegisterFileApbPlugin}
import spinalextras.lib.soc.{CSREventManager, DeviceTree, EventSourceProcess}

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
      val streamUnbuffered = busCtrlWrapped.createAndDriveFlow(Bits(g.dataWidthMax bits), address = 0, documentation="tx").toStream
      val (stream, fifoOccupancy) = streamUnbuffered.queueWithOccupancy(config.txFifoDepth)
      io.write << stream

      //busCtrlWrapped.read(sat255(config.txFifoDepth - fifoOccupancy),address = 4,bitOffset = 16)
      //streamUnbuffered.ready.allowPruning()
      //busCtrlWrapped.read(stream.valid, address = 4, 15)

      val txfull = !streamUnbuffered.ready
      val txempty = !stream.valid
      val nonFull = !txfull

      busCtrlWrapped.read(txfull, address = 4, documentation="txfull")
      busCtrlWrapped.read(txempty, address = 0x18, documentation="txempty")
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
      busCtrlWrapped.read(rxempty, address = 8, documentation="rxempty")
      busCtrlWrapped.read(rxfull, address = 0x1c, documentation="rxfull") // RXFULL

      val nonEmpty = !rxempty

      def genCTS(freeThreshold: Int) = RegNext(fifoOccupancy <= config.rxFifoDepth - freeThreshold) init (False) // freeThreshold => how many remaining space should be in the fifo before allowing transfer
    }

    val eventBaseAddr = 0x0C

    val interruptCtrl = new Area {
      val eventManager = new CSREventManager()
      eventManager.add(new EventSourceProcess(write.nonFull.setName("tx"), description = "tx_irq"))
      eventManager.add(new EventSourceProcess(read.nonEmpty.setName("rx"), description = "rx_irq"))
      val interrupt = eventManager.generateIRQ(busCtrlWrapped, eventBaseAddr)
    }
    io.writeBreak := False
  }

  busCtrl.printDataModel()
}

object UartCtrlPlugin {
  val defaultConfig = UartCtrlMemoryMappedConfig(
    uartCtrlConfig = UartCtrlGenerics(
      dataWidthMax      = 8,
      clockDividerWidth = 20,
      preSamplingSize   = 1,
      samplingSize      = 3,
      postSamplingSize  = 1
    ),
    initConfig = UartCtrlInitConfig(
      baudrate = 115200,
      dataLength = 7,  //7 => 8 bits
      parity = UartParityType.NONE,
      stop = UartStopType.ONE
    ),
    busCanWriteClockDividerConfig = false,
    busCanWriteFrameConfig = false,
    txFifoDepth = 16,
    rxFifoDepth = 16
  )
}
case class UartCtrlPlugin(config: UartCtrlMemoryMappedConfig = UartCtrlPlugin.defaultConfig,
                          mapping : SizeMapping = SizeMapping(0x1800, 32 Bytes),
                          name : String = "uart",
                          irqPriority : Int = 10) extends SpinexRegisterFileApbPlugin(name, mapping) {
  override val apbConfig = Apb3Config(log2Up(mapping.size), 32, useSlaveError = false)
  lazy val uart = master(Uart())

  override val compatible: Seq[String] = Seq("spinex,uart", "litex,uart");

  var _regs : Seq[(String, SizeMapping)] = null
  override def regs = _regs

  var interruptIdx = 0
  override def interrupts: Seq[(Int, Int)] = Seq((interruptIdx, irqPriority))

  override def apply(som: Spinex): Unit = {
    val uartCtrl = SpinexApb3UartCtrl(config)
    som.io.valCallbackRec(uart, name)
    uartCtrl.io.uart <> uart
    uartCtrl.io.apb <> apb

    interruptIdx = som.system.addInterrupt(uartCtrl.io.interrupt)

    _regs = busCtrlToRegs(uartCtrl.busCtrl)
    super.apply(som)
  }

  override def appendDeviceTree(dt: DeviceTree): Unit = {
    super.appendDeviceTree(dt)
    dt.addEntry(f"current-speed = <${config.initConfig.baudrate}>;", baseEntryPath:_*)
  }
}
