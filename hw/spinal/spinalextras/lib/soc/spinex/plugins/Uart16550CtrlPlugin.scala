package spinalextras.lib.soc.spinex.plugins

import spinal.core.{ClockDomain, ClockingArea, IntToBuilder, log2Up}
import spinal.lib.bus.amba3.apb.{Apb3Config, Apb3SlaveFactory}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.com.uart._
import spinal.lib.master
import spinalextras.lib.logging.{GlobalLogger, SignalLogger}
import spinalextras.lib.soc.DeviceTree
import spinalextras.lib.soc.peripherals.{Uart16550Config, Uart16550Ctrl, UartCtrlHighRate}
import spinalextras.lib.soc.spinex.{Spinex, SpinexRegisterFileApbPlugin}

case class Uart16550CtrlPlugin(mapping : SizeMapping = SizeMapping(0x1800, 32 Bytes),
                          name : String = "uart",
                          irqPriority : Int = 10, uartClockDomain: ClockDomain = ClockDomain.current) extends SpinexRegisterFileApbPlugin(name, mapping) {
  override val apbConfig = Apb3Config(log2Up(mapping.size), 32, useSlaveError = false)
  lazy val uart = master(Uart())

  override val compatible: Seq[String] = Seq("ns16550");

  var _regs : Seq[(String, SizeMapping)] = null
  override def regs = _regs

  var interruptIdx = 0
  override def interrupts: Seq[(Int, Int)] = Seq((interruptIdx, irqPriority))

  override def apply(som: Spinex): Unit = {
    val uartCtrl = new ClockingArea(uartClockDomain) {
      val uartCtrl = new UartCtrlHighRate()
    }.uartCtrl

    val busFactory = Apb3SlaveFactory(apb)
    val regCtrl = Uart16550Ctrl.driveFrom16550(uartCtrl, busFactory, Uart16550Config())
    uartCtrl.io.uart <> uart

    som.io.valCallbackRec(uart, name)

    //interruptIdx = som.system.addInterrupt(uartCtrl.io.interrupt, 2)
    interruptIdx = som.system.addInterrupt(regCtrl.interruptCtrl.interrupt, 2)

    _regs = busCtrlToRegs(busFactory)

    GlobalLogger(
      Set("uart"),
      SignalLogger.concat("uart_io", uart.rxd, uart.txd, uart.cts, uart.rts, regCtrl.interruptCtrl.interrupt.setName("uart_irq"))
    )

    super.apply(som)
  }

  override def appendDeviceTree(dt: DeviceTree): Unit = {
    super.appendDeviceTree(dt)
    //dt.addEntry(f"current-speed = <${config.initCnfig.baudrate}>;", baseEntryPath:_*)
  }
}