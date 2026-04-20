package spinalextras.examples

import spinal.core._
import spinal.lib._
import spinal.lib.com.jtag.Jtag
import spinal.lib.com.uart.{UartCtrl, UartCtrlInitConfig, UartParityType, UartStopType}
import spinal.lib.{Timeout, slave}
import spinalextras.lib.Config
import spinalextras.lib.blackbox.lattice.lifcl.WDT
import spinalextras.lib.debug.{JtagChain, JtagLoggerTap}
import spinalextras.lib.logging.{GlobalLogger, SignalLogger}
import spinalextras.lib.misc.SlipEncoder
import spinalextras.lib.soc.spinex.Spinex

import scala.language.postfixOps

class EventLoggerButtonsJtag(button_count : Int) extends Component {
  val io = new Bundle {
    val led = out Bool()
    val inputs = Array.fill(button_count)(in Bool())
    val jtag = slave(Jtag())
  }

  noIoPrefix()
  withAutoPull()

  io.led.setAsReg() init(False)
  val t = Timeout(1 sec)
  when(t) {
    io.led := !io.led
    t.clear()
  } elsewhen(io.inputs(0).fall(False)) {
    io.led := !io.led
    t.clear()
  }

  val wdt = WDT.noop()


  GlobalLogger(
    SignalLogger.concat("led", io.led)
  )

  GlobalLogger(
    SignalLogger.concat("inputs", io.inputs:_*)
  )


  val jtag = new JtagLoggerTap(95)

  val jtagChain = JtagChain(jtag.io.jtag)
  jtagChain.io.jtag <> io.jtag

  GlobalLogger.create_logger_stream(128, ctrlStreams = (jtag.io.outStream, jtag.io.inFlow))
}

class EventLoggerButtonsUart(button_count : Int) extends Component {
  val io = new Bundle {
    val led = out Bool()
    val inputs = Array.fill(button_count)(in Bool())

    val uart_txd = out(Bool())
    val uart_rxd = in(Bool())
  }

  noIoPrefix()
  withAutoPull()

  io.led.setAsReg() init(False)
  val t = Timeout(1 sec)
  when(t) {
    io.led := !io.led
    t.clear()
  } elsewhen(io.inputs(0).fall(False)) {
    io.led := !io.led
    t.clear()
  }

  GlobalLogger(
    SignalLogger.concat("led", io.led)
  )

  GlobalLogger(
    SignalLogger.concat("inputs", io.inputs:_*)
  )

  val ctrl = UartCtrl(UartCtrlInitConfig(115200, 7, parity = UartParityType.NONE, stop = UartStopType.ONE))
  ctrl.io.uart.txd <> io.uart_txd
  ctrl.io.uart.rxd <> io.uart_rxd

  val loggerOutStream = Stream(Bits(96 bits))

  SlipEncoder(loggerOutStream) <> ctrl.io.write

  GlobalLogger.create_logger_stream(8, ctrlStreams = (loggerOutStream, ctrl.io.read.toFlow))
}

object EventLoggerButtons extends App {
  val report = Config.spinal.copy(
      device = Device(vendor = "???", family = "???"),
      defaultClockDomainFrequency = FixedFrequency(60 MHz),
      defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC, resetActiveLevel = LOW),
      targetDirectory = s"hw/gen/EventLoggerButtons",
    )
    .generateVerilog(new EventLoggerButtonsUart(1))

  Spinex.generate_ipx(report)
}