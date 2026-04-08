package spinalextras.mains

import spinal.core._
import spinal.lib.Timeout
import spinalextras.lib.Config
import spinalextras.lib.blackbox.lattice.lifcl.WDT
import spinalextras.lib.misc.AutoInterconnect.buildInterconnect
import spinalextras.lib.soc.peripherals.{UartCtrlPlugin, XipFlashPlugin}
import spinalextras.lib.soc.spinex.{Spinex, SpinexConfig}

import scala.language.postfixOps

class SpinexMinimal(file : String) extends Component {
  val io = new Bundle {
    val led = out Bool()
  }
  noIoPrefix()
  withAutoPull()

  val som = Spinex(SpinexConfig.minimal)

  som.init_rom(file)

  io.led.setAsReg() init(False)
  val t = Timeout(3 sec)
  when(t) {
    io.led := !io.led
    t.clear()
  } elsewhen(som.getPlugin[UartCtrlPlugin].get.uart.rxd.fall() || som.getPlugin[UartCtrlPlugin].get.uart.txd.fall()) {
    io.led := !io.led
    t.clear()
  }

  val wdt = WDT.noop()

  buildInterconnect(Seq(som), io)
}

object SpinexMinimal{
  def main(args: Array[String]) {
    val report = Config.spinal.copy(
      defaultConfigForClockDomains = ClockDomainConfig(
        resetActiveLevel = LOW,
        resetKind = ASYNC
      ),
      targetDirectory = s"hw/gen/SpinexMinimal",
      defaultClockDomainFrequency = FixedFrequency(60 MHz),
      device = Device(vendor = "???", family = "???"),
    ).generateVerilog(new SpinexMinimal(args(0)))

    Spinex.generate_ipx(report)
  }
}
