package spinalextras.lib.soc.spinex.plugins

import spinal.core.IntToBuilder
import spinal.lib.bus.misc.SizeMapping
import spinalextras.lib.soc.peripherals.SpinexApb3Timer
import spinalextras.lib.soc.spinex.{Spinex, SpinexRegisterFileApbPlugin}

import scala.language.postfixOps

case class TimerPlugin(mapping: SizeMapping = SizeMapping(0x2800, 256 Bytes)) extends SpinexRegisterFileApbPlugin("timer", mapping) {
  var _regs: Seq[(String, SizeMapping)] = null

  override def regs = _regs

  override val compatible: Seq[String] = Seq("spinex,timer", "litex,timer0")

  var interrupt : Int = 0

  override def interrupts: Seq[(Int, Int)] = Seq((interrupt, 0))

  override def apply(som: Spinex): Unit = {
    val timer = new SpinexApb3Timer(mapping.base)
    som.system.timerInterrupt setWhen (timer.io.interrupt)
    timer.io.apb <> apb
    _regs = busCtrlToRegs(timer.busCtrl)

    interrupt = som.system.addInterrupt(timer.io.interrupt, 1)

    super.apply(som)
  }
}
