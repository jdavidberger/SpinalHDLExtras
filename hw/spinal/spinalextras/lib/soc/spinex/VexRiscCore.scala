package spinalextras.lib.soc.spinex

import spinal.core._
import spinal.lib._
import spinal.lib.cpu.riscv.debug.{DebugModule, DebugTransportModuleParameter}
import spinalextras.lib.logging.{FlowLogger, GlobalLogger}
import spinalextras.lib.soc.spinex.plugins.JTagPlugin
import spinalextras.lib.soc.spinex.{Spinex, SpinexConfig, SpinexCpuConfig}
import vexriscv.VexRiscv
import vexriscv.plugin.{EmbeddedRiscvJtag, Plugin}
import vexriscv.ip.InstructionCacheMemBus
import vexriscv.plugin.{CsrPlugin, DBusSimpleBus, DBusSimplePlugin, DebugPlugin, EmbeddedRiscvJtag, ExternalInterruptArrayPlugin, IBusCachedPlugin, IBusSimpleBus, IBusSimplePlugin}
import vexriscv.{ExceptionCause, VexRiscv, VexRiscvConfig}

import scala.collection.mutable.ArrayBuffer

case class VexRiscCPU(val plugins: ArrayBuffer[Plugin[VexRiscv]]) extends SpinexCpuConfig {
  def apply(cfg: SpinexConfig, spinex: Spinex): Any = {
    if (cfg.withJtag) {
      plugins += //new DebugPlugin(debugClockDomain, hardwareBreakpointCount),
        new EmbeddedRiscvJtag(
          p = DebugTransportModuleParameter(
            addressWidth = 7,
            version = 1,
            idle = 7
          ),
          debugCd = spinex.debugClockDomain,
          withTunneling = false,
          withTap = true
        )
    }

    val cpu = new VexRiscv(
      config = VexRiscvConfig(
        plugins = plugins,
      )
    )

    for (plugin <- cpu.plugins) plugin match {
      case plugin: IBusCachedPlugin =>
        spinex.add_master(plugin.iBus)
      case plugin: IBusSimplePlugin =>
        spinex.add_master(plugin.iBus)
      case plugin: DBusSimplePlugin => {
        spinex.add_master(plugin.dBus.cmdHalfPipe().setName("dBus_staged"))
        //add_master(plugin.dBus.cmdS2mPipe().setName("dBus_staged"))
      }
      case plugin: CsrPlugin => {
        plugin.timerInterrupt := spinex.system.timerInterrupt
        spinex.withAutoPull()

        if (plugin.config.withPrivilegedDebug) {
          GlobalLogger(Set("cpu"),
            FlowLogger.flows(plugin.debugBus.dmToHart),
            FlowLogger.flows(plugin.debugBus.hartToDm),
          )
        }

        GlobalLogger(Set("cpu"),
          FlowLogger.flows(plugin.exceptionPortsInfos.map({ ec =>
            class ExceptionCauseWithPC() extends ExceptionCause(ec.port.codeWidth) {
              val pc = UInt(32 bits)
            }

            val rtn = Flow(new ExceptionCauseWithPC())
            rtn.payload.pc := ec.stage.input(plugin.pipeline.config.PC)
            rtn.payload.badAddr := ec.port.badAddr
            rtn.payload.code := ec.port.code
            rtn.valid := ec.port.valid
            rtn.setName(ec.port.refOwner.toString + "_" + ec.port.name + "WithPC")
            rtn
          }): _*)
        )
      }
      case plugin: ExternalInterruptArrayPlugin => {
        plugin.externalInterruptArray := spinex.system.externalInterrupts
      }
      case plugin: DebugModule => {
        GlobalLogger(Set("cpu"),
          FlowLogger.flows(plugin.io.harts.map(_.hartToDm): _*),
          FlowLogger.flows(plugin.io.harts.map(_.dmToHart): _*)
        )
      }
      case plugin: DebugPlugin => plugin.debugClockDomain {
        spinex.resetCtrl.systemReset setWhen (RegNext(plugin.io.resetOut))
        spinex.getPlugin[JTagPlugin].get.jtags.append(plugin.io.bus.fromJtag())
      }
      case plugin: EmbeddedRiscvJtag => {
        spinex.getPlugin[JTagPlugin].get.jtags.append(plugin.jtag)
      }
      case _ =>
    }

    cpu

  }
}
