package spinalextras.lib.soc.spinex

import spinal.core._
import spinal.core.fiber.hardFork
import vexiiriscv._
import vexiiriscv.execute.lsu.{LsuCachelessBus, LsuCachelessPlugin, LsuL1Plugin}
import vexiiriscv.fetch.{CachelessBus, FetchCachelessPlugin, FetchL1Bus, FetchL1Plugin}
import vexiiriscv.misc.{EmbeddedRiscvJtag, PrivilegedPlugin}
import spinalextras.lib.soc.spinex.SpinexCpuConfig
import spinalextras.lib.soc.spinex.plugins.JTagPlugin

case class VexiiRiscCPU(val params : vexiiriscv.ParamSimple) extends SpinexCpuConfig {
  def apply(cfg: SpinexConfig, spinex: Spinex): Any = {
    val plugins = params.plugins()
    ParamSimple.setPma(plugins, ParamSimple.defaultPma)

    val cpu = VexiiRiscv(plugins)
    spinex.interconnect.lock.retain()
    spinex.directInterconnect.lock.retain()

    for (plugin <- cpu.host.services) plugin match {
      case plugin: EmbeddedRiscvJtag => {
        plugin.setDebugCd(spinex.debugClockDomain)
        spinex.getPlugin[JTagPlugin].get.lock.retain()
      }
      case _ =>
    }

    hardFork {
      for (plugin <- cpu.host.services) plugin match {
        case plugin: FetchCachelessPlugin => spinex.add_master(plugin.logic.bus)
        case plugin: FetchL1Plugin => spinex.add_master(plugin.logic.bus)
        case plugin: LsuCachelessPlugin => spinex.add_master(plugin.logic.bus)
        case plugin: LsuL1Plugin => assert(false, "LSU L1 plugin is not currently supported")
        case plugin: EmbeddedRiscvJtag => {
          spinex.getPlugin[JTagPlugin].get.jtags.append(plugin.logic.jtag)
          spinex.getPlugin[JTagPlugin].get.lock.release()
        }
        case plugin: PrivilegedPlugin => {
          plugin.logic.harts.foreach(h => {
            h.int.m.timer := spinex.system.timerInterrupt
            h.int.m.external := spinex.system.externalInterrupts.orR
            h.int.m.software := False

            if(h.m.imsic != null) {
              h.m.imsic.file.triggers := spinex.system.externalInterrupts.resized
            }
          })
        }
        case _ =>
      }

      spinex.interconnect.lock.release()
      spinex.directInterconnect.lock.release()
    }

    cpu
  }
}