package spinalextras.lib.bus

import spinal.core.GlobalData
import spinal.lib.bus.regif._

import java.io.PrintWriter

case class DefineGenerator(name : String,
                           override val prefix : String) extends BusIfDoc {
  override  val suffix: String = ".h"

  override def body(): String = {
    (
    s"#define ${prefix}_REGS(REGISTER)" +
      bi.slices.map(descr =>
        s"""\tREGISTER(0x${descr.getAddr().toString(16)}, ${descr.getName()}, "${descr.getDoc().replace("\n", "")}")"""
      )
      ).mkString("\r\n")
  }
}