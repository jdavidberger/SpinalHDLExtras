package spinalextras.lib.bus

import spinal.core.GlobalData
import spinal.lib.bus.regif._

import java.io.PrintWriter

case class DefineGenerator(
                                   fileName : String,
                                   prefix : String) extends BusIfVisitor {
  val pc = GlobalData.get.phaseContext
  val targetPath = s"${pc.config.targetDirectory}/${fileName}.h"
  val pw = new PrintWriter(targetPath)

  override def begin(busDataWidth: Int): Unit = {
    pw.write(s"#define ${prefix}_REGS(REGISTER) \\\n")
  }

  def visit(descr : BaseDescriptor) : Unit = {
    descr match {
      case descr: RegDescr => regDescrVisit(descr)
      case _ => ???
    }
  }

  private def regDescrVisit(descr: RegDescr): Unit = {
    pw.write(s"""\tREGISTER(0x${descr.getAddr().toString(16)}, ${descr.getName()}, "${descr.getDoc().replace("\n", "")}")\\\n""")
  }

  override def end(): Unit = {
    pw.write(s"\n")
    pw.flush()
  }
}