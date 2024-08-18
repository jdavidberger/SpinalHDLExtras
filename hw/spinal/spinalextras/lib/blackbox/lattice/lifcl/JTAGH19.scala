package spinalextras.lib.blackbox.lattice.lifcl

import spinal.core._
import spinal.lib._
import spinal.lib.com.jtag.Jtag

import scala.language.postfixOps

case class JTagH19IO() extends Bundle with IMasterSlave{
  val JSHIFT, JUPDATE, JRSTN, JCE2, CDN = Bool()
  val JTCK, JTDI   = Bool()
  val IP_ENABLE, ER2_TDO = Bits(19 bits)

  override def asMaster(): Unit = {
    out(JTCK, JTDI, JSHIFT, JUPDATE, JRSTN, JCE2, CDN, IP_ENABLE)
    in(ER2_TDO)
  }

  override def asSlave(): Unit = {
    in(JTCK, JTDI, JSHIFT, JUPDATE, JRSTN, JCE2, CDN, IP_ENABLE)
    out(ER2_TDO)
  }
}

class JTAGH19( ) extends BlackBox {
  val io = new Bundle {
    val jtag = slave(Jtag())
    val jtagio = slave(JTagH19IO())
  }
  io.jtagio.setPartialName("")
  io.jtag.setPartialName("")
  io.jtag.elements.foreach(x => x._2.setPartialName(x._1.toUpperCase))

  noIoPrefix()
}