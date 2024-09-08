package spinalextras.lib.mipi

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class MIPIIO(NUM_RX_LANE : Int = 2) extends Bundle with IMasterSlave {
  val clk_n, clk_p = Analog(Bool())

  val data_n, data_p = Analog(Bits(NUM_RX_LANE bits))

  override def asMaster(): Unit = {
    for(s <- Seq(clk_n, clk_p, data_n, data_p)) {
      s.setAsAnalog()
    }
    inout(clk_n, clk_p, data_n, data_p)
  }
}
