package spinalextras.lib.noc

import spinal.core._

import scala.language.postfixOps

case class Header(cfg : NocConfig) extends Bundle {
  val application = Bits(cfg.headerApplicationBits bits)
  val dest = UInt(cfg.topology.addressSize bits)
}

case class Flit(cfg: NocConfig) extends Bundle {
  val vc = UInt(log2Up(cfg.virtualChannels) bits)
  val datum = Bits(cfg.dataWidth bits)
}
