package spinalextras.lib.noc

import spinal.core._
import spinalextras.lib.formal.{FormalData, FormalProperties, FormalProperty}

import scala.language.postfixOps

case class Header(cfg : NocConfig) extends Bundle {
  val dest = UInt(cfg.topology.addressSize bits)
  val application = Bits(cfg.headerApplicationBits bits)
}

case class Flit(cfg: NocConfig) extends Bundle with FormalData {
  val vc = UInt(log2Up(cfg.virtualChannels) bits)
  val datum = Bits(cfg.dataWidth bits)


  /**
   * @return Whether or not the current state of the bundle is valid. Typically either asserted or assumed by a
   *         component which has this bundle as an input or an output.
   *
   *         For complicated properties, consider using the helper class `FormalProperties`
   */
  override def formalIsStateValid(): Seq[FormalProperty] = new FormalProperties(this) {
    if(!isPow2(cfg.virtualChannels)) {
      addFormalProperty(vc < cfg.virtualChannels)
    }
  }
}
