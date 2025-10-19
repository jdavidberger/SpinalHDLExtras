package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal.{FormalDut, anyseq}
import spinal.lib.bus.simple.PipelinedMemoryBusConfig
import spinal.lib.bus.wishbone.{AddressGranularity, Wishbone, WishboneConfig}
import spinal.lib.{master, slave}
import spinalextras.lib.bus.WishboneStage
import spinalextras.lib.formal.{ComponentWithFormalProperties, HasFormalProperties}
import spinalextras.lib.testing.{DefaultFormalDut, FormalTestSuite, GeneralFormalDut, test_funcs}

class WishboneStaging(wbConfig: WishboneConfig, m2s_stage: Boolean, s2m_stage: Boolean) extends ComponentWithFormalProperties {
  val io = new Bundle {
    val inBus = slave(Wishbone(wbConfig))
    val outBus = master(Wishbone(wbConfig))
  }

  io.outBus <> WishboneStage(io.inBus, m2s_stage, s2m_stage)
}

class WishboneStagingTestFormal extends AnyFunSuite with FormalTestSuite {
  val wbConfig = WishboneConfig(32, 32, addressGranularity = AddressGranularity.WORD)
  val wbConfigs = Seq(
    ("Basic", wbConfig),
    ("Byte", wbConfig.copy(addressGranularity = AddressGranularity.BYTE)),
  )

  formalTests().foreach(t => test(t._1) { t._2() })
  override def defaultDepth() = 50

  lazy val testConfigs = for {
    m2s <- Seq(true, false)
    s2m <- Seq(true, false)
    cfg <- wbConfigs
  } yield (f"${cfg._1}_${m2s}_${s2m}", cfg._2, m2s, s2m)

  override def generateRtl() = Seq()
  override def generateRtlBMC() = testConfigs.map(x => (x._1, () => new GeneralFormalDut(() => new WishboneStaging(x._2, x._3, x._4))))
}