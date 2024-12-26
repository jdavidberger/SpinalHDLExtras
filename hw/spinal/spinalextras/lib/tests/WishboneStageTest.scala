package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim.{SimClockDomainHandlePimper, SimTimeout}
import spinal.lib.bus.wishbone.{AddressGranularity, Wishbone, WishboneConfig}
import spinal.lib.{master, slave}
import spinal.lib.sim.ScoreboardInOrder
import spinal.lib.wishbone.sim.{WishboneDriver, WishboneMonitor, WishboneSequencer, WishboneTransaction}
import spinalextras.lib.Config
import spinalextras.lib.bus.WishboneStage

class WishboneStageTestDut(val cfg : WishboneConfig, m2s_stage: Boolean, s2m_stage: Boolean) extends Component {
  val io = new Bundle {
    val mbus = master(Wishbone(cfg))
    val sbus = slave(Wishbone(cfg))
  }

  (io.mbus) << WishboneStage(io.sbus, m2s_stage, s2m_stage)
}


class WishboneStageTest extends AnyFunSuite {
  def testConfig(cfg: WishboneConfig, m2s_stage: Boolean, s2m_stage: Boolean) {
    Config.sim.doSim(
      new WishboneStageTestDut(cfg, m2s_stage, s2m_stage)
    ) { dut =>
      SimTimeout(5000 us)
      dut.clockDomain.forkStimulus(100 MHz)

      val hostDriver = WishboneDriver(dut.io.sbus)
      val deviceDriver = WishboneDriver(dut.io.mbus)

      dut.clockDomain.waitSampling(10)

      val seq = WishboneSequencer.randomGen(dut.cfg)
      val sco = ScoreboardInOrder[WishboneTransaction]()

      new WishboneMonitor(dut.io.sbus, dut.clockDomain, true).addResponseCallback(
        (bus: Wishbone, transaction: WishboneTransaction, we: Boolean) =>
          sco.pushRef(transaction)
      )

      new WishboneMonitor(dut.io.mbus, dut.clockDomain, true).addResponseCallback(
        (bus: Wishbone, transaction: WishboneTransaction, we: Boolean) =>
          sco.pushDut(transaction)
      )

      deviceDriver.slaveSink()

      def fixAddress(address: BigInt): BigInt = {
        if (cfg.wordAddressInc() > 1) {
          address & ~(cfg.wordAddressInc() - 1)
        } else {
          address
        }
      }

      for (repeat <- 0 until 100) {
        seq.generateTransactions(10)

        while (!seq.isEmpty) {
          val tran = seq.nextTransaction.map(x => x.copy(address = fixAddress(x.address)))
          hostDriver.drive(tran, we = (repeat % 2 == 1))
          dut.clockDomain.waitSampling(1)
        }

        dut.clockDomain.waitSampling(10)
      }
    }
  }

  for (cfg <- Seq(
    WishboneConfig(32, 32, addressGranularity = AddressGranularity.WORD),
    WishboneConfig(32, 32, addressGranularity = AddressGranularity.BYTE)
  ); m2s_stage <- Seq(true, false); s2m_stage <- Seq(true, false)) {
    test(s"w${cfg.wordAddressInc()}_${m2s_stage}_${s2m_stage}") {
      testConfig(cfg, m2s_stage, s2m_stage)
    }
  }
}
