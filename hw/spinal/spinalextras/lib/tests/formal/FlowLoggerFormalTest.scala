package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.{Bits, Bundle, ClockDomain, IntToBuilder, Module, assume, when}
import spinal.lib.bus.wishbone.{AddressGranularity, Wishbone, WishboneConfig}
import spinal.lib.{Flow, Stream, master, slave}
import spinalextras.lib.bus.WishboneGlobalBus
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperty}
import spinalextras.lib.logging.{FlowLogger, FlowLoggerTestBench}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

class FlowLoggerPortTestBench() extends ComponentWithFormalProperties {
  val sysBus = WishboneGlobalBus(WishboneConfig(32, 32, addressGranularity = AddressGranularity.BYTE))

  val io = new Bundle {
    val flows = Array.fill(2)(slave(Flow(Bits(3 bits))))
    val bus = slave(new Wishbone(sysBus.config))
    val log = master(Stream(Bits(95 bits)))
  }

  io.bus <> sysBus.add_master("cpu")
  val logger = new FlowLogger(io.flows.map(x => (x.payload, ClockDomain.current)), gtimeTimeout = 5)

  logger.io.flows.zip(io.flows).foreach(x => x._1 <> x._2)


  val portArea = logger.create_logger_port(sysBus, 0, 3, outputStream = Some(io.log))
  when(portArea.loggerFifo.io.flush) {
    assume(portArea.loggerFifo.io.pop.ready)
  }

  override def covers(): Seq[FormalProperty] = Seq(portArea.loggerFifo.io.availability === 0)
}

class FlowLoggerFormalTest extends AnyFunSuite with FormalTestSuite {
  formalTests().foreach(t => test(t._1) {
    t._2()
  })

  override def defaultDepth(): Int = 10

  override def CoverConfig() = formalConfig.withCover(20)

  override def ProveConfig() = formalConfig.withProve(5)

  override def BMCConfig() = formalConfig.withBMC(20)

  override def generateRtl() = Seq(
    ("Basic", () => GeneralFormalDut(() => new FlowLoggerTestBench())),
  )

  override def generateRtlBMC(): Seq[(String, () => Module)] =
    super.generateRtlBMC() ++
      Seq(
        ("Inactive", () => GeneralFormalDut(() => new FlowLoggerTestBench(true))),
        ("Port", () => GeneralFormalDut(() => new FlowLoggerPortTestBench())),
      )
}
