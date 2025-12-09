package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal._
import spinal.lib._
import spinal.lib.bus.misc.{AllMapping, DefaultMapping, SizeMapping}
import spinal.lib.bus.simple.{PipelinedMemoryBusArbiter, PipelinedMemoryBusConfig, PipelinedMemoryBusDecoder}
import spinalextras.lib.bus.PipelineMemoryGlobalBus
import spinalextras.lib.bus.simple.SimpleMemoryProvider
import spinalextras.lib.formal.HasFormalProperties
import spinalextras.lib.memory.PipelinedMemoryBusFIFO
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut, test_funcs}

import scala.collection.mutable

case class PipelinedMemoryBusFIFOFormal[T <: Data](dataType : HardType[T],
                                                   sm : SizeMapping,
                                                   sysBus : Option[PipelineMemoryGlobalBus] = None, localPushDepth : Int = 0, localPopDepth : Int = 0,
                                                   check_flush : Boolean = true,
                                                   check_response : Boolean = false) extends Component {
  val globalBus = sysBus.getOrElse(PipelineMemoryGlobalBus(PipelinedMemoryBusConfig(32, dataType.getBitsWidth)))
  val busSlave = globalBus.add_slave("default", DefaultMapping)
  val dut = FormalDut(new PipelinedMemoryBusFIFO(dataType, sm,
    Some(globalBus),
    localPushDepth, localPopDepth))
  assumeInitial(ClockDomain.current.isResetActive)

  dut.covers().foreach(x => cover(x.condition))

  if(check_flush) {
    anyseq(dut.io.flush)
  } else {
    dut.io.flush := False
  }

  if(check_response) {
    val mem = new SimpleMemoryProvider(mapping = sm, config = globalBus.config)
    mem.io.bus <> busSlave

    val f = dut.io
    val testFifo = StreamFifo(dataType, sm.size.toInt)
    testFifo.io.push.payload := f.push.payload
    testFifo.io.push.valid := f.push.fire
    testFifo.io.flush := f.flush

    testFifo.io.pop.ready := f.pop.fire

    assert(f.occupancy === testFifo.io.occupancy)
    assert(f.pop.fire === False || testFifo.io.pop.fire)
    assert(f.pop.fire === False || (testFifo.io.pop.payload === f.pop.payload))
    cover(testFifo.io.push.ready === False)
    cover(testFifo.io.availability === 0)
  } else {
    anyseq(busSlave.cmd.ready)
    anyseq(busSlave.rsp)
  }

  globalBus.build()

  withAutoPull()

  cover(dut.full)
  cover(dut.io.push.fire)
  cover(dut.io.push.valid)
  cover(busSlave.rsp.valid)
  cover(busSlave.cmd.fire)
  cover(busSlave.cmd.valid)
  addPrePopTask(() => {
    dut.anyseq_inputs()
  })

  HasFormalProperties.printFormalAssertsReport()
}


class PipelinedMemoryBusFIFOFormalTest extends AnyFunSuite with FormalTestSuite {

  val create_formal = (check_response : Boolean) => new PipelinedMemoryBusFIFOFormal(UInt(8 bits), (0, 3), check_response = check_response, localPopDepth = 4)

  formalTests().foreach(t => test(t._1) { t._2() })

  override def defaultDepth() = 10

  override def BMCConfig() : SpinalFormalConfig = FormalConfig.withConfig(config).withBMC(15)
  override def CoverConfig() : SpinalFormalConfig = FormalConfig.withConfig(config).withCover(10).withDebug

  override def generateRtlBMC() = Seq(("check_response", () => create_formal(true)))
  override def generateRtlCover() = Seq(("check_response", () => create_formal(false)))
  override def generateRtl() = Seq(
    ("no_check_response", () => create_formal(false)),
    ("Basic", () => GeneralFormalDut( () => new PipelinedMemoryBusFIFO(Bits(8 bits), SizeMapping(0, 100) )))
  )
}
