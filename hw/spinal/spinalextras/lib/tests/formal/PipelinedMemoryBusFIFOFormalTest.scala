package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal._
import spinal.lib._
import spinal.lib.bus.misc.{AllMapping, DefaultMapping, SizeMapping}
import spinal.lib.bus.simple.{PipelinedMemoryBusArbiter, PipelinedMemoryBusConfig, PipelinedMemoryBusDecoder}
import spinalextras.lib.bus.PipelineMemoryGlobalBus
import spinalextras.lib.bus.simple.SimpleMemoryProvider
import spinalextras.lib.memory.PipelinedMemoryBusFIFO
import spinalextras.lib.testing.{FormalTestSuite, test_funcs}

import scala.collection.mutable

case class PipelinedMemoryBusFIFOFormal[T <: Data](dataType : HardType[T],
                                                   sm : SizeMapping,
                                                   sysBus : Option[PipelineMemoryGlobalBus] = None, localPushDepth : Int = 0, localPopDepth : Int = 0,
                                                   check_flush : Boolean = false,
                                                   check_response : Boolean = false) extends Component {
  val globalBus = sysBus.getOrElse(PipelineMemoryGlobalBus(PipelinedMemoryBusConfig(32, dataType.getBitsWidth)))
  val busSlave = globalBus.add_slave("default", DefaultMapping)
  val dut = FormalDut(new PipelinedMemoryBusFIFO(dataType, sm,
    Some(globalBus),
    localPushDepth, localPopDepth))
  assumeInitial(ClockDomain.current.isResetActive)

  val busSlaveContract = test_funcs.assertPMBContract(busSlave, assume_slave = true)
  dut.covers()

  dut.io.pop.formalAssertsMaster()

  if(check_flush) {
    anyseq(dut.io.flush)
  } else {
    dut.io.flush := False
  }
  anyseq(dut.io.push.valid)
  anyseq(dut.io.push.payload)
  anyseq(dut.io.pop.ready)
  dut.io.push.formalAssumesSlave()

  if(check_response) {
    val mem = new SimpleMemoryProvider(mapping = sm, config = globalBus.config)
    mem.io.bus <> busSlave

    val f = dut.io
    val testFifo = StreamFifo(dataType, sm.size.toInt)
    testFifo.io.push.payload := f.push.payload
    testFifo.io.push.valid := f.push.fire
    testFifo.io.flush := f.flush
    test_funcs.assertStreamContract(testFifo.io.push)

    testFifo.io.pop.ready := f.pop.fire

    assert(f.occupancy === testFifo.io.occupancy)
    assert(f.pop.fire === False || testFifo.io.pop.fire)
    assert(f.pop.fire === False || (testFifo.io.pop.payload === f.pop.payload))
  } else {

    anyseq(busSlave.cmd.ready)
    anyseq(busSlave.rsp)
  }

  globalBus.build()

  withAutoPull()

  test_funcs.formalAssumeLibraryComponents()
}


class PipelinedMemoryBusFIFOFormalTest extends AnyFunSuite with FormalTestSuite {
  val create_formal = (check_response : Boolean) => new PipelinedMemoryBusFIFOFormal(UInt(8 bits), (0, 15), check_response = check_response)

  formalTests().foreach(t => test(t._1) { t._2() })

  override def defaultDepth() = 20

  override def BMCConfig() : SpinalFormalConfig = FormalConfig.withConfig(config).withBMC(15)

  override def generateRtlBMC() = Seq(("check_response", () => create_formal(true)))
  override def generateRtl() = Seq(("no_check_response", () => create_formal(false)))
}
