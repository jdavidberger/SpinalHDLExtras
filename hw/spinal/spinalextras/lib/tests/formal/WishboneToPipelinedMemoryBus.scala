package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal.{FormalDut, anyseq}
import spinal.lib.bus.simple.PipelinedMemoryBusConfig
import spinal.lib.bus.wishbone.{AddressGranularity, WishboneConfig}
import spinalextras.lib.bus.WishboneToPipelinedMemoryBus
import spinalextras.lib.testing.{FormalTestSuite, test_funcs}

class WishboneToPipelinedMemoryBusFormal[T <: Data](pipelinedMemoryBusConfig : PipelinedMemoryBusConfig,
                                                    wbConfig: WishboneConfig, rspQueue : Int = 8, addressMap : (UInt => UInt) = identity) extends Component {
  val dut = FormalDut(new WishboneToPipelinedMemoryBus(pipelinedMemoryBusConfig, wbConfig, rspQueue = rspQueue, addressMap = addressMap))
  assumeInitial(ClockDomain.current.isResetActive)

  assume((dut.io.wb.byteAddress() & (dut.io.wb.config.wordAddressInc() - 1)) === 0)

  for((n, el) <- dut.io.wb.elements) {
    if(el.isInput) {
      anyseq(el)
    }
  }
  anyseq(dut.io.pmb.cmd.ready)
  anyseq(dut.io.pmb.rsp)
}

class WishboneToPipelinedMemoryBusTestFormal extends AnyFunSuite with FormalTestSuite {
  val wbConfig = WishboneConfig(32, 32, addressGranularity = AddressGranularity.WORD)
  val wbConfigs = Seq(
    ("Basic", wbConfig),
    ("Byte", wbConfig.copy(addressGranularity = AddressGranularity.BYTE)),
    //("Pipeline", wbConfig.pipelined),
    //("PipelineByte", wbConfig.copy(addressGranularity = AddressGranularity.BYTE).pipelined),
  )

  formalTests().foreach(t => test(t._1) { t._2() })
  override def defaultDepth() = 20


  override def generateRtlCover() = Seq()
  override def generateRtl() = wbConfigs.map(x => (x._1, () => new WishboneToPipelinedMemoryBusFormal(PipelinedMemoryBusConfig(32,32), x._2)))
}