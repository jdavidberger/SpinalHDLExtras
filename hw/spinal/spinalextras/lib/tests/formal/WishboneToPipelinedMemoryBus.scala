package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal.{FormalDut, anyseq}
import spinal.lib.bus.simple.PipelinedMemoryBusConfig
import spinal.lib.bus.wishbone.{AddressGranularity, WishboneConfig}
import spinalextras.lib.bus.WishboneToPipelinedMemoryBus
import spinalextras.lib.testing.FormalTestSuite

class WishboneToPipelinedMemoryBusFormal[T <: Data](pipelinedMemoryBusConfig : PipelinedMemoryBusConfig,
                                                    wbConfig: WishboneConfig, rspQueue : Int = 8, addressMap : (UInt => UInt) = identity) extends Component {
  val dut = FormalDut(new WishboneToPipelinedMemoryBus(pipelinedMemoryBusConfig, wbConfig, rspQueue = rspQueue, addressMap = addressMap))
  assumeInitial(ClockDomain.current.isResetActive)

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
  val create_formal = () => new WishboneToPipelinedMemoryBusFormal(PipelinedMemoryBusConfig(32, 32), wbConfig, rspQueue = 8)

  formalTests().foreach(t => test(t._1) { t._2() })
  override def defaultDepth() = 50


  override def generateRtl() = Seq(("", () => create_formal()))
}