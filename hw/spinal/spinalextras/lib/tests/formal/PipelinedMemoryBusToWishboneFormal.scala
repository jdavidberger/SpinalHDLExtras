package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal.{FormalDut, anyseq}
import spinal.lib._
import spinal.lib.bus.simple.PipelinedMemoryBusConfig
import spinal.lib.bus.wishbone.{AddressGranularity, WishboneConfig}
import spinalextras.lib.bus.PipelinedMemoryBusToWishbone
import spinalextras.lib.memory.{StreamToBuffer, StridedAccessFIFOReaderAsync}
import spinalextras.lib.testing.{FormalTestSuite, test_funcs}

import scala.language.postfixOps

case class PipelinedMemoryBusToWishboneFormal(wbConfig: WishboneConfig, pipelinedMemoryBusConfig : PipelinedMemoryBusConfig, rspQueue : Int = 8) extends Component{
  val dut = FormalDut(new PipelinedMemoryBusToWishbone(wbConfig, pipelinedMemoryBusConfig, rspQueue))
  assumeInitial(ClockDomain.current.isResetActive)

  dut.io.pmb.cmd.formalAssumesSlave()
  anyseq(dut.io.pmb.cmd.valid)
  anyseq(dut.io.pmb.cmd.payload)

  assume((dut.io.wb.byteAddress() & (dut.io.wb.config.wordAddressInc() - 1)) === 0)

  val wbContract = test_funcs.assumeWishboneBusContract(dut.io.wb)
  for((n, el) <- dut.io.wb.elements) {
    if(el.isInput) {
      println(n, el)
      anyseq(el)
    }
  }
}


class PipelinedMemoryBusToWishboneFormalTest extends AnyFunSuite with FormalTestSuite {
  val wbConfig = WishboneConfig(32, 32, addressGranularity = AddressGranularity.WORD)
  val wbConfigs = Seq(
    ("Basic", wbConfig),
    ("Byte", wbConfig.copy(addressGranularity = AddressGranularity.BYTE)),
//    ("Pipeline", wbConfig.pipelined),
//    ("PipelineByte", wbConfig.copy(addressGranularity = AddressGranularity.BYTE).pipelined),
  )

  formalTests().foreach(t => test(t._1) { t._2() })
  override def defaultDepth() = 50

  override def generateRtl() = wbConfigs.map(x => (x._1, () => new PipelinedMemoryBusToWishboneFormal(x._2, PipelinedMemoryBusConfig(32,32))))
}