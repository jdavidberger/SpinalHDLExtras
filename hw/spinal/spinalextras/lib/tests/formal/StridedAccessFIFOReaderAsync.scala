package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal.{FormalDut, SpinalFormalConfig, anyconst, anyseq}
import spinal.lib._
import spinal.lib.bus.simple.PipelinedMemoryBusConfig

import spinalextras.lib.memory.StridedAccessFIFOReaderAsync
import spinalextras.lib.testing.{FormalTestSuite, test_funcs}

import scala.language.postfixOps

case class StridedAccessFIFOReaderAsyncFormal[T <: Data](
                                                    dataType: HardType[T],
                                                    /** Depth in units of dataType */
                                                    depth: Int,
                                                    baseAddress: BigInt,
                                                    outCnt: Int,
                                                    busConfig: PipelinedMemoryBusConfig = PipelinedMemoryBusConfig(32, 32),
                                                    rsp_latency : Int = 0
                                                  ) extends Component {
  val dut = FormalDut(StridedAccessFIFOReaderAsync(dataType = dataType, depth = depth, baseAddress = baseAddress, outCnt = outCnt, busConfig = busConfig, rsp_latency = rsp_latency))
  assumeInitial(ClockDomain.current.isResetActive)

  anyconst(dut.io.debug_fake_read)

//  if(depth < 20) {
//    val cycles = Counter(8, inc = dut.armRead.rise(True))
//    cover(cycles.value > 3)
//  }
  dut.anyseq_inputs()
  //HasFormalAsserts.printFormalAssertsReport()
}

class StridedAccessFIFOReaderAsyncFormalTest extends AnyFunSuite with FormalTestSuite {

  formalTests().foreach(t => test(t._1) { t._2() })

  override def generateRtlBMC() = Seq() // generateRtl()
  override def generateRtlCover() = Seq() // generateRtl()

  override def BMCConfig(): SpinalFormalConfig = formalConfig.withBMC(15)

  override def CoverConfig(): SpinalFormalConfig = formalConfig.withCover(15)

  override def ProveConfig(): SpinalFormalConfig = formalConfig.withProve(2)

  override def generateRtl() = Seq(
    (s"${suiteName}_tiny", () => StridedAccessFIFOReaderAsyncFormal(UInt(2 bits), 27, 0, 3, busConfig = PipelinedMemoryBusConfig(32, 3))),
    (s"${suiteName}_small", () => StridedAccessFIFOReaderAsyncFormal(UInt(8 bits), 4, 0, 1)),
    ("medium", () => StridedAccessFIFOReaderAsyncFormal(UInt(8 bits), 900, 0, 9)),
    (s"StridedAccessFIFOReaderAsyncFormal_uc1", () => new StridedAccessFIFOReaderAsyncFormal(UInt(8 bits), depth = 1228800, baseAddress = 307200, outCnt = 8, rsp_latency = 0))
  )
}