package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal.{FormalDut, anyseq}
import spinal.lib._
import spinal.lib.bus.simple.PipelinedMemoryBusConfig
import spinalextras.lib.memory.{StridedAccessFIFOAsync, StridedAccessFIFOReaderAsync}
import spinalextras.lib.testing.{FormalAnyTestSuite, FormalTestSuite, GeneralFormalDut, test_funcs}

import scala.language.postfixOps

//case class StridedAccessFIFOAsyncFormal[T <: Data]( f : () => StridedAccessFIFOAsync[T]) extends Component {
//  val dut = FormalDut(f())
//  assumeInitial(ClockDomain.current.isResetActive)
//
//  dut.anyseq_inputs()
//}
//

class StridedAccessFIFOAsyncFormalTest extends FormalAnyTestSuite{

  override def defaultDepth() = 50

  override def BMCConfig() = formalConfig.withBMC(20)
  override def generateRtlCover() = Seq()
  override def generateRtlBMC(): Seq[(String, () => Component)] = generateRtl()
  override def generateRtl() = Seq(
    (suiteName, () => new GeneralFormalDut(() => StridedAccessFIFOAsync(UInt(32 bits), UInt(8 bits), 1024 * 9, 0, 9, rsp_latency = 15), 1)),
    ("StridedAccessFIFOAsync_from_reshape", () => new GeneralFormalDut(() => new StridedAccessFIFOAsync(UInt(48 bits), UInt(12 bits), 1920, 307200, 8,
      busConfig = PipelinedMemoryBusConfig(22, 64),
      rsp_latency = 16, cmd_latency = 1),
      depth = 1
    ))
  )
}