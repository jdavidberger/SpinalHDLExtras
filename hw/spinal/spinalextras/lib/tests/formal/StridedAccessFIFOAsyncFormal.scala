package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal.{FormalDut, anyseq}
import spinal.lib._
import spinal.lib.bus.simple.PipelinedMemoryBusConfig
import spinalextras.lib.memory.{StridedAccessFIFOAsync, StridedAccessFIFOReaderAsync}
import spinalextras.lib.testing.{FormalTestSuite, test_funcs}

import scala.language.postfixOps

case class StridedAccessFIFOAsyncFormal[T <: Data](
                                              pushDataType: HardType[T],
                                              popDataType: HardType[T],
                                              depth: Int,
                                              baseAddress: BigInt,
                                              outCnt: Int,
                                              bufferSize: Int = 32,
                                              busConfig: PipelinedMemoryBusConfig = PipelinedMemoryBusConfig(32, 32),
                                              rsp_latency : Int = 0, cmd_latency : Int = 0
                                            ) extends Component {
  val dut = FormalDut(StridedAccessFIFOAsync(pushDataType, popDataType, depth, baseAddress, outCnt, bufferSize, busConfig, rsp_latency, cmd_latency))
  assumeInitial(ClockDomain.current.isResetActive)

  dut.formalAssumeInputs()
  dut.formalAsserts()

  anyseq(dut.io.push.valid)
  anyseq(dut.io.push.payload)

  anyseq(dut.io.pop.async_ready)

  anyseq(dut.io.bus.cmd.ready)
  anyseq(dut.io.bus.rsp)

  //test_funcs.formalAssumeLibraryComponents()
}


class StridedAccessFIFOAsyncFormalTest extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 20

  formalTests().foreach(t => test(t._1) { t._2() })

  override def BMCConfig() = formalConfig.withBMC(20)
  override def generateRtlCover() = Seq()
  override def generateRtlBMC(): Seq[(String, () => Component)] = generateRtl()
  override def generateRtl() = Seq(
    (suiteName, () => new StridedAccessFIFOAsyncFormal(UInt(32 bits), UInt(8 bits), 1024 * 9, 0, 9, rsp_latency = 15)),
    ("StridedAccessFIFOAsync_from_reshape", () => new StridedAccessFIFOAsyncFormal(UInt(48 bits), UInt(12 bits), 1920, 307200, 8,
      busConfig = PipelinedMemoryBusConfig(22, 64),
      rsp_latency = 16, cmd_latency = 1
    ))
  )
}