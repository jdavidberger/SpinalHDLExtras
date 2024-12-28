package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal.{FormalDut, anyseq}
import spinal.lib._
import spinal.lib.bus.simple.PipelinedMemoryBusConfig
import spinalextras.lib.memory.{StridedAccessFIFOAsync, StridedAccessFIFOReaderAsync}
import spinalextras.lib.testing.FormalTestSuite

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

  dut.io.bus.cmd.formalAssumesSlave()
  dut.io.push.formalAssumesSlave()

  anyseq(dut.io.push.valid)
  anyseq(dut.io.push.payload)

  anyseq(dut.io.pop.async_ready)

  anyseq(dut.io.bus.cmd.ready)
  anyseq(dut.io.bus.rsp)
}


class StridedAccessFIFOAsyncFormalTest extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 200

  formalTests().foreach(t => test(t._1) { t._2() })

  override def generateRtl() = Seq((suiteName,
    () => new StridedAccessFIFOAsyncFormal(UInt(32 bits), UInt(8 bits), 1024 * 9, 0, 9, rsp_latency = 15)))
}