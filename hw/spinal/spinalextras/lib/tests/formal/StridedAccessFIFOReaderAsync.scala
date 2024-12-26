package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal.{FormalDut, anyseq}
import spinal.lib._
import spinal.lib.bus.simple.PipelinedMemoryBusConfig
import spinalextras.lib.memory.StridedAccessFIFOReaderAsync
import spinalextras.lib.testing.FormalTestSuite

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

  anyseq(dut.io.pop.async_ready)
  anyseq(dut.io.bus.cmd.ready)
  anyseq(dut.io.bus.rsp)
}


class StridedAccessFIFOReaderAsyncFormalTest extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 200

  formalTests().foreach(t => test(t._1) { t._2() })

  override def generateRtl() = Seq((suiteName, () => new StridedAccessFIFOReaderAsyncFormal(UInt(8 bits), 900, 0, 9)))
}