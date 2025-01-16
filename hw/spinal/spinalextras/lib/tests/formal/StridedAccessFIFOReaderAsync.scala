package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal.{FormalDut, anyseq}
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

  dut.formalAssumeInputs()
  dut.formalAsserts()

  ///val bus_contract = test_funcs.assertPMBContract(dut.io.bus, assume_slave = true)

  anyseq(dut.io.pop.async_ready)
  anyseq(dut.io.bus.cmd.ready)
  anyseq(dut.io.bus.rsp)

  test_funcs.formalAssumeLibraryComponents()
}

class StridedAccessFIFOReaderAsyncFormalTest extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 10

  formalTests().foreach(t => test(t._1) { t._2() })

  override def generateRtlCover() = generateRtl()

  override def generateRtl() = Seq(
    (suiteName, () => new StridedAccessFIFOReaderAsyncFormal(UInt(8 bits), 900, 0, 9)),
    (s"StridedAccessFIFOReaderAsyncFormal_uc1", () => new StridedAccessFIFOReaderAsyncFormal(UInt(8 bits), depth = 1228800, baseAddress = 307200, outCnt = 8, rsp_latency = 0))
  )
}