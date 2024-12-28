package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal.{FormalDut, anyseq}
import spinal.lib._
import spinal.lib.bus.simple.PipelinedMemoryBusConfig
import spinalextras.lib.memory.{StreamToBuffer, StridedAccessFIFOReaderAsync}
import spinalextras.lib.testing.FormalTestSuite

import scala.language.postfixOps

case class StreamToBufferFormal[T <: Data](
    dataType: HardType[T],
    depth: Int,
    baseAddress: BigInt,
    busConfig: PipelinedMemoryBusConfig = PipelinedMemoryBusConfig(32, 32),
) extends Component {
  val dut = FormalDut(StreamToBuffer(dataType, depth, baseAddress, busConfig))
  assumeInitial(ClockDomain.current.isResetActive)

  dut.io.bus.cmd.formalAssertsMaster()
  dut.io.push.formalAssumesSlave()

  anyseq(dut.io.push.valid)
  anyseq(dut.io.push.payload)

  anyseq(dut.io.bus.cmd.ready)
  anyseq(dut.io.bus.rsp)
}

class StreamToBufferFormalTest extends AnyFunSuite with FormalTestSuite {
  formalTests().foreach(t => test(t._1) { t._2() })

  override def defaultDepth() = 10

  override def generateRtl() = Seq(
    ("8bit", () => new StreamToBufferFormal(Bits(8 bits), 100, 0xcafe)),
    ("24bit", () => new StreamToBufferFormal(Bits(24 bits), 100, 0xcafe)),
    ("32bit", () => new StreamToBufferFormal(Bits(32 bits), 100, 0xcafe)),
    ("32bit_0offset", () => new StreamToBufferFormal(Bits(32 bits), 100, 0)),
    ("64bit", () => new StreamToBufferFormal(Bits(64 bits), 100, 0xcaff))
  )
}
