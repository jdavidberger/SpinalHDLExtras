package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal.{FormalDut, HasFormalAsserts, anyseq}
import spinal.lib._
import spinal.lib.bus.simple.PipelinedMemoryBusConfig
import spinalextras.lib.misc.AsyncStream
import spinalextras.lib.testing.{FormalTestSuite, test_funcs}

import scala.language.postfixOps

class AsyncStreamTest extends Component with HasFormalAsserts {
  val io = new Bundle {
    val stream_in = slave(AsyncStream(Bool()))
    val stream_out = master(AsyncStream(Bool()))
  }

  io.stream_in >> io.stream_out

  override lazy val formalValidInputs = io.stream_in.formalIsProducerValid() && io.stream_out.formalIsConsumerValid()
  override def formalChecks()(implicit useAssumes: Boolean) = null
}
case class AsyncStreamFormal() extends Component {
  val dut = FormalDut(new AsyncStreamTest())
  assumeInitial(ClockDomain.current.isResetActive)

  dut.formalAssumeInputs()
  dut.formalAsserts()
  test_funcs.anyseq_inputs(dut.io)
}


class AsyncStreamFormalTest extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 20

  formalTests().foreach(t => test(t._1) { t._2() })

  override def generateRtl() = Seq((suiteName,
    () => AsyncStreamFormal())
  )
}