package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal.{FormalDut, anyseq}
import spinal.lib._
import spinal.lib.bus.simple.PipelinedMemoryBusConfig
import spinal.lib.formal.ComponentWithFormalAsserts
import spinalextras.lib.misc.AsyncStream
import spinalextras.lib.testing.{FormalTestSuite, test_funcs}

import scala.language.postfixOps

class AsyncStreamTest extends ComponentWithFormalAsserts {
  val io = new Bundle {
    val stream_in = slave(AsyncStream(Bool()))
    val stream_out = master(AsyncStream(Bool()))
  }

  io.stream_in >> io.stream_out

  override protected def formalChecks()(implicit useAssumes: Boolean): Unit = {
    io.stream_in.formalAssertEquivalence(io.stream_out)
    super.formalChecks()
  }
}

case class AsyncStreamFormal() extends Component {
  val dut = FormalDut(new AsyncStreamTest())
  assumeInitial(ClockDomain.current.isResetActive)

  dut.anyseq_inputs()
}


class AsyncStreamFormalTest extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 20

  formalTests().foreach(t => test(t._1) { t._2() })

  override def generateRtl() = Seq((suiteName,
    () => AsyncStreamFormal())
  )
}