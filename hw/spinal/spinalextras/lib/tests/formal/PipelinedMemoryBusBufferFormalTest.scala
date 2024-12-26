package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal._
import spinal.lib.bus.simple._
import spinalextras.lib.memory.PipelinedMemoryBusBuffer
import spinalextras.lib.testing.FormalTestSuite


case class PipelinedMemoryBusBufferFormal[T <: Data](dataType : HardType[T], depth : Int, baseAddress : Int = 0,
                                               var config : PipelinedMemoryBusConfig = null,
                                               rsp_latency : Int = 0, cmd_latency : Int = 0, read_trigger : Int = -1,

                                                     check_flush : Boolean = true) extends Component {
  val dut = FormalDut(new PipelinedMemoryBusBuffer(dataType, depth, baseAddress, config, rsp_latency, cmd_latency, read_trigger))
  assumeInitial(ClockDomain.current.isResetActive)

  if(check_flush) {
    anyseq(dut.io.flush)
  } else {
    dut.io.flush := False
  }
  anyseq(dut.io.bus.rsp)
  anyseq(dut.io.bus.cmd.ready)
  anyseq(dut.io.memoryValid)
  anyseq(dut.io.pop.ready)
  assume(~dut.overflow)
}

class PipelinedMemoryBusBufferFormalTest extends AnyFunSuite with FormalTestSuite {

  val depth = 100

  val create_formal = () => new PipelinedMemoryBusBufferFormal(UInt(8 bits), 500, config = PipelinedMemoryBusConfig(32, 32), rsp_latency = 10)

  formalTests().foreach(t => test(t._1) { t._2() })

  override def generateRtl(): Seq[(String, () => Component)] = Seq(("PMBBufferTest", create_formal))
}
