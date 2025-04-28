package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal._
import spinal.lib.bus.simple._
import spinalextras.lib.formal.HasFormalProperties
import spinalextras.lib.memory.PipelinedMemoryBusBuffer
import spinalextras.lib.testing.{FormalTestSuite, test_funcs}


case class PipelinedMemoryBusBufferFormal[T <: Data](dataType : HardType[T], depth : Int, baseAddress : Int = 0,
                                                     var config : PipelinedMemoryBusConfig = null,
                                                     rsp_latency : Int = 0, cmd_latency : Int = 0, read_trigger : Int = -1,
                                                     check_flush : Boolean = false) extends Component {
  val dut = FormalDut(new PipelinedMemoryBusBuffer(dataType, depth, baseAddress, config, rsp_latency, cmd_latency, read_trigger))
  assumeInitial(ClockDomain.current.isResetActive)

  //dut.formalAssumeInputs()
  test_funcs.anyseq_inputs(dut.io)

  if(!check_flush) {
    assume(!dut.io.flush)
  }

  HasFormalProperties.printFormalAssertsReport()
}

class PipelinedMemoryBusBufferFormalTest extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 10

  var configs =
    for(rsp_latency <- Seq(0, 3, 8, 10);
        addressWidth <- Seq(32, 9);
        datawidth <- ((Seq(32, 8)));
        busWidth <- ((Seq(32)));
        cmd_latency <- 0 until 3) yield {
      (s"PMBBufferTest_rl${rsp_latency}_cl${cmd_latency}_bw${busWidth}_dw${datawidth}_aw${addressWidth}",
        () => new PipelinedMemoryBusBufferFormal(UInt(datawidth bits), 400,
          config = PipelinedMemoryBusConfig(addressWidth, busWidth), rsp_latency = rsp_latency, cmd_latency = cmd_latency))
    }

  formalTests().foreach(t => test(t._1) { t._2() })

  override def BMCConfig(): SpinalFormalConfig = formalConfig.withBMC(30)

  override def generateRtl(): Seq[(String, () => Component)] = configs
}
