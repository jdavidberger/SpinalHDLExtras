package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal._
import spinal.lib.bus.simple._
import spinalextras.lib.formal.HasFormalProperties
import spinalextras.lib.memory.PipelinedMemoryBusBuffer
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut, test_funcs}

class PipelinedMemoryBusBufferFormalTest extends AnyFunSuite with FormalTestSuite {
  override def defaultDepth() = 5

  val has_flush = true
  var configs =
    for(rsp_latency <- Seq(0, 2, 10);
        addressWidth <- Seq(32, 9);
        datawidth <- Seq(32, 8);
        depth <- Seq(3, 400);
        busWidth <- Seq(32);
        //has_flush <- Seq(true, false);
        cmd_latency <- 0 until 3) yield {
      (s"PMBBufferTest_rl${rsp_latency}_depth${depth}_cl${cmd_latency}_bw${busWidth}_dw${datawidth}_aw${addressWidth}_hf${has_flush}",
        () => GeneralFormalDut(() => new PipelinedMemoryBusBuffer(UInt(datawidth bits), depth * datawidth,
          config = PipelinedMemoryBusConfig(addressWidth, busWidth), rsp_latency = rsp_latency,
          cmd_latency = cmd_latency, has_flush = has_flush)))
    }

  formalTests().foreach(t => test(t._1) { t._2() })

  override def CoverConfig(): SpinalFormalConfig = formalConfig.withCover(30)

  override def generateRtl(): Seq[(String, () => Component)] = configs
  //override def generateRtlProve(): Seq[(String, () => Component)] = Seq()
}
