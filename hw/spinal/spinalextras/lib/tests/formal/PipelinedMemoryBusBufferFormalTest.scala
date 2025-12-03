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

  def create_config(rsp_latency : Int, addressWidth : Int, datawidth : Int, depth : Int, busWidth : Int, cmd_latency : Int, has_flush : Boolean) = {
    (s"PMBBufferTest_rl${rsp_latency}_depth${depth}_cl${cmd_latency}_bw${busWidth}_dw${datawidth}_aw${addressWidth}_hf${has_flush}",
      () => GeneralFormalDut(() => new PipelinedMemoryBusBuffer(UInt(datawidth bits), depth * datawidth,
        config = PipelinedMemoryBusConfig(addressWidth, busWidth), rsp_latency = rsp_latency,
        cmd_latency = cmd_latency, has_flush = has_flush)))
  }

  val has_flush = true
  var configs : Seq[(String, () => Component)] = {
    (for(rsp_latency <- Seq(0, 10);
        addressWidth <- Seq(32);
        datawidth <- Seq(32);
        depth <- Seq(3, 400);
        busWidth <- Seq(32);
        //has_flush <- Seq(true, false);
        cmd_latency <- Seq(0, 2)) yield {
      create_config(rsp_latency, addressWidth, datawidth, depth, busWidth, cmd_latency, has_flush)
    }) ++ Seq(
      create_config(3, 9, 8, 100, 32, 0, has_flush),
      create_config(2, 9, 32, 100, 32, 0, has_flush)
    )
  }

  formalTests().foreach(t => test(t._1) { t._2() })

  override def CoverConfig(): SpinalFormalConfig = formalConfig.withCover(30)

  override def generateRtl(): Seq[(String, () => Component)] = configs
  //override def generateRtlProve(): Seq[(String, () => Component)] = Seq()
}
