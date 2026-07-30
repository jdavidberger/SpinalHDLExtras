package spinalextras.lib.noc

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._
import spinalextras.lib.formal.ComponentWithFormalProperties
import spinalextras.lib.logging.{FlowLogger, GlobalLogger}
import spinalextras.lib.noc.topology.{Mesh, Ring, Star, Torus, Tree}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

import scala.collection.mutable
import scala.language.postfixOps

class NoC(val cfg: NocConfig) extends ComponentWithFormalProperties {
  val io = new Bundle {
    val inputs = Array.fill(cfg.topology.nodes)(slave(Stream(Fragment(cfg.datatype))))
    val outputs = Array.fill(cfg.topology.nodes)(master(Stream(Fragment(cfg.datatype))))
  }

  io.inputs.zipWithIndex.foreach(x => x._1.setName("input_" + cfg.topology.addressName(x._2)))
  io.outputs.zipWithIndex.foreach(x => x._1.setName("output_" + cfg.topology.addressName(x._2)))

  def sealUnusedPorts(): NoC = {
    io.inputs.filter(_.valid.dlcIsEmpty).foreach(_.setIdle())
    io.outputs.filter(_.ready.dlcIsEmpty).foreach(_.freeRun())
    this
  }

  GlobalLogger(
    Set("noc-headers"),
    FlowLogger.countBeats(
      io.inputs:_*
    ),
    FlowLogger.countBeats(
      io.outputs:_*
    )
  )

  val nodes = cfg.topology.createNodes(this)
}

class NocFormalTester extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 2

  formalTests().foreach(t => test(t._1) {
    t._2()
  })

  override def generateRtl() = {
    for((name, cfg) <- NocConfig.testConfigurations()) yield {
      (name, () => GeneralFormalDut(() => new NoC(cfg.copy(dataWidth = 8)), 1))
    }
  }
}



