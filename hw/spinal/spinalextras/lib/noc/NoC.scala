package spinalextras.lib.noc

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._
import spinal.lib.bus.regif.AccessType.RW
import spinal.lib.bus.regif.BusIf
import spinalextras.lib.formal.ComponentWithFormalProperties
import spinalextras.lib.noc.topology.{Mesh, Ring, Star, Torus, Tree}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

import scala.collection.mutable
import scala.language.postfixOps

class NoC(val cfg: NocConfig) extends ComponentWithFormalProperties {
  val io = new Bundle {
    val inputs = Array.fill(cfg.topology.nodes)(slave(Stream(Fragment(cfg.datatype))))
    val outputs = Array.fill(cfg.topology.nodes)(master(Stream(Fragment(cfg.datatype))))
  }

  def sealUnusedPorts(): Unit = {
    io.inputs.filter(_.valid.dlcIsEmpty).foreach(_.setIdle())
    io.outputs.filter(_.ready.dlcIsEmpty).foreach(_.freeRun())
  }

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



