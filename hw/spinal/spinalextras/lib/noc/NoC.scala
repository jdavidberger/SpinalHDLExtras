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

  def sealUnusedPorts(): NoC = {
    io.inputs.filter(_.valid.dlcIsEmpty).foreach(_.setIdle())
    io.outputs.filter(_.ready.dlcIsEmpty).foreach(_.freeRun())
    this
  }

  def configureOutputNode(node : Int, output: Stream[Fragment[Bits]]) = {
    output <> io.outputs(node)
  }

  def configureInputNode(node : Int, input : Stream[Fragment[Bits]], busIf : BusIf) {
    val reg = busIf.newReg(f"${input.name} exit_node")
    val destination = reg.field(UInt(16 bits), RW) init(0)
    configureInputNode(node, input, destination)
  }

  def configureInputNode(node : Int, input: Stream[Fragment[Bits]], destination : UInt): Unit = {
    val header = Header(cfg)
    header.dest := destination.resized
    header.application.setAll()

    input.insertHeader(header.asBits.resized).map(x => {
      val flit = Fragment(cfg.datatype)
      flit.fragment := x.fragment
      flit.last := x.last
      flit
    }) <> io.inputs(node)
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



