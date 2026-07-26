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

trait NocProcessor {
  def connect(input: Stream[Fragment[Bits]], output: Stream[Fragment[Bits]])
}

case class TupleProcessor(input : Stream[Fragment[Bits]], output : Stream[Fragment[Bits]]) extends NocProcessor {
  override def connect(input: Stream[Fragment[Bits]], output: Stream[Fragment[Bits]]): Unit = {
    if(this.input != null) {
      this.input <> input
    } else {
      input.setBlocked()
    }

    if(this.output != null) {
      this.output <> output
    } else {
      output.setBlocked()
    }
  }
}

object NoC {
  def apply(processors: Seq[NocProcessor], cfg: NocConfig): NoC = {
    val _cfg = cfg.copy(topology = cfg.topology.sizeFor(processors.size))
    val noc = new NoC(_cfg)
    processors.zipWithIndex.foreach { case (p, idx) =>
      p.connect(noc.io.inputs(idx), noc.io.outputs(idx))
    }

    noc
  }
}

class NoCDesign(cfg : NocConfig) {
  val outputs = new mutable.ArrayBuffer[NocConfig => Stream[Fragment[Bits]]]()
  val inputs = new mutable.ArrayBuffer[NocConfig => Stream[Fragment[Bits]]]()

  def addInput(input: NocConfig => Stream[Fragment[Bits]]): Unit = {
    inputs.append(input)
  }

  def addBitsInput(input: Stream[Fragment[Bits]], busIf : BusIf) {
    val reg = busIf.newReg(f"${input.name} exit_node")
    val dest = reg.field(UInt(16 bits), RW) init(0)
    val vcid = reg.field(UInt(8 bits), RW) init(0)
    addBitsInput(input, dest, vcid)
  }

  def addBitsInput(input: Stream[Fragment[Bits]], exit_node : UInt, vc : UInt = U(0)): Unit = {
    inputs.append(cfg => {
      val header = Header(cfg)
      header.dest := exit_node.resized

      input.insertHeader(header.asBits.resized)
    })
  }

  def addOutput(output: NocConfig => Stream[Fragment[Bits]]) = {
    outputs.append(output)
  }

  def addBitsOutput(output: Stream[Fragment[Bits]]) = {
    outputs.append(cfg => {
      output
    })
  }

  def create() : NoC = {
    val _cfg = cfg.copy(topology = cfg.topology.sizeFor(Math.max(inputs.size, outputs.size)))

    val processors = inputs.zipAll(outputs, null, null).map(x => new TupleProcessor(
      if (x._1 == null) null else x._1(_cfg),
      if (x._2 == null) null else x._2(_cfg))
    )

    NoC(processors, _cfg)
  }
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



