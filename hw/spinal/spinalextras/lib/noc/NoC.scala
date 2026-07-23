package spinalextras.lib.noc

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._
import spinal.lib.bus.regif.AccessType.RW
import spinal.lib.bus.regif.BusIf
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties, FormalProperty}
import spinalextras.lib.noc.topology.{Mesh, Ring, Star, Torus, Tree}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

import scala.collection.mutable
import scala.language.postfixOps

class NoC(val cfg: NocConfig) extends ComponentWithFormalProperties {
  val io = new Bundle {
    val inputs = Array.fill(cfg.topology.nodes)(slave(Stream(Fragment(Flit(cfg)))))
    val outputs = Array.fill(cfg.topology.nodes)(master(Stream(Fragment(Flit(cfg)))))
  }

  def sealUnusedPorts(): Unit = {
    io.inputs.filter(_.valid.dlcIsEmpty).foreach(_.setIdle())
    io.outputs.filter(_.ready.dlcIsEmpty).foreach(_.freeRun())
  }

  def configureOutputNode(node : Int, output: Stream[Fragment[Bits]]) = {
    val flitStream = Stream(Fragment(Flit(cfg)))

    val vcidStreams = StreamDemux(flitStream, flitStream.payload.vc, cfg.virtualChannels)

    StreamArbiterFactory().lowerFirst.noLock.on(vcidStreams).map(flit => {
      val p = cloneOf(output.payload)
      p.last := flit.last
      p.fragment := flit.fragment.datum
      p
    }) >> output

    flitStream <> io.outputs(node)
  }

  def configureInputNode(node : Int, input : Stream[Fragment[Bits]], busIf : BusIf) {
    val reg = busIf.newReg(f"${input.name} exit_node")
    val destination = reg.field(UInt(16 bits), RW) init(0)
    val vcid = reg.field(UInt(8 bits), RW) init(0)
    configureInputNode(node, input, destination, vcid)
  }

  def configureInputNode(node : Int, input: Stream[Fragment[Bits]], destination : UInt, vc : UInt = U(0)): Unit = {
    val header = Header(cfg)
    header.dest := destination.resized
    header.application.setAll()

    input.insertHeader(header.asBits.resized).map(x => {
      val flit = Fragment(Flit(cfg))
      flit.datum := x.fragment
      flit.vc := vc.resized
      flit.last := x.last
      flit
    }) <> io.inputs(node)
  }

  val nodes = cfg.topology.createNodes(this)

  // Deadlock-freedom: track every flit-carrying handshake whose blocking could ever be the
  // reason nothing moves -- the NoC's own external ports, each router's inter-node ports, and
  // each router's VC-allocation subsystem boundary (routedFlits in, allocatedFlits out).
  //
  // Deliberately excluded: each router's per-VC FIFO-pop stream (RouterNode's
  // `inputPorts(*).io.outputs`). That stream stalls for exactly one cycle whenever a flit's
  // routing decision is latching (RouterNode.scala's `vc` register, set unconditionally the
  // cycle it sees `vcStreams.valid`), regardless of anything else happening in the NoC.
  // Counting it here would flag that ordinary one-cycle event as a false "deadlock".
  private lazy val formalFlitStreams: Seq[Stream[_]] =
    io.inputs ++ io.outputs ++
      nodes.flatMap(node =>
        node.io.inputs ++ node.io.outputs ++
          node.allocator.io.routedFlits.flatMap(_.toSeq) ++
          node.allocator.io.allocatedFlits.flatMap(_.toSeq)
      )

  private lazy val formalAllOutputsReady = io.outputs.map(_.ready).reduce(_ && _)
  private lazy val formalAnyStalled = formalFlitStreams.map(_.isStall).reduce(_ || _)
  private lazy val formalAnyFired = formalFlitStreams.map(_.fire).reduce(_ || _)

  override def formalComponentProperties(): Seq[FormalProperty] = new FormalProperties(this) {
    addFormalProperty(
      !(formalAllOutputsReady && formalAnyStalled && !formalAnyFired),
      "NoC deadlock: every output ready to accept, some internal link stalled, but nothing anywhere in the NoC is making progress")
  }

  override def covers(): Seq[FormalProperty] = Seq(
    FormalProperty(formalAnyStalled, "an internal link stalls at least once (reachability sanity check, so the assert above isn't vacuous)")
  )
}

trait NocProcessor {
  def connect(input: Stream[Fragment[Flit]], output: Stream[Fragment[Flit]])
}

case class TupleProcessor(input : Stream[Fragment[Flit]], output : Stream[Fragment[Flit]]) extends NocProcessor {
  override def connect(input: Stream[Fragment[Flit]], output: Stream[Fragment[Flit]]): Unit = {
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
  val outputs = new mutable.ArrayBuffer[NocConfig => Stream[Fragment[Flit]]]()
  val inputs = new mutable.ArrayBuffer[NocConfig => Stream[Fragment[Flit]]]()

  def addInput(input: NocConfig => Stream[Fragment[Flit]]): Unit = {
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

      input.insertHeader(header.asBits.resized).map(x => {
        val flit = Fragment(Flit(cfg))
        flit.datum := x.fragment
        flit.vc := vc.resized
        flit.last := x.last
        flit
      })
    })
  }

  def addOutput(output: NocConfig => Stream[Fragment[Flit]]) = {
    outputs.append(output)
  }

  def addBitsOutput(output: Stream[Fragment[Bits]]) = {
    outputs.append(cfg => {
      val flitStream = Stream(Fragment(Flit(cfg)))
      flitStream.map(flit => {
        val p = cloneOf(output.payload)
        p.last := flit.last
        p.fragment := flit.fragment.datum
        p
      }) >> output

      flitStream
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

  override def defaultDepth() = 3

  formalTests().foreach(t => test(t._1) {
    t._2()
  })

  override def generateRtl() = {
    for((name, cfg) <- NocConfig.testConfigurations()) yield {
      (name, () => GeneralFormalDut(() => new NoC(cfg)))
    }
  }
}



