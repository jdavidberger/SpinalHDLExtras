package spinalextras.lib.noc

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._
import spinal.lib.bus.regif.AccessType.RW
import spinal.lib.bus.regif.BusIf
import spinalextras.lib.noc.topology.{Mesh, Ring, Star, Torus, Tree}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

import scala.collection.mutable
import scala.language.postfixOps

class NoC(val cfg: NocConfig) extends Component {
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

  override def defaultDepth() = 10

  formalTests().foreach(t => test(t._1) {
    t._2()
  })

  override def generateRtl() = {
    Seq(
      (s"BasicStar", () => GeneralFormalDut(() => new NoC(cfg = NocConfig(Star(8))))),
      (s"BasicTorus", () => GeneralFormalDut(() => new NoC(cfg = NocConfig(new Torus((3, 2)))))),
      (s"BasicTree", () => GeneralFormalDut(() => new NoC(cfg = NocConfig(new Tree(10, 2))))),
      (s"BasicRing", () => GeneralFormalDut(() => new NoC(cfg = NocConfig(new Ring(6))))),
      (s"BasicMesh", () => GeneralFormalDut(() => new NoC(cfg = NocConfig(new Mesh((3, 2)))))),
    )
  }
}



