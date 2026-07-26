package spinalextras.lib.noc

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib.StreamArbiter.{LowerFirst, TransactionLock}
import spinal.lib._
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties, FormalProperty}
import spinalextras.lib.logging.{FlowLogger, GlobalLogger}
import spinalextras.lib.misc.Optional
import spinalextras.lib.misc.StreamTools.CreateFragment
import spinalextras.lib.noc.topology.{Mesh, Ring, Tree}
import spinalextras.lib.noc.virtualchannels.{Dynamic, VirtualIdAllocator}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class RouterNode(val cfg: NocConfig, val address: Int) extends ComponentWithFormalProperties {
  val canonicalOutputPortIndices = cfg.topology.nodePortIndicesForCanonicalPorts(address)
  val canonicalInputPortIndices = cfg.topology.nodePortIndicesForCanonicalPorts(address)

  var connectivityIn : Int = canonicalOutputPortIndices.size
  var connectivityOut : Int = connectivityIn

  val io = new Bundle {
    val inputs = Array.fill(connectivityIn)(slave(Stream(Fragment(Flit(cfg)))))
    val outputs = Vec(master(Stream(Fragment(Flit(cfg)))), connectivityOut)
  }

  def inputs(canonicalPort : Int): Stream[Fragment[Flit]] = {
    io.inputs(cfg.topology.resolveCanonicalInputPort(address, canonicalPort))
  }

  def outputs(canonicalPort : Int): Stream[Fragment[Flit]] = {
    io.outputs(cfg.topology.resolveCanonicalOutputPort(address, canonicalPort))
  }

  val inputPorts = io.inputs.zipWithIndex.map {case (port, idx) => InputPort(port, if (idx == 0) 1 else cfg.virtualChannels)}
  val outputPorts = io.outputs.zipWithIndex.map { case(port, idx) => OutputPort(port, cfg.virtualChannels) }

  outputPorts.zip(canonicalOutputPortIndices).foreach { case(op, p) => op.setName(s"outputPort_${cfg.topology.portName(p)}")}
  inputPorts.zip(canonicalInputPortIndices).foreach { case(op, p) => op.setName(s"inputPort_${cfg.topology.portName(p)}")}

  for (port <- 0 until connectivityIn; vc <- inputPorts(port).io.outputs.indices) {
    val isStart = RegInit(True)
    val portFlowFire = inputPorts(port).io.outputs(vc).toFlowFire
    isStart := portFlowFire.lastFire

    GlobalLogger(
      Set("noc-router", "router-input", s"router-input-${address}", s"router-input-${port}-${vc}"),
      FlowLogger.flows(portFlowFire.takeWhen(isStart).map(x => {
        TupleBundle(x.fragment, U(port, log2Up(connectivityIn) bits), U(vc, cfg.virtualChannelBits bits))
      }).setName(s"noc_input_p${port}_vc${vc}"))
    )
  }

  val allocators = for (p <- canonicalOutputPortIndices) yield new VirtualIdAllocator(
    cfg            = cfg,
    address        = address,
    canonicalPort     = p
  )


  // Maps a destination output port o (o != inputPort) to its slot in the
  // connectivityOut - 1-sized, inputPort-excluded vector FlitRouter produces
  // (see FlitRouter.io.output / Topology.resolveDestPort).
  def destSlot(inputPort: Int, o: Int): Int = if (o < inputPort) o else o - 1

  for ((canonical_port, port_idx) <- canonicalInputPortIndices.zipWithIndex; vcid <- 0 until cfg.virtualChannels) {
    if(vcid < inputPorts(port_idx).virtualChannels) {
      val routed = FlitRouter(this, inputPort = canonical_port, vcid = vcid, input = inputPorts(port_idx).io.outputs(vcid))

      allocators.foreach(allocator =>
        allocator.routedFlits(canonical_port).foreach(
          routed(cfg.topology.resolveCanonicalOutputPort(address, allocator.canonicalPort, canonical_port)) <> _(vcid)
        )
      )
    } else {
      allocators.foreach(allocator => allocator.routedFlits(canonical_port).foreach(
        _(vcid).setIdle()
      ))
    }
  }

  for ((allocator, idx) <- allocators.zipWithIndex; vcid <- 0 until cfg.virtualChannels) {
    allocator.io.allocatedFlits(vcid) <> outputPorts(idx).io.inputs(vcid)
  }
}


class NocRouterFormalTester extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 2

  formalTests().foreach(t => test(t._1) {
    t._2()
  })

  override def generateRtl() = {
    for((name, cfg) <- NocConfig.testConfigurations()) yield
      (name, () => GeneralFormalDut(() => new RouterNode(cfg, 0)))
  }
}


class OutputPort(cfg : NocConfig, val virtualChannels : Int) extends Component {
  val io = new Bundle {
    val inputs = Vec(slave(Stream(Fragment(Flit(cfg)))), virtualChannels)
    val output = master(Stream(Fragment(Flit(cfg))))
  }

  // lowerFirst (or transactionLock) previously let a continuously-ready pool
  // (unescaped) VC starve a bursty escape VC of physical link bandwidth:
  // lowerFirst gives pool static priority outright, and even with roundRobin,
  // transactionLock only re-arbitrates once per whole (multi-beat) packet, so
  // a VC with a much higher duty cycle than its competitor still dominates
  // over many transactions. noLock re-arbitrates every beat instead -- safe
  // here since each flit carries its own vc tag and is redemuxed downstream
  // (see InputPort) -- which is what actually lets a low-duty-cycle transit
  // flow get its fair share of turns.
  if(virtualChannels > 1) {
    StreamArbiterFactory().roundRobin.noLock.on(io.inputs) <> io.output
  } else {
    io.inputs(0) <> io.output
  }
}

object OutputPort {
  def apply(i: Stream[Fragment[Flit]], virtualChannels : Int) = {
    val port = new OutputPort(i.payload.fragment.cfg, virtualChannels)
    port.io.output <> i
    port
  }
}

class InputPort(val cfg : NocConfig, val virtualChannels : Int) extends Component {
  val io = new Bundle {
    val input = slave(Stream(Fragment(Flit(cfg))))

    val outputs =
      Vec(master(Stream(Fragment(Bits(cfg.dataWidth bits)))), virtualChannels)
  }

  val fifos = Array.fill(virtualChannels)(StreamFifo(
    Fragment(Bits(cfg.dataWidth bits)),
    cfg.vcDepth
  ))

  if(virtualChannels == 1) {
    io.input.map(x => CreateFragment(x.datum, x.last)) <> fifos.head.io.push
  } else {
    StreamDemux(io.input, io.input.payload.vc, virtualChannels).zip(fifos).foreach(x => {
      x._1.map(x => CreateFragment(x.datum, x.last)) <> x._2.io.push
    })
  }
  io.outputs.zip(fifos).foreach(x => x._1 <> x._2.io.pop)
}

object InputPort {
  def apply(i: Stream[Fragment[Flit]], virtualChannels : Int) = {
    val port = new InputPort(i.payload.fragment.cfg, virtualChannels)
    port.io.input <> i
    port
  }

  def apply(i: Stream[Fragment[Flit]]): InputPort = apply(i, i.payload.cfg.virtualChannels)
  def apply(cfg : NocConfig): InputPort = new InputPort(cfg, cfg.virtualChannels)
}

class InputPortFormalTester extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 10

  formalTests().foreach(t => test(t._1) {
    t._2()
  })

  override def generateRtl() = {
    Seq(
      (s"Basic", () =>
        GeneralFormalDut(() => InputPort(cfg = NocConfig(topology = new Mesh((4, 3))))))
    )
  }
}