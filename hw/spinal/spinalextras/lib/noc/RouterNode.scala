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

case class RoutedFlit(cfg : NocConfig, connectivityOut : Int) extends Bundle {
  val flit = Flit(cfg)
  val routedNode = UInt(log2Up(connectivityOut) bits)
}





class RouterNode(val cfg: NocConfig, val address: Int) extends ComponentWithFormalProperties {
  var connectivityIn : Int = cfg.topology.nodePortIndicesForCanonicalPorts(address).size
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

  val inputPorts = io.inputs.map(InputPort(_))
  val outputPorts = io.outputs.map(OutputPort(_))

  for (port <- 0 until connectivityIn; vc <- 0 until cfg.virtualChannels) {
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

  // Allocate one downstream (destNode, destVc) slot per packet and route the
  // granted flits to the matching output port. This replaces the old
  // Static/Dynamic VcMap, which only arbitrated per-VC without any output-slot
  // locking.
  val allocator = new VirtualIdAllocator(
    cfg          = cfg,
    connectivityIn  = connectivityIn,
    connectivityOut = connectivityOut,
    dynamicAllocation     = cfg.virtualChannelMode == Dynamic,
    roundRobinArbitration = cfg.virtualChannelArbitrationPolicy == RoundRobin
  )

  val routerActivity = Vec(Vec(Bool(), connectivityIn), cfg.virtualChannels)
  for (inputPort <- 0 until connectivityIn; vcid <- 0 until cfg.virtualChannels) {
    FlitRouter(this, inputPort = inputPort, vcid = vcid, input = inputPorts(inputPort).io.outputs(vcid)) <>
      allocator.io.routedFlits(inputPort)(vcid)
  }

  for (o <- 0 until connectivityOut; vcid <- 0 until cfg.virtualChannels) {
    allocator.io.allocatedFlits(o)(vcid) <> outputPorts(o).io.inputs(vcid)
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


class OutputPort(cfg : NocConfig) extends Component {
  val io = new Bundle {
    val inputs = Vec(slave(Stream(Fragment(Flit(cfg)))), cfg.virtualChannels)
    val output = master(Stream(Fragment(Flit(cfg))))
  }

  StreamArbiterFactory().lowerFirst.transactionLock.on(io.inputs) <> io.output
}

object OutputPort {
  def apply(i: Stream[Fragment[Flit]]) = {
    val port = new OutputPort(i.payload.fragment.cfg)
    port.io.output <> i
    port
  }
}

class InputPort(cfg : NocConfig) extends Component {
  val io = new Bundle {
    val input = slave(Stream(Fragment(Flit(cfg))))

    val outputs =
      Vec(master(Stream(Fragment(Bits(cfg.dataWidth bits)))), cfg.virtualChannels)
  }

  val fifos = Array.fill(cfg.virtualChannels)(StreamFifo(
    Fragment(Bits(cfg.dataWidth bits)),
    cfg.vcDepth
  ))

  StreamDemux(io.input, io.input.payload.vc, cfg.virtualChannels).zip(fifos).foreach(x => {
    x._1.map(x => CreateFragment(x.datum, x.last)) <> x._2.io.push
  })
  io.outputs.zip(fifos).foreach(x => x._1 <> x._2.io.pop)
}

object InputPort {
  def apply(i: Stream[Fragment[Flit]]) = {
    val port = new InputPort(i.payload.fragment.cfg)
    port.io.input <> i
    port
  }
}

class InputPortFormalTester extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 10

  formalTests().foreach(t => test(t._1) {
    t._2()
  })

  override def generateRtl() = {
    Seq(
      (s"Basic", () =>
        GeneralFormalDut(() => new InputPort(cfg = NocConfig(topology = new Mesh((4, 3))))))
    )
  }
}