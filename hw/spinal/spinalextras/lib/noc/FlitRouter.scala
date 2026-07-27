package spinalextras.lib.noc

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._
import spinalextras.lib.logging._
import spinalextras.lib.misc.Optional
import spinalextras.lib.misc.arbitration.{Async, Register, Stall}
import spinalextras.lib.noc.topology.{Mesh, Ring}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

import scala.language.postfixOps

class FlitRouter(val cfg: NocConfig, address: Int, inputPort: Topology.canonical_port) extends Component {
  val outputNodeIndices = cfg.topology.nodePortIndicesForCanonicalPorts(address, inputPort)
  val connectivityOut = outputNodeIndices.size

  val io = new Bundle {
    val input = slave(Stream(Fragment(Bits(cfg.dataWidth bits))))
    val output = Vec(master(Stream(Fragment(Bits(cfg.dataWidth bits)))), connectivityOut)

    val activity = out(Bool())
  }

  io.output.zip(outputNodeIndices).foreach { case(port, canonical_port) => port.setName(s"io_output_${cfg.topology.portName(canonical_port)}")}
  // Already expressed in the connectivityOut-sized, inputPort-excluded
  // numbering -- resolveDestPort guarantees this never points back through
  // inputPort, so no self-redirect or compaction is needed here.
  val outputNode = RegInit(Optional.Empty(UInt(log2Up(connectivityOut) bits)))

  // RoutingMode.Register stages the input through a standard registered
  // Stream pipe before any of the route-decision logic below ever sees it,
  // shortening the combinational path that feeds outputNode -- at the cost
  // of an extra cycle of latency compared to Stall (outputNode still takes
  // its own cycle on top of the stage). Stall and Async both operate
  // directly on io.input.
  val input = cfg.routingMode match {
    case Register => io.input.stage()
    case Stall | Async => io.input
  }

  io.activity := False

  if (cfg.routingMode == Async) {
    val hdr = Header(cfg)
    hdr.assignFromBits(input.payload.fragment)
    val computedDest = cfg.topology.resolveDestPort(hdr.dest, address, inputPort)

    // On the first flit of a new packet (outputNode not yet holding a
    // value), admit it combinationally this same cycle using computedDest,
    // instead of forcing it to wait for outputNode to register that exact
    // same value one cycle later. dest/admit fall back to exactly
    // outputNode.value/outputNode.has_value -- today's behavior -- the
    // moment a decision is actually latched.
    val bypassing = !outputNode.has_value && input.valid
    val dest = outputNode.or_else(computedDest)
    val admit = outputNode.has_value || bypassing

    when(outputNode.has_value) {
      when(input.lastFire) {
        outputNode.clear()
      }
    } elsewhen (input.valid) {
      io.activity := True
      // A single-beat packet fully admitted this same cycle via the bypass
      // never needs outputNode to hold anything -- latching it here would
      // wrongly stall the very next packet at this port behind a stale,
      // already-finished decision.
      when(!(bypassing && input.lastFire)) {
        outputNode.set_value(computedDest)
      }
    }

    when(admit) {
      if (dest.maxValue >= connectivityOut) {
        assert(dest < connectivityOut)
      }
    }

    StreamDemux(input.continueWhen(admit), dest, connectivityOut) <> io.output
  } else {
    // Stall and Register share this same register-gated route decision --
    // Register's only difference is that `input` above is already staged,
    // not raw io.input, so it doesn't need (and doesn't get) any special
    // casing here.
    when(outputNode.has_value) {
      when(input.lastFire) {
        //report(Seq("Finish Address: ", address, " ", cfg.topology.addressName(address), " vcid ", idx))
        outputNode.clear()
      }
    } elsewhen (input.valid) {
      val hdr = Header(cfg)
      hdr.assignFromBits(input.payload.fragment)

      outputNode.set_value(cfg.topology.resolveDestPort(hdr.dest, address, inputPort))
      io.activity := True
      //report(Seq("Start Address: ", address, " ", cfg.topology.addressName(address), " dst ", hdr.dest, " app ", hdr.application, " vcid ", idx, " output ", outputNode))
    }

    when(outputNode.has_value) {
      if (outputNode.value.maxValue >= connectivityOut) {
        assert(outputNode.value < connectivityOut)
      }
    }

    StreamDemux(input.continueWhen(outputNode.has_value), outputNode.value, connectivityOut) <> io.output
  }
}

object FlitRouter {
  def apply(node: RouterNode, inputPort: Topology.canonical_port, vcid: Int, input: Stream[Fragment[Bits]]): Vec[Stream[Fragment[Bits]]] = {
    val router = new FlitRouter(node.cfg, node.address, inputPort = inputPort)
    router.setName(s"flit_router_${node.address}_p${node.cfg.topology.portName(inputPort)}_v${vcid}")
    router.io.input <> input
    router.io.output
  }
}

// Covers all three RoutingMode branches directly (rather than only relying
// on them being exercised incidentally by the larger NoC/RouterNode formal
// suites), across one acyclic (Mesh) and one cyclic (Ring) topology -- the
// generic Stream contract asserted for free on every Stream port (see
// docs/formal.md) is exactly what would catch an Async/Register admission
// path that violates "valid must not be dropped before ready" or "payload
// must stay stable while stalled".
class FlitRouterFormalTester extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 10

  formalTests().foreach(t => test(t._1) {
    t._2()
  })

  override def generateRtl() = {
    for (routingMode <- Seq(Stall, Async, Register);
         (name, topology) <- Seq("Mesh" -> new Mesh((3, 3)), "Ring" -> new Ring(4))) yield
      (s"${name}_${NocConfig.objectName(routingMode)}", () =>
        GeneralFormalDut(() => new FlitRouter(
          NocConfig(topology = topology, routingMode = routingMode), address = 0, inputPort = 0
        ))
      )
  }
}