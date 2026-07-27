package spinalextras.lib.noc

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._
import spinalextras.lib.logging._
import spinalextras.lib.misc.Optional
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
  val input = io.input

  io.activity := False

  if (!cfg.pipelineBypass) {
    // Unchanged from before pipelineBypass existed: outputNode only ever
    // takes effect one cycle after it's set, so a new packet's first flit
    // always pays an unconditional 1-cycle bubble at this hop before it can
    // move, even though the destination is already fully determined by the
    // header sitting on input this same cycle. See the pipelineBypass
    // branch below for the same-cycle fast path around that bubble.
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
  } else {
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

// Covers both branches of the pipelineBypass if/else directly (rather than
// only relying on it being exercised incidentally by the larger NoC/
// RouterNode formal suites), across one acyclic (Mesh) and one cyclic
// (Ring) topology -- the generic Stream contract asserted for free on every
// Stream port (see docs/formal.md) is exactly what would catch a bypass
// that violates "valid must not be dropped before ready" or "payload must
// stay stable while stalled", which is the specific hazard the bypass's
// same-cycle admission has to avoid.
class FlitRouterFormalTester extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 10

  formalTests().foreach(t => test(t._1) {
    t._2()
  })

  override def generateRtl() = {
    for (pipelineBypass <- Seq(false, true);
         (name, topology) <- Seq("Mesh" -> new Mesh((3, 3)), "Ring" -> new Ring(4))) yield
      (s"${name}_bypass${pipelineBypass}", () =>
        GeneralFormalDut(() => new FlitRouter(
          NocConfig(topology = topology, pipelineBypass = pipelineBypass), address = 0, inputPort = 0
        ))
      )
  }
}