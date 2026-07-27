package spinalextras.lib.noc

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._
import spinalextras.lib.logging._
import spinalextras.lib.misc.Optional
import spinalextras.lib.misc.StreamTools.CreateFragment
import spinalextras.lib.misc.arbitration.{Async, Register, Stall}
import spinalextras.lib.noc.topology.{Mesh, Ring}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

import scala.language.postfixOps

// A flit tagged with its already-resolved destination port. RoutingMode.
// Register uses this to carry the route decision through the exact same
// register that stages the flit's data (input.stage()), rather than giving
// the decision a second, separate register that would stack its own extra
// cycle of latency on top of the stage.
case class DestTaggedFlit(dataWidth: Int, destBits: Int) extends Bundle {
  val datum = Bits(dataWidth bits)
  val dest = UInt(destBits bits)
}

class FlitRouter(val cfg: NocConfig, address: Int, inputPort: Topology.canonical_port) extends Component {
  val outputNodeIndices = cfg.topology.nodePortIndicesForCanonicalPorts(address, inputPort)
  val connectivityOut = outputNodeIndices.size
  val destBits = log2Up(connectivityOut)

  val io = new Bundle {
    val input = slave(Stream(Fragment(Bits(cfg.dataWidth bits))))
    val output = Vec(master(Stream(Fragment(Bits(cfg.dataWidth bits)))), connectivityOut)

    val activity = out(Bool())
  }

  io.output.zip(outputNodeIndices).foreach { case(port, canonical_port) => port.setName(s"io_output_${cfg.topology.portName(canonical_port)}")}
  // Already expressed in the connectivityOut-sized, inputPort-excluded
  // numbering -- resolveDestPort guarantees this never points back through
  // inputPort, so no self-redirect or compaction is needed here.
  val outputNode = RegInit(Optional.Empty(UInt(destBits bits)))
  val input = io.input

  io.activity := False

  val hdr = Header(cfg)
  hdr.assignFromBits(input.payload.fragment)
  // A pure, immediately-available function of the header already sitting
  // on input this cycle -- computed unconditionally since every
  // RoutingMode below needs it. outputNode holds the same value across the
  // rest of a multi-beat packet, so a header only actually needs decoding
  // on the packet's first flit; or_else picks whichever is live.
  val computedDest = cfg.topology.resolveDestPort(hdr.dest, address, inputPort)
  val dest = outputNode.or_else(computedDest)

  if (cfg.routingMode == Register) {
    // Decide (and hold, across a multi-beat packet) the route exactly like
    // Stall does -- outputNode/computedDest/dest above are identical -- but
    // never gate `input` on it. Instead tag each flit with its already-
    // resolved `dest` and let a single input.stage() register carry
    // (payload, dest) through together. That's the only registered delay a
    // flit pays here -- no second, separate register for the decision
    // stacking an extra cycle on top of the stage -- and admission is never
    // stalled waiting on outputNode, since its write below is bookkeeping
    // that happens alongside the flit's own flow, not a gate on it.
    when(outputNode.has_value) {
      when(input.lastFire) {
        outputNode.clear()
      }
    } elsewhen (input.valid) {
      outputNode.set_value(computedDest)
      io.activity := True
    }

    when(input.valid) {
      if (dest.maxValue >= connectivityOut) {
        assert(dest < connectivityOut)
      }
    }

    val tagged = DestTaggedFlit(cfg.dataWidth, destBits)
    tagged.datum := input.payload.fragment
    tagged.dest := dest
    val staged = input.translateWith(CreateFragment(tagged, input.payload.last)).stage()

    val demuxed = StreamDemux(staged, staged.payload.fragment.dest, connectivityOut)
    for ((out, dm) <- io.output.zip(demuxed)) {
      out <> dm.translateWith(CreateFragment(dm.payload.fragment.datum, dm.payload.last))
    }
  } else {
    // Stall and Async share this same admission logic -- the only
    // difference is `bypassing`, which Async computes from outputNode/input
    // and Stall pins to False. With bypassing always False, `admit`
    // reduces to exactly `outputNode.has_value` and the outputNode write
    // below is unconditional, i.e. exactly Stall's original behavior.
    val bypassing = if (cfg.routingMode == Async) !outputNode.has_value && input.valid else False
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
      // already-finished decision. bypassing is always False for Stall, so
      // this always latches unconditionally there, same as before.
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