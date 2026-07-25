package spinalextras.lib.noc.virtualchannels

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties, FormalProperty}
import spinalextras.lib.misc.arbitration.{GrantTable, GrantTableArbiter, GrantTableCrossbar}
import spinalextras.lib.noc.topology.Mesh
import spinalextras.lib.noc.{Flit, NocConfig, RoundRobin, Topology}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

import scala.collection.mutable.ArrayBuffer

// Allocates the shared physical-link resource (one crossbar of vc lanes) for
// a single output port -- one instance per (router, output port), not one
// instance covering every output port of a router. Each output port's vc
// lanes are an independent arbitration domain, so there is no shared state
// to gain by bundling multiple outputs into one component.
class VirtualIdAllocator(val cfg: NocConfig,
                         connectivityIn: Int,
                         address : Topology.address_t,
                         outputPort: Int,
                        ) extends ComponentWithFormalProperties {
  val roundRobinArbitration = cfg.virtualChannelArbitrationPolicy == RoundRobin

  val vcCount = cfg.virtualChannels

  val io = new Bundle {
    // One inbound stream per (input port, source vc lane) targeting this
    // allocator's output port -- FlitRouter already demuxed by resolved
    // destination, so no routedNode tag needs to ride along. The slot at
    // inputPort == outputPort doesn't exist (a port never routes back
    // through itself) and is left permanently idle.
    val routedFlits = Vec(
      Vec(slave(Stream(Fragment(Bits(cfg.dataWidth bits)))), vcCount),
      connectivityIn
    )

    // One outbound stream per destination vc lane -- the actual
    // per-output-link resource being allocated.
    val allocatedFlits = Vec(master(Stream(Fragment(Flit(cfg)))), vcCount)

    val activity = out(Bool())
  }

  def retag(rf: Fragment[Bits], v: Int): Fragment[Flit] = {
    val f = Fragment(Flit(cfg))
    f.last := rf.last
    f.fragment.datum := rf.fragment
    f.fragment.vc := U(v, cfg.virtualChannelBits bits)
    f
  }

  io.activity := False

  val candidateCount = connectivityIn * vcCount
  def candidateOf(i: Int, s: Int): Int = i * vcCount + s

  // Physical input port 0 is always local injection (Topology.createNodes
  // wires it directly, bypassing canonical-port resolution used for every
  // inter-router link) -- so candidates sourced from it are local, and every
  // other input port is transit (relayed from a neighboring router).
  val LocalInputPort = 0

  val canonical_port = cfg.topology.nodePortIndicesForCanonicalPorts(address)(outputPort)
  val allowed = cfg.topology.allowedTransitionTable(cfg, (address, canonical_port), candidateCount, vcCount)

  val crossbar = new GrantTableCrossbar(Bits(cfg.dataWidth bits), allowed, roundRobinArbitration)
  when(crossbar.io.activity) {
    io.activity := True
  }

  crossbar.setName(s"crossbar_o${outputPort}")

  for (i <- 0 until connectivityIn; s <- 0 until vcCount) {
    val source = crossbar.io.sources(candidateOf(i, s))
    if (i == outputPort) {
      // A port never routes back through itself, so no routedFlits slot
      // exists for this candidate; leave it permanently idle.
      source.setIdle()
    } else {
      source <> io.routedFlits(i)(s)
    }
  }
  for (v <- 0 until vcCount) {
    crossbar.io.dests(v).map(retag(_, v)) <> io.allocatedFlits(v)
  }
}


class VirtualIdAllocatorFormalTester extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 10

  formalTests().foreach(t => test(t._1) {
    t._2()
  })

  override def generateRtl() = {
    val connectivityOut = 2
    for (rr <- Seq(true, false); dynamic <- Seq(true, false); o <- 0 until connectivityOut) yield
      (s"Basic_rr${rr}_dyn${dynamic}_o${o}", () =>
        GeneralFormalDut(() => new VirtualIdAllocator(
          cfg = NocConfig(topology = new Mesh((4, 3))), 2, 0, o
        ))
      )
  }
}
