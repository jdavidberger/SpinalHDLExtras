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
                         address : Topology.address_t,
                         val canonicalPort: Int,
                        ) extends ComponentWithFormalProperties {
  val roundRobinArbitration = cfg.virtualChannelArbitrationPolicy == RoundRobin

  val vcCount = cfg.virtualChannels
  val inputPorts = cfg.topology.nodeInputPortIndicesForCanonicalPorts(address, canonicalPort)
  val connectivityIn = inputPorts.size

  setName(s"allocator_${address}_${cfg.topology.portName(canonicalPort)}")
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

  def routedFlits(port : Topology.canonical_port) = {
    val idx = inputPorts.indexOf(port)
    if (idx != -1) Some(io.routedFlits(idx))
    else None
  }

  io.routedFlits.zip(inputPorts).foreach { case(flit, port) => flit.setName(s"routedFlits_${cfg.topology.portName(port)}")}

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

  val allowed = cfg.topology.allowedTransitionTable(cfg, (address, canonicalPort), candidateCount, vcCount)

  val crossbar = new GrantTableCrossbar(Bits(cfg.dataWidth bits), allowed, roundRobinArbitration, cfg.routingMode)
  when(crossbar.io.activity) {
    io.activity := True
  }

  crossbar.setName(s"crossbar_${cfg.topology.portName(canonicalPort)}")

  for (i <- 0 until connectivityIn; s <- 0 until vcCount) {
    crossbar.io.sources(candidateOf(i, s)) <> io.routedFlits(i)(s)
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
          cfg = NocConfig(topology = new Mesh((4, 3))), 2, o
        ))
      )
  }
}
