package spinalextras.lib.noc.virtualchannels

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties, FormalProperty}
import spinalextras.lib.misc.arbitration.{GrantTable, GrantTableArbiter, GrantTableCrossbar}
import spinalextras.lib.noc.topology.Mesh
import spinalextras.lib.noc.{Flit, NocConfig, RoundRobin, RoutedFlit, Topology}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

import scala.collection.mutable.ArrayBuffer

class VirtualIdAllocator(val cfg: NocConfig,
                         connectivityIn: Int,
                         connectivityOut: Int,
                         address : Topology.address_t,
                        ) extends ComponentWithFormalProperties {
  val roundRobinArbitration = cfg.virtualChannelArbitrationPolicy == RoundRobin

  val io = new Bundle {
    // One inbound stream per (input port, source vc lane). Each flit
    // carries routedNode: the output port its packet is headed to, resolved
    // once at packet start from the header.
    val routedFlits = Vec(
      Vec(slave(Stream(Fragment(RoutedFlit(cfg, connectivityOut)))), cfg.virtualChannels),
      connectivityIn
    )

    // One outbound stream per (output port, destination vc lane) -- the
    // actual per-output-link resource being allocated. A vc lane is a
    // shared physical-link resource, not a per-input-port one, so this is
    // indexed by output port, not input port.
    val allocatedFlits = Vec(
      Vec(master(Stream(Fragment(Flit(cfg)))), cfg.virtualChannels),
      connectivityOut
    )

    val activity = out(Bool())
  }

  val vcCount = cfg.virtualChannels

  def retag(rf: Fragment[RoutedFlit], v: Int): Fragment[Flit] = {
    val f = Fragment(Flit(cfg))
    f.last := rf.last
    f.fragment.datum := rf.fragment.flit.datum
    f.fragment.vc := U(v, cfg.virtualChannelBits bits)
    f
  }

  // Demux each physical (input port, source vc) candidate by its packet's
  // destination output port. demuxed(i)(s)(o) is then a genuine,
  // independently-owned Stream -- valid only when this candidate is both
  // present and actually headed to o -- so it can be wired straight into
  // that output's VcRouter, with StreamDemux itself (not us) responsible
  // for the valid gating and ready routing being correct.
  val demuxed = Array.tabulate(connectivityIn, vcCount) { (i, s) =>
    StreamDemux(io.routedFlits(i)(s), io.routedFlits(i)(s).payload.fragment.routedNode, connectivityOut)
  }
  io.activity := False

  val candidateCount = connectivityIn * vcCount
  def candidateOf(i: Int, s: Int): Int = i * vcCount + s

  // Physical input port 0 is always local injection (Topology.createNodes
  // wires it directly, bypassing canonical-port resolution used for every
  // inter-router link) -- so candidates sourced from it are local, and every
  // other input port is transit (relayed from a neighboring router).
  val LocalInputPort = 0

  val outputCrossbars = for (o <- 0 until connectivityOut) yield new Area {
    val canonical_port = cfg.topology.nodePortIndicesForCanonicalPorts(address)(o)
    val allowed = cfg.topology.allowedTransitionTable(cfg, (address, canonical_port), candidateCount, vcCount)

    val crossbar = new GrantTableCrossbar(RoutedFlit(cfg, connectivityOut), allowed, roundRobinArbitration)
    when(crossbar.io.activity) {
      io.activity := True
    }

    crossbar.setName(s"crossbar_o${o}")

    for (i <- 0 until connectivityIn; s <- 0 until vcCount) {
      crossbar.io.sources(candidateOf(i, s)) <> demuxed(i)(s)(o)
    }
    for (v <- 0 until vcCount) {
      crossbar.io.dests(v).map(retag(_, v)) <> io.allocatedFlits(o)(v)
    }
  }
}


class VirtualIdAllocatorFormalTester extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 10

  formalTests().foreach(t => test(t._1) {
    t._2()
  })

  override def generateRtl() = {
    for (rr <- Seq(true, false); dynamic <- Seq(true, false)) yield
      (s"Basic_rr${rr}_dyn${dynamic}", () =>
        GeneralFormalDut(() => new VirtualIdAllocator(
          cfg = NocConfig(topology = new Mesh((4, 3))), 2, 2, 0
        ))
      )
  }
}
