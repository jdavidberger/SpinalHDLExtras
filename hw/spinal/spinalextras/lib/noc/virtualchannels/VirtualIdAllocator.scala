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

class VirtualIdAllocator(val cfg: NocConfig,
                         connectivityIn: Int,
                         connectivityOut: Int,
                         address : Topology.address_t,
                        ) extends ComponentWithFormalProperties {
  val roundRobinArbitration = cfg.virtualChannelArbitrationPolicy == RoundRobin

  val io = new Bundle {
    // One inbound stream per (input port, source vc lane, destination output
    // port). The destination dimension is sized connectivityOut - 1 and
    // excludes the input port itself -- FlitRouter already demuxed by
    // resolved destination, so no routedNode tag needs to ride along.
    val routedFlits = Vec(
      Vec(Vec(slave(Stream(Fragment(Bits(cfg.dataWidth bits)))), connectivityOut - 1), cfg.virtualChannels),
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

  def retag(rf: Fragment[Bits], v: Int): Fragment[Flit] = {
    val f = Fragment(Flit(cfg))
    f.last := rf.last
    f.fragment.datum := rf.fragment
    f.fragment.vc := U(v, cfg.virtualChannelBits bits)
    f
  }

  // Maps a destination output port o (o != i) to its slot in the
  // connectivityOut - 1-sized io.routedFlits(i)(s) vector, which FlitRouter
  // populated by dropping input port i's own slot from the numbering.
  def destSlot(i: Int, o: Int): Int = if (o < i) o else o - 1

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

    val crossbar = new GrantTableCrossbar(Bits(cfg.dataWidth bits), allowed, roundRobinArbitration)
    when(crossbar.io.activity) {
      io.activity := True
    }

    crossbar.setName(s"crossbar_o${o}")

    for (i <- 0 until connectivityIn; s <- 0 until vcCount) {
      val source = crossbar.io.sources(candidateOf(i, s))
      if (i == o) {
        // A port never routes back through itself, so no routedFlits slot
        // exists for this candidate at this output; leave it permanently idle.
        source.setIdle()
      } else {
        source <> io.routedFlits(i)(s)(destSlot(i, o))
      }
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
