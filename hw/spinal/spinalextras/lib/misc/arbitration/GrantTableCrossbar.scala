package spinalextras.lib.misc.arbitration

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties, FormalProperty}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

// An N:M crossbar switch: candidateCount source streams contend for
// channelCount dest lanes under a GrantTableArbiter, and the winning
// pairings are carried through by a GrantTableStreamRouter -- request/release
// are derived straight from the sources'/dests' own stream handshaking, so
// callers see nothing but plain stream IO, no grant matrix or arbiter
// bookkeeping to wire up themselves.
class GrantTableCrossbar[T <: Data](payloadType: HardType[T],
                                    allowed: Seq[Seq[Boolean]],
                                    roundRobinArbitration: Boolean,
                                    routingMode: RoutingMode = Stall) extends ComponentWithFormalProperties {
  val channelCount = allowed.length
  val candidateCount = allowed.headOption.map(_.length).getOrElse(0)

  val io = new Bundle {
    val sources = Vec(slave(Stream(Fragment(payloadType))), candidateCount)
    val dests   = Vec(master(Stream(Fragment(payloadType))), channelCount)

    val activity = out(Bool())
  }

  // RoutingMode.Register stages every source through a standard registered
  // Stream pipe before router (below) ever sees it -- but the arbiter's own
  // request/grant decision-making runs off the live io.sources (below),
  // not this staged copy, so arbitration starts immediately rather than
  // waiting an extra cycle for staging to finish first. The arbiter's own
  // multi-cycle latency to actually decide a winner is unaffected either
  // way; staging just means that decision no longer has to gate
  // io.sources.ready while it's pending. Stall and Async both operate
  // directly on io.sources for the data path too.
  val sources = routingMode match {
    case Register => Vec(io.sources.map(_.stage()))
    case Stall | Async => io.sources
  }

  val arbiter = new GrantTableArbiter(roundRobinArbitration, allowed, routingMode)
  io.activity := arbiter.io.activity

  for (c <- 0 until candidateCount) {
    arbiter.io.request(c) := io.sources(c).valid
  }
  for (v <- 0 until channelCount) {
    arbiter.io.release(v) := io.dests(v).lastFire
  }

  if (routingMode == Async) {
    // arbiter.io.grant only reflects a fresh (lane, candidate) pairing one
    // cycle after laneSelector/candidateSelector already agreed on it
    // (arbiter.io.freshGrant, available combinationally that same cycle).
    // Route through the combination of the two immediately instead of
    // waiting a cycle for grant to register the pairing; grant and
    // freshGrant can never both be set for the same pairing (freshGrant
    // only fires for a (lane, candidate) pair that was, this same cycle,
    // still free in grant), so ORing them together can't create a double
    // grant.
    val effectiveGrant = new GrantTable(allowed)
    for ((v, c) <- effectiveGrant.allowedPairs) {
      effectiveGrant(v, c) := arbiter.io.grant(v, c) || arbiter.io.freshGrant(v, c)
    }

    val router = effectiveGrant.createRouter(payloadType)
    router.io.sources <> sources
    router.io.dests <> io.dests

    // Tell the arbiter, per lane, whether the pairing it freshly decided
    // this cycle also fully retires (its stream's last fragment fires)
    // this same cycle -- see the comment on io.retiredBypass in
    // GrantTableArbiter.
    for (v <- 0 until channelCount) {
      arbiter.io.retiredBypass(v) := arbiter.io.freshGrant.laneBusy(v) && io.dests(v).lastFire
    }
  } else {
    val router = arbiter.io.grant.createRouter(payloadType)
    router.io.sources <> sources
    router.io.dests <> io.dests
  }

  override def formalComponentProperties(): Seq[FormalProperty] = new FormalProperties(this) {
    for (i <- 0 until candidateCount; s <- 0 until channelCount) {
      when(arbiter.candidateSelector.io.chosen.valid &&
        arbiter.candidateSelector.io.chosen.payload === U(i, arbiter.candidateBits bits)) {
        addFormalProperty(io.sources(i).valid,
          s"a candidate held by a GrantTable's candidateSelector must still be valid on its backing stream")
      }
    }
  }
}

class GrantTableCrossbarFormalTester extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 20

  formalTests().foreach(t => test(t._1) {
    t._2()
  })

  override def generateRtl() = {
    (for (rr <- Seq(true, false); routingMode <- Seq(Stall, Async, Register); candidates <- Seq(1, 2, 5); channels <- Seq(1, 2, 3)) yield
      (s"AllowAll_rr${rr}_${routingMode}_c${candidates}_vc${channels}", () =>
        GeneralFormalDut(() => new GrantTableCrossbar(Bits(4 bits), GrantTable.allowAll(candidates, channels), rr, routingMode))
      )) ++
      (for (rr <- Seq(true, false); routingMode <- Seq(Stall, Async, Register); channels <- Seq(1, 2, 3); ports <- Seq(1, 2, 3)) yield
        (s"Diagonal_rr${rr}_${routingMode}_ports${ports}_vc${channels}", () =>
          GeneralFormalDut(() => new GrantTableCrossbar(Bits(4 bits), GrantTable.diagonal(ports * channels, channels), rr, routingMode))
        ))
  }
}
