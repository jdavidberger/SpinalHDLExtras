package spinalextras.lib.misc.arbitration

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim.SimDataPimper
import spinal.lib.misc.aia.APlicGenParam.test
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties, FormalProperty}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

import scala.language.postfixOps

// Matches up to `candidateCount` requesters against `channelCount`
// interchangeable lanes, committing at most one new (lane, candidate)
// pairing per cycle. A pairing is held (grant(v)(c) stays true) until the
// caller signals io.release(v) -- e.g. because whatever was occupying that
// lane finished.
//
// `allowed(v)(c)` restricts which lanes candidate c may ever be granted --
// e.g. to pin a candidate to a single lane (Static), or to reserve a lane so
// only certain candidates may ever reach it (an escape/dateline VC). Use
// GrantTable.allowAll for the previous unrestricted behavior. Its shape
// alone (rows = channels, columns = candidates) determines candidateCount
// and channelCount, so this is the only thing callers need to provide.
class GrantTableArbiter(roundRobinArbitration: Boolean,
                        allowed: Seq[Seq[Boolean]],
                        routingMode: RoutingMode = Stall) extends ComponentWithFormalProperties {
  val channelCount = allowed.length
  val candidateCount = allowed.headOption.map(_.length).getOrElse(0)
  require(allowed.forall(_.length == candidateCount),
    s"allowed rows must all have the same length (candidateCount=$candidateCount)")

  val candidateBits = log2Up(candidateCount)
  val channelBits = log2Up(channelCount)

  val io = new Bundle {
    val request = in Vec(Bool(), candidateCount) // request(c): candidate c wants a lane
    val release = in Vec(Bool(), channelCount) // release(v): lane v's current occupant is done
    val grant = out(new GrantTable(allowed)) // grant(v)(c): lane v is currently serving candidate c

    // RoutingMode.Async only. freshGrant(v)(c): this cycle's newly-decided
    // (lane, candidate) pairing, if any -- combinationally available a full
    // cycle before `grant` would register it. retiredBypass(v): the caller
    // (GrantTableCrossbar) routed freshGrant's pairing on lane v through
    // combinationally this same cycle *and* its stream's last fragment
    // fired -- i.e. the whole transfer started and finished in one cycle,
    // so freshGrant must not be latched into `grant`, or a lane already
    // fully vacated this cycle would stay wrongly held for whatever
    // candidate happens to use it next, skipping a real arbitration round.
    val freshGrant = (routingMode == Async) generate out(new GrantTable(allowed))
    val retiredBypass = (routingMode == Async) generate in Vec(Bool(), channelCount)

    val activity = out(Bool())
  }

  val grant = io.grant.asReg()
  io.request.simPublic()

  def laneBusy(v: Int): Bool = io.grant.laneBusy(v)

  def candidateBusy(c: Int): Bool = io.grant.candidateBusy(c)

  // At most one candidate bit is ever set per lane, so clearing the whole
  // lane is equivalent to clearing just the granted candidate.
  for (v <- 0 until channelCount) {
    when(io.release(v)) {
      grant.clearLane(v)
    }
  }

  // allowedMask(v)(c): compile-time lookup table -- indexed at compile time
  // by lane v, giving a Vec indexable by a runtime candidate index of
  // whether candidate c may use lane v.
  val allowedMask = grant.allowedMask

  val laneSelector = new ChannelSelector(channelCount, roundRobinArbitration = false)
  val candidateSelector = new ChannelSelector(candidateCount, roundRobinArbitration)

  // A candidate is only eligible to be picked (held) if it currently has at
  // least one free, allowed lane -- otherwise candidateSelector could latch
  // onto a candidate that can never be served right now, stalling this
  // entire output port even though some other, presently-servable candidate
  // is waiting.
  for (c <- 0 until candidateCount) {
    val hasFreeAllowedLane = (0 until channelCount)
      .filter(v => grant.allowed(v, c))
      .map(v => !laneBusy(v))
      .foldLeft(False)(_ || _)
    candidateSelector.io.requests(c) := io.request(c) && !candidateBusy(c) && hasFreeAllowedLane
  }

  io.activity := candidateSelector.io.activity

  // Any free lane the held candidate is allowed to use will do -- among
  // those, they're interchangeable -- so no fairness policy is needed for
  // lane selection. allowedMask(v) is a compile-time (lane v fixed) lookup,
  // so indexing it by the runtime chosen candidate gives, for each lane,
  // whether the held candidate may use it.
  val chosenAllowed = Vec.tabulate(channelCount)(v => allowedMask(v)(candidateSelector.io.chosen.payload))
  for (v <- 0 until channelCount) {
    laneSelector.io.requests(v) := !laneBusy(v) && candidateSelector.io.chosen.valid && chosenAllowed(v)
  }

  // Commit the pairing the moment both sides are holding a pick. There is
  // no external backpressure on this join -- once both are valid they fire
  // together unconditionally.
  val bothValid = laneSelector.io.chosen.valid && candidateSelector.io.chosen.valid
  laneSelector.io.chosen.ready := bothValid
  candidateSelector.io.chosen.ready := bothValid

  if (routingMode == Async) {
    io.freshGrant.grant.foreach(_ := False)
    when(bothValid) {
      for ((v, c) <- io.freshGrant.allowedPairs) {
        when(laneSelector.io.chosen.payload === U(v, channelBits bits) &&
          candidateSelector.io.chosen.payload === U(c, candidateBits bits)) {
          io.freshGrant(v, c) := True
        }
      }
    }
  }

  when(bothValid) {
    for (v <- 0 until channelCount; c <- 0 until candidateCount) {
      when(laneSelector.io.chosen.payload === U(v, channelBits bits) &&
        candidateSelector.io.chosen.payload === U(c, candidateBits bits)) {
        if (routingMode == Async) {
          // Started and fully finished this same cycle via the crossbar's
          // combinational fast path -- nothing left to hold, and latching
          // it anyway would leave lane v wrongly occupied next cycle.
          when(!io.retiredBypass(v)) {
            grant.claim(v, c)
          }
        } else {
          grant.claim(v, c)
        }
      }
    }
  }

  def laneBusy(v: UInt): Bool = io.grant.laneBusy(v)

  def candidateBusy(c: UInt): Bool = io.grant.candidateBusy(c)

  // The exclusion properties on io.grant (see GrantTableOutput) constrain
  // `grant` itself, but say nothing about laneSelector/candidateSelector's
  // internal `held` state (exposed via io.chosen) relative to `grant`. That
  // gap is enough for k-induction to posit an unreachable state where a
  // selector is holding an index that `grant` already shows busy -- every
  // property stated elsewhere is still satisfied by such a state, and the
  // very next commit would then double-grant a lane or a candidate. Tying
  // "currently held" back to "currently free in grant" closes that gap.
  override def formalComponentProperties(): Seq[FormalProperty] = new FormalProperties(this) {
    when(laneSelector.io.chosen.valid) {
      addFormalProperty(!laneBusy(laneSelector.io.chosen.payload),
        "a lane currently held by laneSelector must still be free in grant")
    }
    when(candidateSelector.io.chosen.valid) {
      addFormalProperty(!candidateBusy(candidateSelector.io.chosen.payload),
        "a candidate currently held by candidateSelector must still be unassigned in grant")
    }
  }
}


class GrantTableFormalTester extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 20

  formalTests().foreach(t => test(t._1) {
    t._2()
  })

  override def generateRtl() = {
    // GrantTableArbiter's own RoutingMode.Async ports (freshGrant/
    // retiredBypass) are only meaningful wired up to a real GrantTableCrossbar
    // (retiredBypass depends on the actual data streams) -- Async is
    // exercised there (GrantTableCrossbarFormalTester) instead of here.
    (for (rr <- Seq(true, false); candidates <- Seq(1, 2, 5); channels <- Seq(1, 2, 3)) yield
      (s"AllowAll_rr${rr}_c${candidates}_vc${channels}", () =>
        GeneralFormalDut(() => new GrantTableArbiter(rr, GrantTable.allowAll(candidates, channels)))
      )) ++
      (for (rr <- Seq(true, false); channels <- Seq(1, 2, 3); ports <- Seq(1, 2, 3)) yield
        (s"Diagonal_rr${rr}_ports${ports}_vc${channels}", () =>
          GeneralFormalDut(() => new GrantTableArbiter(rr, GrantTable.diagonal(ports * channels, channels)))
        ))
  }
}
