package spinalextras.lib.noc.virtualchannels

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalData, FormalProperties, FormalProperty}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

class GrantTableOutput(candidateCount : Int, vcCount : Int) extends Bundle with FormalData {
  val grant = Vec(Vec(Bool(), candidateCount), vcCount)

  def apply(n : Int) = grant(n)
  /**
   * @return Whether or not the current state of the bundle is valid. Typically either asserted or assumed by a
   *         component which has this bundle as an input or an output.
   *
   *         For complicated properties, consider using the helper class `FormalProperties`
   */
  override def formalIsStateValid(): Seq[FormalProperty] = new FormalProperties() {
    // At most one candidate granted per lane -- io.dests(v) can only ever
    // forward one payload at a time.
    for (v <- 0 until vcCount; c1 <- 0 until candidateCount; c2 <- (c1 + 1) until candidateCount) {
      addFormalProperty(!(grant(v)(c1) && grant(v)(c2)),
        s"grant lane $v must not be granted to more than one candidate at once")
    }

    // At most one lane granted per candidate -- io.sources(c) can only ever
    // be consumed by one lane at a time.
    for (c <- 0 until candidateCount; v1 <- 0 until vcCount; v2 <- (v1 + 1) until vcCount) {
      addFormalProperty(!(grant(v1)(c) && grant(v2)(c)),
        s"candidate $c must not be granted more than one lane at once")
    }

  }
}

// Matches up to `candidateCount` requesters against `vcCount` interchangeable
// lanes, committing at most one new (lane, candidate) pairing per cycle. A
// pairing is held (grant(v)(c) stays true) until the caller signals
// io.release(v) -- e.g. because whatever was occupying that lane finished.
//
// This is deliberately generic (no NoC-specific concepts): candidates and
// lanes are just indices. A NoC-level VC allocator is built by wiring
// (input port, source vc) candidates into one of these per output port.
class GrantTable(candidateCount: Int, vcCount: Int, roundRobinArbitration: Boolean) extends ComponentWithFormalProperties {
  val candidateBits = log2Up(candidateCount)
  val vcBits = log2Up(vcCount)

  val io = new Bundle {
    val request = in Vec(Bool(), candidateCount)                // request(c): candidate c wants a lane
    val release = in Vec(Bool(), vcCount)                       // release(v): lane v's current occupant is done
    val grant   = out (new GrantTableOutput(candidateCount, vcCount)) // grant(v)(c): lane v is currently serving candidate c
  }

  val grant = Vec(Vec(RegInit(False), candidateCount), vcCount)
  io.grant.grant := grant

  def laneBusy(v: Int): Bool = grant(v).reduce(_ || _)
  def candidateBusy(c: Int): Bool = grant.map(_(c)).reduce(_ || _)

  // At most one candidate bit is ever set per lane, so clearing the whole
  // row is equivalent to clearing just the granted candidate.
  for (v <- 0 until vcCount) {
    when(io.release(v)) {
      grant(v).foreach(_ := False)
    }
  }

  // Any free lane will do -- they're interchangeable -- so no fairness
  // policy is needed for lane selection.
  val laneSelector = new VcSelector(vcCount, roundRobinArbitration = false)
  for (v <- 0 until vcCount) laneSelector.io.requests(v) := !laneBusy(v)

  val candidateSelector = new VcSelector(candidateCount, roundRobinArbitration)
  for (c <- 0 until candidateCount) candidateSelector.io.requests(c) := io.request(c) && !candidateBusy(c)

  // Commit the pairing the moment both sides are holding a pick. There is
  // no external backpressure on this join -- once both are valid they fire
  // together unconditionally.
  val bothValid = laneSelector.io.chosen.valid && candidateSelector.io.chosen.valid
  laneSelector.io.chosen.ready := bothValid
  candidateSelector.io.chosen.ready := bothValid

  when(bothValid) {
    for (v <- 0 until vcCount; c <- 0 until candidateCount) {
      when(laneSelector.io.chosen.payload === U(v, vcBits bits) &&
           candidateSelector.io.chosen.payload === U(c, candidateBits bits)) {
        grant(v)(c) := True
      }
    }
  }

  def laneBusy(v: UInt): Bool = grant(v).reduce(_ || _)
  def candidateBusy(c: UInt): Bool = grant.map(_(c)).reduce(_ || _)

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
    for (rr <- Seq(true, false); candidates <- Seq(1, 2, 5); vcs <- Seq(1, 2, 3)) yield
      (s"Basic_rr${rr}_c${candidates}_vc${vcs}", () =>
        GeneralFormalDut(() => new GrantTable(candidates, vcs, rr))
      )
  }
}
