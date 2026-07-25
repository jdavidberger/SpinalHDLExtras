package spinalextras.lib.noc.virtualchannels

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib.{CountOne, Flow, master}
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalData, FormalProperties, FormalProperty}
import spinalextras.lib.logging.{FlowLogger, GlobalLogger}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

class GrantTableOutput(candidateCount : Int, vcCount : Int, allowed : Seq[Seq[Boolean]]) extends Bundle with FormalData {
  // allowed(v)(c) -- v-major, same layout as GrantTable's own `grant` state
  // -- restricts which (lane, candidate) pairings can ever be granted. Only
  // those pairings get a real wire here; every other (v, c) combination is a
  // compile-time False, since GrantTable can never set it.
  val allowedPairs: Seq[(Int, Int)] =
    for (v <- 0 until vcCount; c <- 0 until candidateCount if allowed(v)(c)) yield (v, c)
  private val pairIndex: Map[(Int, Int), Int] = allowedPairs.zipWithIndex.toMap

  val grant = Vec(Bool(), allowedPairs.length)

  def apply(v : Int, c : Int): Bool = pairIndex.get((v, c)) match {
    case Some(i) => grant(i)
    case None => False
  }
  def allowed(v : Int, c : Int) : Boolean = allowed(v)(c)

  def laneBusy(v: Int): Bool =
    allowedPairs.zipWithIndex.collect { case ((vv, _), i) if vv == v => grant(i) }.foldLeft(False: Bool)(_ || _)
  def candidateBusy(c: Int): Bool =
    allowedPairs.zipWithIndex.collect { case ((_, cc), i) if cc == c => grant(i) }.foldLeft(False: Bool)(_ || _)
  def laneBusy(v: UInt): Bool = Vec.tabulate(vcCount)(vv => laneBusy(vv))(v)
  def candidateBusy(c: UInt): Bool = Vec.tabulate(candidateCount)(cc => candidateBusy(cc))(c)

  def init(): this.type = {
    grant.foreach(_.init(False))
    this
  }

  def asReg(): GrantTableOutput = {
    val r = Reg(new GrantTableOutput(candidateCount, vcCount, allowed)).init()
    this <> r
    r
  }

  def allowedMask = Vec(allowed.map(row => Vec(row.map(Bool(_)))))

  def clearLane(v: Int): Unit =
    allowedPairs.zipWithIndex.foreach { case ((vv, _), i) => if (vv == v) grant(i) := False }

  def claim(v: Int, c: Int): Unit = pairIndex.get((v, c)).foreach(i => grant(i) := True)

  /**
   * @return Whether or not the current state of the bundle is valid. Typically either asserted or assumed by a
   *         component which has this bundle as an input or an output.
   *
   *         For complicated properties, consider using the helper class `FormalProperties`
   */
  override def formalIsStateValid(): Seq[FormalProperty] = new FormalProperties() {
    // At most one candidate granted per lane -- io.dests(v) can only ever
    // forward one payload at a time.
    for (v <- 0 until vcCount) {
      val bits = allowedPairs.zipWithIndex.collect { case ((vv, _), i) if vv == v => grant(i) }
      addFormalProperty(CountOne(bits) <= 1,
        s"grant lane $v must not be granted to more than one candidate at once")
    }

    // At most one lane granted per candidate -- io.sources(c) can only ever
    // be consumed by one lane at a time.
    for (c <- 0 until candidateCount) {
      val bits = allowedPairs.zipWithIndex.collect { case ((_, cc), i) if cc == c => grant(i) }
      addFormalProperty(CountOne(bits) <= 1,
        s"candidate $c must not be granted more than one lane at once")
    }

  }
}

object GrantTable {
  // No restriction: any candidate may be granted any lane -- fully
  // adaptive/Dynamic behavior. v-major (allowed(v)(c)), matching GrantTable's
  // own `grant` state layout.
  def allowAll(candidateCount: Int, vcCount: Int): Seq[Seq[Boolean]] =
    Seq.fill(vcCount)(Seq.fill(candidateCount)(true))

  // candidateCount must be a multiple of vcCount, laid out as
  // candidateOf(i, s) = i * vcCount + s (see VirtualIdAllocator): candidate
  // c may only ever be granted lane (c % vcCount), i.e. destVc is pinned to
  // the candidate's own source-vc slot and never reassigned -- Static
  // behavior.
  def diagonal(candidateCount: Int, vcCount: Int): Seq[Seq[Boolean]] =
    Seq.tabulate(vcCount, candidateCount) { (v, c) => (c % vcCount) == v }
}

// Matches up to `candidateCount` requesters against `vcCount` interchangeable
// lanes, committing at most one new (lane, candidate) pairing per cycle. A
// pairing is held (grant(v)(c) stays true) until the caller signals
// io.release(v) -- e.g. because whatever was occupying that lane finished.
//
// `allowed(v)(c)` restricts which lanes candidate c may ever be granted --
// e.g. to pin a candidate to a single lane (Static), or to reserve a lane so
// only certain candidates may ever reach it (an escape/dateline VC). Use
// GrantTable.allowAll for the previous unrestricted behavior.
//
// `highPriority(c)`: candidateSelector only ever considers low-priority
// candidates when no high-priority candidate is currently eligible. This is
// a hard priority, not weighted fairness -- e.g. a NoC allocator uses it to
// starve local injection in favor of transit traffic already in the
// network, since only the latter is needed for the network to keep
// draining. Defaults to all-true (no effect) for callers that don't need it.
//
// This is deliberately generic (no NoC-specific concepts): candidates and
// lanes are just indices. A NoC-level VC allocator is built by wiring
// (input port, source vc) candidates into one of these per output port.
class GrantTable(candidateCount: Int, vcCount: Int, roundRobinArbitration: Boolean,
                  allowed: Seq[Seq[Boolean]]) extends ComponentWithFormalProperties {
  require(allowed.length == vcCount && allowed.forall(_.length == candidateCount),
    s"allowed must be a $vcCount x $candidateCount matrix")
  // Constructor default values can't reference other constructor params
  // (Scala restriction), hence the Nil-sentinel indirection here.

  val candidateBits = log2Up(candidateCount)
  val vcBits = log2Up(vcCount)

  val io = new Bundle {
    val request = in Vec(Bool(), candidateCount)                // request(c): candidate c wants a lane
    val release = in Vec(Bool(), vcCount)                       // release(v): lane v's current occupant is done
    val grant   = out (new GrantTableOutput(candidateCount, vcCount, allowed)) // grant(v)(c): lane v is currently serving candidate c

    val activity = out (Bool())
  }

  val grant = io.grant.asReg()
  io.request.simPublic()

  def laneBusy(v: Int): Bool = io.grant.laneBusy(v)
  def candidateBusy(c: Int): Bool = io.grant.candidateBusy(c)

  // At most one candidate bit is ever set per lane, so clearing the whole
  // lane is equivalent to clearing just the granted candidate.
  for (v <- 0 until vcCount) {
    when(io.release(v)) {
      grant.clearLane(v)
    }
  }

  // allowedMask(v)(c): compile-time lookup table -- indexed at compile time
  // by lane v, giving a Vec indexable by a runtime candidate index of
  // whether candidate c may use lane v.
  val allowedMask = grant.allowedMask

  val laneSelector = new VcSelector(vcCount, roundRobinArbitration = false)
  val candidateSelector = new VcSelector(candidateCount, roundRobinArbitration)

  // A candidate is only eligible to be picked (held) if it currently has at
  // least one free, allowed lane -- otherwise candidateSelector could latch
  // onto a candidate that can never be served right now, stalling this
  // entire output port even though some other, presently-servable candidate
  // is waiting.
  val eligible = Vec(Bool(), candidateCount)
  for (c <- 0 until candidateCount) {
    val hasFreeAllowedLane = (0 until vcCount)
      .filter(v => grant.allowed(v, c))
      .map(v => !laneBusy(v))
      .foldLeft(False)(_ || _)
    eligible(c) := io.request(c) && !candidateBusy(c) && hasFreeAllowedLane
  }

  for (c <- 0 until candidateCount) {
    candidateSelector.io.requests(c) := eligible(c)
  }
  io.activity := candidateSelector.io.activity

  // Any free lane the held candidate is allowed to use will do -- among
  // those, they're interchangeable -- so no fairness policy is needed for
  // lane selection. allowedMask(v) is a compile-time (lane v fixed) lookup,
  // so indexing it by the runtime chosen candidate gives, for each lane,
  // whether the held candidate may use it.
  val chosenAllowed = Vec.tabulate(vcCount)(v => allowedMask(v)(candidateSelector.io.chosen.payload))
  for (v <- 0 until vcCount) {
    laneSelector.io.requests(v) := !laneBusy(v) && candidateSelector.io.chosen.valid && chosenAllowed(v)
  }

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
        grant.claim(v, c)
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
    (for (rr <- Seq(true, false); candidates <- Seq(1, 2, 5); vcs <- Seq(1, 2, 3)) yield
      (s"AllowAll_rr${rr}_c${candidates}_vc${vcs}", () =>
        GeneralFormalDut(() => new GrantTable(candidates, vcs, rr, GrantTable.allowAll(candidates, vcs)))
      )) ++
    (for (rr <- Seq(true, false); vcs <- Seq(1, 2, 3); ports <- Seq(1, 2, 3)) yield
      (s"Diagonal_rr${rr}_ports${ports}_vc${vcs}", () =>
        GeneralFormalDut(() => new GrantTable(ports * vcs, vcs, rr, GrantTable.diagonal(ports * vcs, vcs)))
      ))
  }
}
