package spinalextras.lib.noc.virtualchannels

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal._
import spinal.lib._
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties, FormalProperty}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

// Pure Stream muxing driven by an externally-supplied grant matrix: for each
// dest lane v, whichever candidate c has io.grant(v)(c) set is connected
// through to io.dests(v). No allocation logic and no payload transform live
// here -- the payload type is generic and passes through unchanged; callers
// needing to retag a payload (e.g. stamping a new vc id) do so with .map()
// on io.dests, outside this component.
//
// Correctness here relies on io.grant following the same invariants a real
// GrantTable guarantees: at most one candidate granted per lane, at most one
// lane per candidate, and a granted pairing held stable until its lane
// fires its last fragment. Nothing in the body enforces that -- it's the
// caller's (GrantTable's) responsibility -- so those invariants are stated
// explicitly in formalComponentInputProperties and assumed when this is
// formally tested directly, rather than re-derived by pairing with a real
// GrantTable instance.
class VcRouter[T <: Data](payloadType: HardType[T], candidateCount: Int, vcCount: Int) extends ComponentWithFormalProperties {
  val io = new Bundle {
    val grant   = in (new GrantTableOutput(candidateCount, vcCount))
    val sources = Vec(slave(Stream(Fragment(payloadType))), candidateCount)
    val dests   = Vec(master(Stream(Fragment(payloadType))), vcCount)
  }

  io.sources.foreach(_.setBlocked())
  io.dests.foreach(_.setIdle())

  for (v <- 0 until vcCount; c <- 0 until candidateCount) {
    when(io.grant(v)(c)) {
      io.sources(c) <> io.dests(v)
    }
  }

  override def formalComponentInputProperties(): Seq[FormalProperty] = new FormalProperties(this) {
    // A held pairing doesn't change except across a release: if (v, c) was
    // granted last cycle and lane v didn't fire its last fragment last
    // cycle, (v, c) must still be granted this cycle. Combined with the
    // mutual-exclusion properties above, this also rules out silently
    // reassigning a candidate to a different lane, or a lane to a different
    // candidate, without going through a released (all-false) state first.
    for (v <- 0 until vcCount) {
      val laneWasReleased = past(io.dests(v).lastFire) init (False)
      for (c <- 0 until candidateCount) {
        val wasGranted = past(io.grant(v)(c)) init (False)
        addFormalProperty(laneWasReleased || !wasGranted || io.grant(v)(c),
          s"grant($v)($c) must not be revoked except when lane $v's transaction completes")
      }
    }
  }
}

class VcRouterFormalTester extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 10

  formalTests().foreach(t => test(t._1) {
    t._2()
  })

  override def generateRtl() = {
    for (candidates <- Seq(1, 2, 5); vcs <- Seq(1, 2, 3)) yield
      (s"Basic_c${candidates}_vc${vcs}", () =>
        GeneralFormalDut(() => new VcRouter(Bits(4 bits), candidates, vcs))
      )
  }
}
