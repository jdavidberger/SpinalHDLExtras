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
                                    roundRobinArbitration: Boolean) extends ComponentWithFormalProperties {
  val channelCount = allowed.length
  val candidateCount = allowed.headOption.map(_.length).getOrElse(0)

  val io = new Bundle {
    val sources = Vec(slave(Stream(Fragment(payloadType))), candidateCount)
    val dests   = Vec(master(Stream(Fragment(payloadType))), channelCount)

    val activity = out(Bool())
  }

  val arbiter = new GrantTableArbiter(roundRobinArbitration, allowed)
  io.activity := arbiter.io.activity

  val router = arbiter.io.grant.createRouter(payloadType)

  for (c <- 0 until candidateCount) {
    arbiter.io.request(c) := io.sources(c).valid
  }
  for (v <- 0 until channelCount) {
    arbiter.io.release(v) := io.dests(v).lastFire
  }

  router.io.sources <> io.sources
  router.io.dests <> io.dests

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
    (for (rr <- Seq(true, false); candidates <- Seq(1, 2, 5); channels <- Seq(1, 2, 3)) yield
      (s"AllowAll_rr${rr}_c${candidates}_vc${channels}", () =>
        GeneralFormalDut(() => new GrantTableCrossbar(Bits(4 bits), GrantTable.allowAll(candidates, channels), rr))
      )) ++
      (for (rr <- Seq(true, false); channels <- Seq(1, 2, 3); ports <- Seq(1, 2, 3)) yield
        (s"Diagonal_rr${rr}_ports${ports}_vc${channels}", () =>
          GeneralFormalDut(() => new GrantTableCrossbar(Bits(4 bits), GrantTable.diagonal(ports * channels, channels), rr))
        ))
  }
}
