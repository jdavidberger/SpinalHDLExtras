package spinalextras.lib.noc.virtualchannels

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._
import spinalextras.lib.misc.Optional
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

// Picks at most one requester out of n each cycle, and presents it as a
// Stream: valid = a winner is held, payload = its index, ready = the
// consumer takes it. To honor Stream semantics (payload/valid must not
// change while stalled), a winner is latched once picked and held stable --
// ignoring any changes in `requests` -- until the consumer fires it. Only
// then is a new winner searched for, from the requests present at that time.
class VcSelector(n: Int, roundRobinArbitration: Boolean) extends Component {
  val vcBits = log2Up(n)

  val io = new Bundle {
    val requests = in Vec(Bool(), n)
    val chosen   = master(Stream(UInt(vcBits bits)))
  }

  val rrPointer = if (roundRobinArbitration) RegInit(U(0, vcBits bits)) else null

  // Search for a winner among the current requests. Only consulted while
  // not currently holding one (see below), so this never disturbs an
  // already-presented, not-yet-taken choice.
  val pick = Optional.Empty(UInt(vcBits bits))
  var wc = when(False) {}
  if (roundRobinArbitration) {
    // Prefer requesters at or after the round-robin pointer, then wrap.
    for (i <- 0 until n) {
      wc = wc.elsewhen(io.requests(i) && U(i, vcBits bits) >= rrPointer) {
        pick.set_value(U(i, vcBits bits))
      }
    }
    for (i <- 0 until n) {
      wc = wc.elsewhen(io.requests(i) && U(i, vcBits bits) < rrPointer) {
        pick.set_value(U(i, vcBits bits))
      }
    }
  } else {
    for (i <- 0 until n) {
      wc = wc.elsewhen(io.requests(i)) {
        pick.set_value(U(i, vcBits bits))
      }
    }
  }

  val held = RegInit(Optional.Empty(UInt(vcBits bits)))

  io.chosen.valid := held.has_value
  io.chosen.payload := held.value

  when(held.has_value && io.chosen.fire) {
    held.clear()
    if (roundRobinArbitration) {
      rrPointer := Mux(held.value === U(n - 1, vcBits bits), U(0, vcBits bits), (held.value + 1).resize(vcBits))
    }
  } elsewhen (!held.has_value) {
    held := pick
  }
}

class VcSelectorFormalTester extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 10

  formalTests().foreach(t => test(t._1) {
    t._2()
  })

  override def generateRtl() = {
    for (rr <- Seq(true, false); n <- Seq(2, 3, 4)) yield
      (s"Basic_rr${rr}_n${n}", () =>
        GeneralFormalDut(() => new VcSelector(n, rr))
      )
  }
}
