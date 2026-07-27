package spinalextras.lib.tests.noc

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinalextras.lib.misc.arbitration.{Async, GrantTable, GrantTableCrossbar, Register, RoutingMode, Stall}
import spinalextras.lib.noc._
import spinalextras.lib.noc.topology.Mesh

import scala.collection.mutable

// Wraps a bare FlitRouter with a structured (UInt dest) input instead of a
// raw Bits header, so the testbench doesn't need to know Header's bit
// layout -- the header packing itself happens in hardware, exactly like a
// real integration would.
class FlitRouterBubbleHarness(cfg: NocConfig) extends Component {
  val router = new FlitRouter(cfg, address = 0, inputPort = 0)

  val io = new Bundle {
    val destIn = in(UInt(cfg.topology.addressSize bits))
    val validIn = in(Bool())
    val readyOut = out(Bool())
    val outputs = Vec(master(Stream(Fragment(Bits(cfg.dataWidth bits)))), router.connectivityOut)
  }

  val hdr = Header(cfg)
  hdr.dest := io.destIn
  hdr.application.setAll()

  router.io.input.valid := io.validIn
  router.io.input.payload.fragment := hdr.asBits.resized
  router.io.input.payload.last := True
  io.readyOut := router.io.input.ready

  io.outputs <> router.io.output
}

/**
 * FlitRouter's forced-stall bubble, verified cycle-accurately in
 * simulation (not just formally/structurally): drive two single-flit
 * packets back to back with the sender never withholding a flit it's able
 * to send (valid asserted once, held until both are accepted) and the
 * consumer always ready, then check exactly which cycles each flit is
 * accepted and delivered on.
 *
 * `Stall` (today's behavior) makes outputNode's register-gated route
 * decision expose a real gap on the output stream between the two
 * packets -- a "bubble" -- even though nothing downstream was ever
 * unready. `Async` and `Register` both admit each packet's first flit
 * without that gap, differing only in *when* the flit reaches the output:
 * Async admits combinationally the same cycle its header arrives (zero
 * added latency), Register keeps outputNode's normal one-cycle latency but
 * carries the flit through `input.stage()` instead of stalling the wire
 * for it -- so Async's per-packet latency is exactly one cycle less than
 * Stall's and Register's, which are equal.
 */
class FlitRouterBubbleSpec extends AnyFunSuite {

  case class Result(acceptCycles: Seq[Int], deliverCycles: Seq[Int])

  def run(routingMode: RoutingMode): Result = {
    val cfg = NocConfig(topology = new Mesh((2, 1)), dataWidth = 16, routingMode = routingMode)

    var result: Result = null

    SimConfig.compile(new FlitRouterBubbleHarness(cfg)).doSim(seed = 42) { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      dut.io.validIn #= false
      dut.io.outputs.foreach(_.ready #= true)
      dut.clockDomain.waitSampling(5)

      // Both packets target node 1 -- the only non-local port -- so both
      // land on the same io.outputs index and can be told apart purely by
      // arrival order.
      dut.io.destIn #= cfg.topology.addressToRouteableAddress(1)
      dut.io.validIn #= true

      var cycle = 0
      val acceptCycles = mutable.ArrayBuffer[Int]()
      val deliverCycles = mutable.ArrayBuffer[Int]()

      // "No stalls on the sender": valid is asserted once and never
      // dropped except right after the second packet is accepted -- the
      // sender never withholds a flit it's able to send.
      while (deliverCycles.size < 2 && cycle < 200) {
        dut.clockDomain.waitSampling()
        cycle += 1

        if (dut.io.validIn.toBoolean && dut.io.readyOut.toBoolean) {
          acceptCycles += cycle
          if (acceptCycles.size == 2) dut.io.validIn #= false
        }
        for (i <- dut.io.outputs.indices) {
          if (dut.io.outputs(i).valid.toBoolean && dut.io.outputs(i).ready.toBoolean) {
            assert(i == 1, s"flit delivered on unexpected output port $i (expected 1, the East/node-1 port)")
            deliverCycles += cycle
          }
        }
      }

      assert(deliverCycles.size == 2, s"timed out waiting for both packets to be delivered (routingMode=$routingMode)")
      result = Result(acceptCycles.toSeq, deliverCycles.toSeq)
    }

    result
  }

  // presentCycle(0) = 1: the first cycle the DUT can possibly react to the
  // initial poke. presentCycle(1) = acceptCycles(0) + 1: the earliest
  // cycle the sender could legally swap to a new flit, per Stream
  // discipline (payload/valid must hold until the previous flit fires).
  def latencies(r: Result): Seq[Int] = {
    val presentCycles = Seq(1, r.acceptCycles(0) + 1)
    r.deliverCycles.zip(presentCycles).map { case (d, p) => d - p }
  }

  def gap(r: Result): Int = r.deliverCycles(1) - r.deliverCycles(0)

  test("Stall shows a bubble between back-to-back packets") {
    val r = run(Stall)
    assert(gap(r) == 2, s"expected a 1-cycle bubble (gap=2 for 1-flit packets) between deliveries, got gap=${gap(r)} ($r)")
    assert(latencies(r) == Seq(1, 1), s"expected deterministic 1-cycle latency for both packets, got ${latencies(r)}")
  }

  test("Async shows no bubble between back-to-back packets") {
    val r = run(Async)
    assert(gap(r) == 1, s"expected zero-bubble back-to-back delivery (gap=1 for 1-flit packets), got gap=${gap(r)} ($r)")
    assert(latencies(r) == Seq(0, 0), s"expected deterministic 0-cycle latency for both packets, got ${latencies(r)}")
  }

  test("Register shows no bubble between back-to-back packets") {
    val r = run(Register)
    assert(gap(r) == 1, s"expected zero-bubble back-to-back delivery (gap=1 for 1-flit packets), got gap=${gap(r)} ($r)")
    assert(latencies(r) == Seq(1, 1), s"expected deterministic 1-cycle latency for both packets, got ${latencies(r)}")
  }

  test("Async's latency is exactly one cycle less than Stall's and Register's") {
    val stallLatency = latencies(run(Stall)).head
    val asyncLatency = latencies(run(Async)).head
    val registerLatency = latencies(run(Register)).head
    assert(asyncLatency == stallLatency - 1,
      s"Async latency ($asyncLatency) should be Stall latency ($stallLatency) minus one")
    assert(asyncLatency == registerLatency - 1,
      s"Async latency ($asyncLatency) should be Register latency ($registerLatency) minus one")
  }
}

/**
 * GrantTableCrossbar's forced-stall bubble, verified the same way as
 * FlitRouter's above -- but the observable shape of the fix is different
 * here, because RoutingMode.Async and RoutingMode.Register fix different
 * things:
 *
 *   - Async removes exactly the registered-`grant`-commit bubble (the
 *     direct analogue of FlitRouter's outputNode bubble) but leaves
 *     candidateSelector/laneSelector's own multi-cycle latency to *decide*
 *     a winner untouched (by design -- see GrantTableArbiter). So Async's
 *     per-packet admission latency is exactly one cycle less than Stall's,
 *     deterministically, for every packet -- not just the first.
 *   - Register doesn't speed up delivery at all (same latency as Stall);
 *     it decouples the *sender* from the wait via a one-deep
 *     `input.stage()`-style buffer. A packet arriving while that buffer is
 *     empty is accepted with zero sender-visible stall, full stop --
 *     that's the "no stall" property this mode actually provides. A
 *     *second* back-to-back packet can still see backpressure once the
 *     buffer is occupied by the first (a single register can't hide
 *     candidateSelector/laneSelector's multi-cycle latency indefinitely),
 *     which is expected, not a bug.
 *
 * This harness is also what caught a real deadlock: an earlier version of
 * GrantTableCrossbar drove `arbiter.io.request` from the live
 * `io.sources(c).valid` alone. In Register mode, a flit that has already
 * fired into the stage lets the sender legally drop valid immediately
 * (Stream discipline) -- so request(c) would drop too, and if that
 * happened before candidateSelector/laneSelector had a chance to notice,
 * the buffered flit was abandoned forever. Fixed by also counting the
 * staged copy's own valid.
 */
class GrantTableCrossbarBubbleSpec extends AnyFunSuite {

  case class Result(acceptCycles: Seq[Int], deliverCycles: Seq[Int])

  def run(routingMode: RoutingMode): Result = {
    val allowed = GrantTable.allowAll(1, 1)

    var result: Result = null

    SimConfig.compile(new GrantTableCrossbar(Bits(8 bits), allowed, roundRobinArbitration = true, routingMode))
      .doSim(seed = 42) { dut =>
        dut.clockDomain.forkStimulus(period = 10)
        dut.io.sources.foreach(_.valid #= false)
        dut.io.dests.foreach(_.ready #= true)
        dut.clockDomain.waitSampling(5)

        dut.io.sources(0).payload.fragment #= 0xAB
        dut.io.sources(0).payload.last #= true
        dut.io.sources(0).valid #= true

        var cycle = 0
        val acceptCycles = mutable.ArrayBuffer[Int]()
        val deliverCycles = mutable.ArrayBuffer[Int]()

        // "No stalls on the sender": valid is asserted once and never
        // dropped except right after the second transfer is accepted.
        while (deliverCycles.size < 2 && cycle < 200) {
          dut.clockDomain.waitSampling()
          cycle += 1

          if (dut.io.sources(0).valid.toBoolean && dut.io.sources(0).ready.toBoolean) {
            acceptCycles += cycle
            if (acceptCycles.size == 2) dut.io.sources(0).valid #= false
          }
          if (dut.io.dests(0).valid.toBoolean && dut.io.dests(0).ready.toBoolean) {
            deliverCycles += cycle
          }
        }

        assert(deliverCycles.size == 2, s"timed out waiting for both transfers to be delivered (routingMode=$routingMode)")
        result = Result(acceptCycles.toSeq, deliverCycles.toSeq)
      }

    result
  }

  // Sender-visible admission stall for the *first* transfer only (arriving
  // to an otherwise-idle crossbar): cycles from valid-first-asserted
  // (cycle 1) to accepted.
  def firstStall(r: Result): Int = r.acceptCycles.head - 1

  // Per-packet destination latency (accept to deliver) -- meaningful for
  // Stall/Async, where accept and deliver coincide with no queuing in
  // between; not asserted for Register's second packet, where it also
  // reflects however long the first packet was still occupying the
  // one-deep stage, not just this packet's own admission cost.
  def deliveryLatencies(r: Result): Seq[Int] = r.deliverCycles.zip(r.acceptCycles).map { case (d, a) => d - a }

  test("Stall stalls the sender for a fresh admission, deterministically for both transfers") {
    val r = run(Stall)
    assert(firstStall(r) > 0, s"expected Stall to stall the sender on a fresh admission, got ${firstStall(r)} ($r)")
    val latencies = deliveryLatencies(r)
    assert(latencies.head == 0 && latencies(1) == 0,
      s"Stall's accept and deliver should coincide (no staging), got $latencies")
  }

  test("Async stalls the sender one cycle less than Stall, deterministically for both transfers") {
    val stall = run(Stall)
    val async = run(Async)
    assert(firstStall(async) == firstStall(stall) - 1,
      s"Async's sender stall (${firstStall(async)}) should be Stall's (${firstStall(stall)}) minus one")
    val stallGaps = stall.acceptCycles(1) - stall.acceptCycles(0)
    val asyncGaps = async.acceptCycles(1) - async.acceptCycles(0)
    assert(asyncGaps == stallGaps - 1,
      s"the gap between the two transfers' admissions should also shrink by exactly one cycle " +
        s"(Stall gap=$stallGaps, Async gap=$asyncGaps)")
  }

  test("Register never stalls the sender for a transfer arriving to an empty crossbar") {
    val r = run(Register)
    assert(firstStall(r) == 0, s"expected zero sender-visible stall for the first (uncontended) transfer, got ${firstStall(r)} ($r)")
  }

  test("Register doesn't deadlock a second back-to-back transfer once the buffer is occupied") {
    // Regression for the request-from-live-valid-only bug described in the
    // class doc above: this used to hang forever (the second transfer was
    // accepted into the stage but never delivered).
    val r = run(Register)
    assert(r.deliverCycles.size == 2, s"both transfers should eventually deliver, got $r")
  }
}
