package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinalextras.lib.noc._
import spinalextras.lib.noc.topology.{Mesh, Ring, Star, Torus, Tree}
import spinalextras.lib.noc.virtualchannels._

import scala.collection.mutable
import scala.language.postfixOps
import scala.util.Random

/**
 * ============================================================================
 * Generic pathing test harness for arbitrary NoC topologies
 * ============================================================================
 *
 * `NocPathingHarness` wraps a plain `NoC(cfg)` and, using the
 * `configureInputNode` / `configureOutputNode` helpers `NoC` already exposes,
 * gives every node a plain `Stream(Fragment(Bits))` in/out port plus a
 * `dest`/`vc` control signal. This means the testbench never has to know how
 * `Header` gets bit-packed onto a `Flit` -- it lets the hardware build the
 * header exactly the way any real integration would (via `insertHeader`),
 * and only has to supply the destination node and a payload.
 *
 * Because `headerApplicationBits` is defined as `dataWidth - addressSize`,
 * `Header.asBits` is always exactly `dataWidth` bits wide, so `insertHeader`
 * always prepends exactly *one* header flit in front of whatever payload
 * flits the testbench sends. That invariant is what lets the receive side
 * below treat "the first flit after an idle gap" as the (opaque, ignorable)
 * header and everything else as payload, without decoding it.
 */
class NocPathingHarness(cfg: NocConfig) extends Component {
  val n = cfg.topology.nodes

  val io = new Bundle {
    val rawInputs  = Vec(slave(Stream(Fragment(Bits(cfg.dataWidth bits)))), n)
    val destInputs = in(Vec(UInt(cfg.topology.addressSize bits), n))
    val vcInputs   = in(Vec(UInt(cfg.virtualChannelBits bits), n))
    val rawOutputs = Vec(master(Stream(Fragment(Bits(cfg.dataWidth bits)))), n)
  }

  val noc = new NoC(cfg)

  for (i <- 0 until n) {
    noc.configureInputNode(i, io.rawInputs(i), io.destInputs(i), io.vcInputs(i))
    noc.configureOutputNode(i, io.rawOutputs(i))
  }
}

/**
 * Drives packets through a `NocPathingHarness` and checks that every packet
 * arrives at the node it was actually addressed to, with its payload intact,
 * and that nothing shows up anywhere it shouldn't.
 *
 * Packets are injected strictly one at a time, globally: the previous packet
 * is fully drained before the next one is sent. That's a deliberate
 * simplification, not an oversight -- `OutputPort` arbitrates with
 * `StreamArbiterFactory(...).noLock`, so in general flits belonging to two
 * *different* in-flight packets that happen to share an output port could
 * interleave beat-by-beat. The raw `Bits` ports this harness drives (via
 * `configureInputNode`/`configureOutputNode`) don't expose the `vc` tag a
 * receiver would need to de-interleave that. Testing one packet at a time
 * sidesteps that ambiguity entirely and keeps this harness scoped to the
 * question it's meant to answer -- is the path from every source to every
 * destination correct? -- rather than also trying to be a VC/throughput
 * stress test.
 */
object NocPathingTester {

  case class Packet(src: Int, dst: Int, id: Int, vc: Int = 0)
  private case class Arrival(node: Int, src: Int, dst: Int, id: Int)

  /** Every (src, dst) pair exactly once, including src == dst (local loopback). */
  def allPairs(cfg: NocConfig): Seq[Packet] = {
    val n = cfg.topology.nodes
    val vcs = Math.max(cfg.virtualChannels, 1)
    (for (src <- 0 until n; dst <- 0 until n) yield (src, dst)).zipWithIndex.map {
      case ((src, dst), id) => Packet(src, dst, id, id % vcs)
    }
  }

  /** A random sample of (src, dst) pairs -- for topologies too large to test exhaustively. */
  def randomPairs(cfg: NocConfig, count: Int, seed: Long = 0): Seq[Packet] = {
    val n = cfg.topology.nodes
    val vcs = Math.max(cfg.virtualChannels, 1)
    val rnd = new Random(seed)
    (0 until count).map(id => Packet(rnd.nextInt(n), rnd.nextInt(n), id, rnd.nextInt(vcs)))
  }

  /**
   * Build a fresh `NoC(cfg)`, drive `packets` through it one at a time, and
   * assert every one is delivered to the correct physical node with its
   * payload unchanged.
   */
  def test(cfg: NocConfig,
           packets: Seq[Packet] = null,
           timeoutCycles: Int = 20000,
           simSeed: Int = 42): Unit = {

    val pkts = Option(packets).getOrElse(allPairs(cfg))
    require(pkts.nonEmpty, "no packets to send")
    val maxId = pkts.map(_.id).max
    require(BigInt(maxId) < (BigInt(1) << cfg.dataWidth),
      s"packet id $maxId doesn't fit in a ${cfg.dataWidth}-bit flit; shrink the packet set or widen dataWidth")

    SimConfig.withWave.compile(new NocPathingHarness(cfg)).doSim(seed = simSeed) { dut =>
      dut.clockDomain.forkStimulus(period = 10)

      val n = dut.n
      for (node <- 0 until n) {
        dut.io.rawInputs(node).valid #= false
      }

      dut.clockDomain.waitSampling(10)

      // Every output port is always ready to accept; every accepted beat is
      // recorded here. First beat after an idle node is the (opaque) header,
      // everything after that up to `last` is our own 3-beat payload
      // (src, dst, id).
      val arrivals = mutable.Queue[Arrival]()
      val malformed = mutable.ArrayBuffer[String]()

      for (node <- 0 until n) {
        val port = dut.io.rawOutputs(node)
        port.ready #= true

        fork {
          var expectHeader = true
          val buf = mutable.ArrayBuffer[BigInt]()
          while (true) {
            dut.clockDomain.waitSamplingWhere(port.valid.toBoolean && port.ready.toBoolean)
            if (expectHeader) {
              expectHeader = false
            } else {
              buf += port.payload.fragment.toBigInt
              if (port.payload.last.toBoolean) {
                if (buf.size == 3) {
                  arrivals.enqueue(Arrival(node, buf(0).toInt, buf(1).toInt, buf(2).toInt))
                  println(s"Buffer arrived ${arrivals.last}")
                } else {
                  malformed += s"node $node: packet ended with ${buf.size} payload beats (expected 3): $buf"
                }
                buf.clear()
                expectHeader = true
              }
            }
          }
        }
      }

      def sendPacket(p: Packet): Unit = {
        val stream = dut.io.rawInputs(p.src)
        println(s"Sending ${p.src} -> ${p.dst} ${p.vc}")
        dut.io.destInputs(p.src) #= cfg.topology.addressToRouteableAddress(p.dst)
        dut.io.vcInputs(p.src) #= p.vc

        val beats = Seq(BigInt(p.src), BigInt(p.dst), BigInt(p.id))
        for ((data, idx) <- beats.zipWithIndex) {
          stream.valid #= true
          stream.payload.fragment #= data
          stream.payload.last #= (idx == beats.size - 1)
          dut.clockDomain.waitSamplingWhere(stream.ready.toBoolean)
        }
        stream.valid #= false
      }

      def waitForArrival(p: Packet): Arrival = {
        var cycles = 0
        while (arrivals.isEmpty) {
          dut.clockDomain.waitSampling()
          cycles += 1
          assert(cycles < timeoutCycles,
            s"Timed out after $timeoutCycles cycles waiting for packet ${p.id} " +
              s"(${cfg.topology.addressName(p.src)} -> ${cfg.topology.addressName(p.dst)}) to arrive. " +
              s"Either the path is broken/deadlocked, or flits are being dropped.")
        }
        arrivals.dequeue()
      }

      var checked = 0
      for (p <- pkts) {
        sendPacket(p)
        val a = waitForArrival(p)

        assert(malformed.isEmpty, s"malformed traffic observed: ${malformed.mkString("; ")}")

        assert(a.src == p.src && a.dst == p.dst && a.id == p.id,
          s"payload corrupted in transit: sent $p, received $a")

        assert(a.node == p.dst,
          s"MISROUTED: packet sent from ${cfg.topology.addressName(p.src)} " +
            s"addressed to ${cfg.topology.addressName(p.dst)} arrived at " +
            s"${cfg.topology.addressName(a.node)} instead")

        checked += 1
      }

      // Let things settle, then make sure nothing extra/duplicated ever shows up.
      dut.clockDomain.waitSampling(10)
      assert(arrivals.isEmpty, s"extra/duplicate packet(s) arrived that were never sent: ${arrivals.toSeq}")
      assert(malformed.isEmpty, s"malformed traffic observed: ${malformed.mkString("; ")}")

      println(s"NocPathingTester: verified $checked/${pkts.size} packets across $n nodes " +
        s"(${cfg.topology.getClass.getSimpleName})")
    }
  }
}

/**
 * Runs the pathing harness against every topology the NoC library ships,
 * mirroring the topology/size combinations used elsewhere in the repo's
 * formal test suites (see `NocFormalTester.generateRtl`).
 */
class NocPathingSpec extends AnyFunSuite {

  def topologies: Seq[(String, NocConfig)] = NocConfig.testConfigurations()

  for ((name, cfg) <- topologies) {
    test(s"pathing is correct across all node pairs: $name") {
      NocPathingTester.test(cfg)
    }
  }

  // A bigger mesh, sampled rather than exhaustive, just to sanity-check the
  // harness scales down the packet count sensibly for larger node counts.
  test("pathing is correct (sampled) on a larger mesh: Mesh(6x6)") {
    val cfg = NocConfig(topology = new Torus((6, 6)))
    NocPathingTester.test(cfg, packets = NocPathingTester.randomPairs(cfg, count = 200))
  }
}