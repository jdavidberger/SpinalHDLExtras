package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinalextras.lib.noc._
import spinalextras.lib.noc.topology.{Mesh, Ring, Star, Torus, Tree}
import spinalextras.lib.noc.virtualchannels.Dynamic

import scala.collection.mutable
import scala.language.postfixOps
import scala.util.Random

/**
 * ============================================================================
 * Concurrency / VC-isolation test harness
 * ============================================================================
 *
 * `NocPathingSim.scala` deliberately tests one packet at a time, precisely
 * because the raw `Bits` ports it exposes don't carry the `vc` tag needed to
 * tell two *simultaneously* in-flight packets apart if their flits end up
 * interleaved on a shared physical link. This harness exists to remove that
 * restriction and test the thing it explicitly punted on:
 *
 *   1. That multiple packets can genuinely be in flight through the NoC at
 *      the same time (not just pipelined-but-effectively-serial).
 *   2. That packets on different VCIDs sharing a physical link don't get
 *      their flits mixed up with each other.
 *
 * `NocConcurrentHarness` is almost identical to `NocPathingHarness`, except
 * the output side exposes the full `Stream(Fragment(Flit(cfg)))` (including
 * `.vc`) instead of going through `configureOutputNode`. That's what lets
 * the receive side in `NocConcurrentTester` demultiplex flits from several
 * concurrently-arriving packets sharing one output port: it keeps one
 * "packet currently being reassembled" buffer *per VCID* instead of one per
 * node. That's a safe way to disambiguate because of how routing already
 * works here: a router only ever binds one in-flight packet to a given
 * (output port, vc) pair at a time (see `vcToOutputNode` / `vc.has_value` in
 * `RouterNode`) -- so at any single hop, two packets that are genuinely
 * concurrent on the same wire are guaranteed to be on different VCs, which
 * is exactly the tag this harness uses to keep them apart.
 */
class NocConcurrentHarness(cfg: NocConfig) extends Component {
  val n = cfg.topology.nodes

  val io = new Bundle {
    val rawInputs  = Vec(slave(Stream(Fragment(Bits(cfg.dataWidth bits)))), n)
    val destInputs = in(Vec(UInt(cfg.topology.addressSize bits), n))
    val vcInputs   = in(Vec(UInt(cfg.virtualChannelBits bits), n))
    val outputs    = Vec(master(Stream(Fragment(Flit(cfg)))), n)
  }

  val noc = new NoC(cfg)

  for (i <- 0 until n) {
    noc.configureInputNode(i, io.rawInputs(i), io.destInputs(i), io.vcInputs(i))
    io.outputs(i) <> noc.io.outputs(i)
  }
}

object NocConcurrentTester {

  import NocPathingTester.Packet

  private case class Arrival(node: Int, src: Int, dst: Int, id: Int, vc: Int)
  private case class SendWindow(startCycle: Long, endCycle: Long)

  /**
   * `packetsPerSrc` packets from every node, to random destinations, cycling
   * through the available VCs. Every source runs on its own fork (see
   * `test` below), so this alone is enough to get many genuinely concurrent
   * streams moving through the NoC at once.
   */
  def floodPackets(cfg: NocConfig, packetsPerSrc: Int = 4, seed: Long = 0): Seq[Packet] = {
    val n = cfg.topology.nodes
    val vcs = Math.max(cfg.virtualChannels, 1)
    val rnd = new Random(seed)
    var id = 0
    val pkts = mutable.ArrayBuffer[Packet]()
    for (src <- 0 until n; _ <- 0 until packetsPerSrc) {
      pkts += Packet(src, rnd.nextInt(n), id, id % vcs)
      id += 1
    }
    pkts.toSeq
  }

  /**
   * One packet from every other node, all addressed at `dst`, each on a
   * distinct (round-robin) VCID. However `dst` is reached, every one of
   * these packets necessarily shares at least `dst`'s final inbound link
   * with several of the others -- a topology-agnostic way to force real
   * contention/interleaving on a shared physical port and exercise VC
   * isolation specifically, rather than hoping for it to happen by chance.
   */
  def manyToOne(cfg: NocConfig, dst: Int, includeSelf: Boolean = false): Seq[Packet] = {
    val n = cfg.topology.nodes
    val vcs = Math.max(cfg.virtualChannels, 1)
    (0 until n).filter(src => includeSelf || src != dst).zipWithIndex.map {
      case (src, id) => Packet(src, dst, id, id % vcs)
    }
  }

  /**
   * Send `packets` through a fresh `NoC(cfg)`, with one source-node worth of
   * packets driven per fork (so different sources genuinely overlap in
   * time), and verify:
   *
   *   - every packet arrives at the correct node, with its payload intact
   *   - nothing arrives that wasn't sent, nothing arrives twice
   *   - at least two packets were actually in flight at the same time
   *     (otherwise this test proved nothing about concurrency)
   */
  def test(cfg: NocConfig,
           packets: Seq[Packet],
           timeoutCycles: Int = 40000,
           simSeed: Int = 42): Unit = {

    require(packets.nonEmpty, "no packets to send")
    val maxId = packets.map(_.id).max
    require(BigInt(maxId) < (BigInt(1) << cfg.dataWidth),
      s"packet id $maxId doesn't fit in a ${cfg.dataWidth}-bit flit")
    packets.foreach(p => require(p.vc < Math.max(cfg.virtualChannels, 1),
      s"packet $p uses vc ${p.vc}, but cfg only has ${cfg.virtualChannels} virtual channel(s)"))

    SimConfig.withWave.compile(new NocConcurrentHarness(cfg)).doSim(seed = simSeed) { dut =>
      dut.clockDomain.forkStimulus(period = 10)

      val n = dut.n
      for (node <- 0 until n) dut.io.rawInputs(node).valid #= false

      dut.clockDomain.waitSampling(10)

      var cycle = 0L
      fork {
        while (true) {
          dut.clockDomain.waitSampling()
          cycle += 1
        }
      }

      val arrivals = mutable.Queue[Arrival]()
      val malformed = mutable.ArrayBuffer[String]()
      val sendWindows = mutable.Map[Int, SendWindow]() // packet id -> (start, end) cycle

      // --- receive side: one reconstruction buffer per (node, vc). Two
      //     packets concurrently sharing a node's output link are
      //     guaranteed (by how routing claims a vc) to be on different
      //     VCs, so keying by vc is sufficient to keep them apart even if
      //     their flits interleave beat-by-beat. ---
      for (node <- 0 until n) {
        val port = dut.io.outputs(node)
        port.ready #= true

        fork {
          val expectHeader = mutable.Map[Int, Boolean]().withDefaultValue(true)
          val bufs = mutable.Map[Int, mutable.ArrayBuffer[BigInt]]()

          while (true) {
            dut.clockDomain.waitSamplingWhere(port.valid.toBoolean && port.ready.toBoolean)
            val vc = port.payload.fragment.vc.toInt
            val buf = bufs.getOrElseUpdate(vc, mutable.ArrayBuffer[BigInt]())

            if (expectHeader(vc)) {
              expectHeader(vc) = false // header flit for this vc's packet: opaque, ignored
            } else {
              buf += port.payload.fragment.datum.toBigInt
              if (port.payload.last.toBoolean) {
                if (buf.size == 3) {
                  arrivals.enqueue(Arrival(node, buf(0).toInt, buf(1).toInt, buf(2).toInt, vc))
                } else {
                  malformed += s"node $node vc $vc: packet ended with ${buf.size} payload beats (expected 3): $buf"
                }
                buf.clear()
                expectHeader(vc) = true
              }
            }
          }
        }
      }

      def sendPacket(p: Packet): Unit = {
        val stream = dut.io.rawInputs(p.src)
        dut.io.destInputs(p.src) #= cfg.topology.addressToRouteableAddress(p.dst)
        dut.io.vcInputs(p.src) #= p.vc

        sendWindows(p.id) = SendWindow(cycle, -1)

        val beats = Seq(BigInt(p.src), BigInt(p.dst), BigInt(p.id))
        for ((data, idx) <- beats.zipWithIndex) {
          stream.valid #= true
          stream.payload.fragment #= data
          stream.payload.last #= (idx == beats.size - 1)
          dut.clockDomain.waitSamplingWhere(stream.ready.toBoolean)
        }
        stream.valid #= false
      }

      // --- send side: one fork per source node (sequential within a
      //     source, concurrent across sources). This is what actually
      //     produces multiple simultaneous in-flight streams. ---
      for ((_, srcPackets) <- packets.groupBy(_.src)) {
        fork {
          for (p <- srcPackets) sendPacket(p)
        }
      }

      // --- wait for every packet to arrive ---
      val expected = packets.map(p => (p.src, p.dst, p.id)).toSet
      val seen = mutable.Set[(Int, Int, Int)]()
      var waited = 0
      while (seen.size < expected.size) {
        while (arrivals.nonEmpty) {
          val a = arrivals.dequeue()
          assert(malformed.isEmpty, s"malformed traffic observed: ${malformed.mkString("; ")}")

          sendWindows.get(a.id).foreach(w => sendWindows(a.id) = w.copy(endCycle = cycle))

          val key = (a.src, a.dst, a.id)
          assert(expected.contains(key), s"unexpected packet arrived (never sent): $a")
          assert(!seen.contains(key), s"packet ${a.id} ($a) was delivered more than once")
          assert(a.node == a.dst,
            s"MISROUTED: packet ${a.id} from ${cfg.topology.addressName(a.src)} to " +
              s"${cfg.topology.addressName(a.dst)} arrived at ${cfg.topology.addressName(a.node)} instead")
          seen += key
        }
        if (seen.size < expected.size) {
          dut.clockDomain.waitSampling()
          waited += 1
          assert(waited < timeoutCycles,
            s"Timed out after $timeoutCycles cycles: ${seen.size}/${expected.size} packets arrived. " +
              s"Still missing: ${(expected -- seen).toSeq.sortBy(_._3)}")
        }
      }

      dut.clockDomain.waitSampling(10)
      assert(arrivals.isEmpty, s"extra/duplicate packet(s) arrived that were never sent: ${arrivals.toSeq}")
      assert(malformed.isEmpty, s"malformed traffic observed: ${malformed.mkString("; ")}")

      // --- prove this genuinely exercised concurrency rather than
      //     accidentally serializing everything ---
      val windows = sendWindows.values.filter(_.endCycle >= 0).toSeq
      val overlapExists = windows.combinations(2).exists {
        case Seq(a, b) => a.startCycle < b.endCycle && b.startCycle < a.endCycle
        case _ => false
      }
      assert(overlapExists,
        "no two packets were ever in flight at the same time -- this run didn't actually test " +
          "concurrency; every packet was processed strictly one at a time")

      println(s"NocConcurrentTester: verified ${seen.size} packets across $n nodes " +
        s"(${cfg.topology.getClass.getSimpleName}); confirmed genuine overlap in flight")
    }
  }
}

/**
 * Confirms, for every topology, that (a) several packets can genuinely be
 * in flight through the NoC concurrently, and (b) packets on different
 * VCIDs sharing a link don't get mangled together.
 */
class NocConcurrencySpec extends AnyFunSuite {

  def topologies: Seq[(String, NocConfig)] = NocConfig.testConfigurations()

  for ((name, cfg) <- topologies) {
    test(s"multiple streams run through the NoC concurrently: $name") {
      NocConcurrentTester.test(cfg, NocConcurrentTester.floodPackets(cfg, packetsPerSrc = 4))
    }

    test(s"differing VCIDs resolve as distinct, unmangled packets: $name") {
      // Every other node fires at node 0 at once, each on its own VC --
      // forces real contention on node 0's inbound link(s) regardless of
      // topology, which is exactly where flits from different VCs would
      // get tangled together if VC isolation were broken.
      NocConcurrentTester.test(cfg, NocConcurrentTester.manyToOne(cfg, dst = 0))
    }
  }

}