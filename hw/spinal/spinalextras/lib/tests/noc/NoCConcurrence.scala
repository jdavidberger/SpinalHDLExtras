package spinalextras.lib.tests.noc

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim.SimManagerContext
import spinalextras.lib.Config
import spinalextras.lib.misc.arbitration.{Async, Register}
import spinalextras.lib.noc._
import spinalextras.lib.noc.protocols.DataStreamSpecification
import spinalextras.lib.noc.virtualchannels.Dynamic

import scala.collection.mutable
import scala.language.postfixOps
import scala.util.Random

/**
 * ============================================================================
 * Concurrency / VC-isolation test harness
 * ============================================================================
 *
 * `NocPathingSim.scala` deliberately tests one packet at a time -- see its
 * own header comment for why. This harness removes that restriction and
 * tests the thing `NocPathingHarness` explicitly punts on:
 *
 *   1. That multiple packets can genuinely be in flight through the NoC at
 *      the same time (not just pipelined-but-effectively-serial).
 *   2. That packets sharing an internal link on different VCs don't get
 *      their flits mixed up with each other.
 *
 * `NocConcurrentHarness` is otherwise identical to `NocPathingHarness`: it
 * shares the same `DataStreamSpecification`-backed wiring, so every node's
 * output is a plain `Stream(Fragment(Bits))` carrying only payload beats
 * (the routing header is stripped before delivery), with no `vc` tag (see
 * `NocConfig.datatype`). `Topology.createNodes` arbitrates that final
 * external link with `fragmentLock`, so a full packet always finishes
 * before the next one starts on it -- flits from different
 * packets can never interleave there. That's what lets `NocConcurrentTester`
 * get away with one reconstruction buffer per node instead of per VC: the
 * genuine concurrency under test happens deeper in the network, on the
 * shared internal links between routers, but it's already resolved by the
 * time flits reach a node's external output. Packets are told apart purely
 * by the `id` each one carries as a payload beat (see `Arrival`), not by VC.
 */
class NocConcurrentHarness(cfg: NocConfig) extends Component {
  val n = cfg.topology.nodes

  val io = new Bundle {
    val rawInputs  = Vec(slave(Stream(Fragment(cfg.datatype))), n)
    val destInputs = in(Vec(UInt(cfg.topology.addressSize bits), n))
    val outputs    = Vec(master(Stream(Fragment(cfg.datatype))), n)
  }

  val builder = new NoCBuilder(cfg)
  val spec = new DataStreamSpecification(HardType(cfg.datatype), builder)

  for (i <- 0 until n) {
    val header = Header(cfg)
    header.dest := io.destInputs(i).resized
    header.application.setAll()

    io.rawInputs(i) <> spec.addSource(header.asBits.resized, i)
    spec.addSink(i) <> io.outputs(i)
  }

  val noc = builder.build()

  //val simLog = GlobalLogger.create_simulation_logger(tags = Set("noc-grant-table"))
}

object NocConcurrentTester {

  import NocPathingTester.Packet

  private case class Arrival(node: Int, src: Int, dst: Int, id: Int)
  private case class SendWindow(startCycle: Long, endCycle: Long)

  /**
   * `packetsPerSrc` packets from every node, to random destinations, cycling
   * through the available VCs. Every source runs on its own fork (see
   * `test` below), so this alone is enough to get many genuinely concurrent
   * streams moving through the NoC at once.
   */
  def floodPackets(cfg: NocConfig, packetsPerSrc: Int = 4, seed: Long = 0): Seq[Packet] = {
    val n = cfg.topology.nodes
    val rnd = new Random(seed)
    var id = 0
    val pkts = mutable.ArrayBuffer[Packet]()
    for (src <- 0 until n; _ <- 0 until packetsPerSrc) {
      var dst = rnd.nextInt(n - 1)
      if (dst >= src) dst += 1
      pkts += Packet(src, dst, id)
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
      case (src, id) => Packet(src, dst, id)
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

    Config.sim.compile(new NocConcurrentHarness(cfg)).doSim(seed = simSeed) { dut =>
      dut.clockDomain.forkStimulus(period = 10)

      val path = SimManagerContext.current.manager.asInstanceOf[CoreSimManager].compiled.compiledPath
      //dut.simLog.startCapture(dut.clockDomain, path + "/logger.sqlite")

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

      // --- receive side: one reconstruction buffer per node. The NoC's
      //     final external output arbitration (Topology.createNodes) uses
      //     fragmentLock, so a full packet always finishes before the next
      //     one starts on a given node's output -- flits from different
      //     packets can't interleave there, so no per-vc keying is needed;
      //     packets are told apart by their `id` payload beat instead.
      //     `DataStreamSpecification` strips the routing header before
      //     delivery, so every beat that arrives is payload. ---
      for (node <- 0 until n) {
        val port = dut.io.outputs(node)
        port.ready #= true

        fork {
          val buf = new mutable.ArrayBuffer[BigInt]()
          while (true) {
            dut.clockDomain.waitSamplingWhere(port.valid.toBoolean && port.ready.toBoolean)
            buf += port.payload.fragment.toBigInt
            if (port.payload.last.toBoolean) {
              if (buf.size == 3) {
                arrivals.enqueue(Arrival(node, buf(0).toInt, buf(1).toInt, buf(2).toInt))
              } else {
                malformed += s"node $node: packet ended with ${buf.size} payload beats (expected 3): $buf"
              }
              buf.clear()
            }
          }
        }
      }

      def sendPacket(p: Packet): Unit = {
        val stream = dut.io.rawInputs(p.src)
        dut.io.destInputs(p.src) #= cfg.topology.addressToRouteableAddress(p.dst)

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
          if (waited >= timeoutCycles) {
            NocDebug.dumpStalledState(dut.noc)
          }
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
      println(name)
      // A cyclic channel dependency, if one exists, is a permanent deadlock
      // once every buffer around the cycle is simultaneously occupied and
      // waiting -- it isn't a transient slowdown that a longer timeout would
      // ride out. So the way to actually find one (rather than pass by luck
      // on too-light traffic) is to guarantee the flood alone can exceed the
      // network's total per-link buffering (~ vcCount * vcDepth flits per
      // link) many times over, not to pick a flat packet count that happens
      // to work for today's default vc/depth combination.
      val packetsPerSrc = 4 * cfg.virtualChannels * cfg.vcDepth
      // Legitimately-healthy delivery time scales with total flit volume, so
      // the timeout budget needs to grow along with packetsPerSrc too --
      // otherwise a heavier flood on a perfectly deadlock-free config would
      // spuriously time out from sheer volume rather than a real stall.
      val timeoutCycles = 40000 * cfg.virtualChannels * cfg.vcDepth
      NocConcurrentTester.test(cfg, NocConcurrentTester.floodPackets(cfg, packetsPerSrc = packetsPerSrc),
        timeoutCycles = timeoutCycles)
    }
  }

}

// Regression coverage for a deadlock that reproduced even with all traffic
// routed one-way (no direction-vs-direction interaction, no local-vs-transit
// admission race) -- root cause turned out to be OutputPort's physical-link
// arbiter starving the escape VC of wire bandwidth in favor of a
// continuously-ready pool VC (see RouterNode.OutputPort).
class RingOneWayRegressionSpec extends AnyFunSuite {
  test("Ring3 clockwise-only vc2 dynamic roundrobin") {
    val cfg = NocConfig(
      topology = new spinalextras.lib.noc.topology.Ring(3, spinalextras.lib.noc.topology.ClockwiseAlways),
      virtualChannels = 2,
      virtualChannelMode = Dynamic,
      virtualChannelArbitrationPolicy = RoundRobin
    )
    val packetsPerSrc = 4 * cfg.virtualChannels * cfg.vcDepth
    val timeoutCycles = 40000 * cfg.virtualChannels * cfg.vcDepth
    NocConcurrentTester.test(cfg, NocConcurrentTester.floodPackets(cfg, packetsPerSrc = packetsPerSrc),
      timeoutCycles = timeoutCycles)
  }
}

/**
 * NocConfig.routingMode regression: confirms Async/Register don't introduce
 * deadlock/starvation under the same concurrent-flood conditions
 * `NocConcurrencySpec` already uses to cover VC-isolation/deadlock-freedom
 * for the Stall path. Async is the one that matters most here -- unlike
 * FlitRouter's route-decision bypass, GrantTableCrossbar's Async mode does
 * reach into the VC grant itself (see GrantTableArbiter's freshGrant/
 * retiredBypass), so it's worth confirming empirically, on both an acyclic
 * and a cyclic (wraparound) topology, rather than just by inspection.
 */
class NocPipelineBypassConcurrencySpec extends AnyFunSuite {

  def baseTopologies: Seq[(String, NocConfig)] = Seq(
    "Mesh_3x3_vc2_vcmDynamic" -> NocConfig(
      topology = new spinalextras.lib.noc.topology.Mesh((3, 3)), virtualChannels = 2, virtualChannelMode = Dynamic),
    "Ring_6_vc2_vcmStatic" -> NocConfig(
      topology = new spinalextras.lib.noc.topology.Ring(6), virtualChannels = 2),
    "Torus_3x2_vc2_vcmDynamic" -> NocConfig(
      topology = new spinalextras.lib.noc.topology.Torus((3, 2)), virtualChannels = 2, virtualChannelMode = Dynamic),
  )

  def topologies: Seq[(String, NocConfig)] = for (
    routingMode <- Seq(Async, Register);
    (name, cfg) <- baseTopologies
  ) yield s"${name}_${NocConfig.objectName(routingMode)}" -> cfg.copy(routingMode = routingMode)

  for ((name, cfg) <- topologies) {
    test(s"multiple streams run through the NoC concurrently with routingMode=${cfg.routingMode}: $name") {
      val packetsPerSrc = 4 * cfg.virtualChannels * cfg.vcDepth
      val timeoutCycles = 40000 * cfg.virtualChannels * cfg.vcDepth
      NocConcurrentTester.test(cfg, NocConcurrentTester.floodPackets(cfg, packetsPerSrc = packetsPerSrc),
        timeoutCycles = timeoutCycles)
    }
  }
}

class NocVCIDSpec extends AnyFunSuite {
  def topologies: Seq[(String, NocConfig)] = NocConfig.testConfigurations()

  for ((name, cfg) <- topologies) {
    if(cfg.topology.nodes > 1) {
      test(s"differing VCIDs resolve as distinct, unmangled packets: $name") {
        // Every other node fires at node 0 at once, each on its own VC --
        // forces real contention on node 0's inbound link(s) regardless of
        // topology, which is exactly where flits from different VCs would
        // get tangled together if VC isolation were broken.
        NocConcurrentTester.test(cfg, NocConcurrentTester.manyToOne(cfg, dst = 0))
      }
    }
  }

}