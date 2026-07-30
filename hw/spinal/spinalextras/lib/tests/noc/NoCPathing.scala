package spinalextras.lib.tests.noc

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim.SimManagerContext
import spinalextras.lib.Config
import spinalextras.lib.logging.{GlobalLogger, SignalLogger}
import spinalextras.lib.noc._
import spinalextras.lib.noc.protocols.DataStreamSpecification
import spinalextras.lib.noc.topology.Torus

import scala.collection.mutable
import scala.language.postfixOps
import scala.util.Random

/**
 * ============================================================================
 * Generic pathing test harness for arbitrary NoC topologies
 * ============================================================================
 *
 * `NocPathingHarness` wraps a `NoCBuilder(cfg)` sharing a single
 * `DataStreamSpecification`, registering every node as both a source and a
 * sink at its own physical address. This means the testbench never has to
 * know how `Header` gets bit-packed onto a `Flit` -- `DataStreamSpecification`
 * builds the header exactly the way any real integration would (via
 * `insertHeader`) and strips it back off on delivery (via `takeHead`), so
 * each node's external port only ever carries payload beats, never the
 * routing header. The testbench only has to supply the destination node and
 * a payload.
 */
class NocPathingHarness(cfg: NocConfig) extends Component {
  val n = cfg.topology.nodes

  val io = new Bundle {
    val rawInputs  = Vec(slave(Stream(Fragment(Bits(cfg.dataWidth bits)))), n)
    val destInputs = in(Vec(UInt(cfg.topology.addressSize bits), n))
    val rawOutputs = Vec(master(Stream(Fragment(Bits(cfg.dataWidth bits)))), n)
  }

  val builder = new NoCBuilder(cfg)
  val spec = new DataStreamSpecification(HardType(cfg.datatype), builder)

  for (i <- 0 until n) {
    val header = Header(cfg)
    header.dest := io.destInputs(i).resized
    header.application.setAll()

    io.rawInputs(i) <> spec.addSource(header.asBits.resized, i)
    spec.addSink(i) <> io.rawOutputs(i)
  }

  val noc = builder.build()
  val simLog = GlobalLogger.create_simulation_logger(tags = Set("noc-headers"))
}

/**
 * Drives packets through a `NocPathingHarness` and checks that every packet
 * arrives at the node it was actually addressed to, with its payload intact,
 * and that nothing shows up anywhere it shouldn't.
 *
 * Packets are injected strictly one at a time, globally: the previous packet
 * is fully drained before the next one is sent. That's a deliberate scoping
 * choice, not a requirement -- `Topology.createNodes` arbitrates the shared
 * external output with `fragmentLock`, so on the `Flit`-to-`Bits` boundary
 * this harness actually exercises, one packet's flits always finish before
 * the next one's begin, and interleaving isn't possible here regardless.
 * Testing one packet at a time just keeps this harness scoped to the
 * question it's meant to answer -- is the path from every source to every
 * destination correct? -- leaving genuine concurrency and VC isolation to
 * `NocConcurrentTester`.
 */
object NocPathingTester {

  case class Packet(src: Int, dst: Int, id: Int) {

  }
  private case class Arrival(node: Int, src: Int, dst: Int, id: Int)

  /** Every (src, dst) pair exactly once, including src == dst (local loopback). */
  def allPairs(cfg: NocConfig): Seq[Packet] = {
    val n = cfg.topology.nodes
    val vcs = Math.max(cfg.virtualChannels, 1)
    (for (src <- 0 until n; dst <- 0 until n) yield (src, dst)).zipWithIndex.map {
      case ((src, dst), id) => Packet(src, dst, id)
    }.filter(pkt => pkt.src != pkt.dst)
  }

  /** A random sample of (src, dst) pairs -- for topologies too large to test exhaustively. */
  def randomPairs(cfg: NocConfig, count: Int, seed: Long = 0): Seq[Packet] = {
    val n = cfg.topology.nodes
    val rnd = new Random(seed)
    (0 until count).map(id => Packet(rnd.nextInt(n), rnd.nextInt(n), id)).filter(pkt => pkt.src != pkt.dst)
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

    Config.sim.compile(new NocPathingHarness(cfg)).doSim(seed = simSeed) { dut =>
      dut.clockDomain.forkStimulus(period = 10)

      dut.simLog.startCapture(dut.clockDomain, SimManagerContext.current.manager.testName + ".sqlite")

      val n = dut.n
      for (node <- 0 until n) {
        dut.io.rawInputs(node).valid #= false
      }

      dut.clockDomain.waitSampling(10)

      // Every output port is always ready to accept; every accepted beat is
      // recorded here. `DataStreamSpecification` strips the routing header
      // before delivery, so every beat that arrives is our own 3-beat
      // payload (src, dst, id) -- there's no header flit to skip.
      val arrivals = mutable.Queue[Arrival]()
      val malformed = mutable.ArrayBuffer[String]()

      for (node <- 0 until n) {
        val port = dut.io.rawOutputs(node)
        port.ready #= true

        fork {
          val buf = mutable.ArrayBuffer[BigInt]()
          while (true) {
            dut.clockDomain.waitSamplingWhere(port.valid.toBoolean && port.ready.toBoolean)
            buf += port.payload.fragment.toBigInt
            if (port.payload.last.toBoolean) {
              if (buf.size == 3) {
                arrivals.enqueue(Arrival(node, buf(0).toInt, buf(1).toInt, buf(2).toInt))
                println(s"Buffer arrived ${arrivals.last}")
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
        println(s"Sending ${p.src} -> ${p.dst} ")
        dut.io.destInputs(p.src) #= cfg.topology.addressToRouteableAddress(p.dst)

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
  test("pathing is correct (sampled) on a larger mesh: Torus(6x6)") {
    val cfg = NocConfig(topology = new Torus((6, 6)))
    NocPathingTester.test(cfg, packets = NocPathingTester.randomPairs(cfg, count = 200))
  }
}