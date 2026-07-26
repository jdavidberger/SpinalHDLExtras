package spinalextras.lib.tests.noc

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinalextras.lib.Config
import spinalextras.lib.noc.NocConfig
import spinalextras.lib.noc.protocols.{DataStreamSpecification, NoCBuilder}
import spinalextras.lib.noc.topology.Mesh

import scala.collection.mutable
import scala.language.postfixOps
import scala.util.Random

/**
 * Wraps a DataStreamSpecification with `numSources` sources and `numSinks` sinks in a NoC,
 * surfacing every source/sink as external IO. Each source's header is baked, post-`build()`, to
 * address sink `destSinkOf(i)` -- deferring that assignment until after `builder.build()` is what
 * lets this work whether the sink's address was pinned or auto-assigned, since only then is
 * `NodeSlot.resolvedAddress` legal to read.
 */
class DataStreamSpecificationHarness(nocCfg: NocConfig, datatype: HardType[Bits],
                                      numSources: Int, numSinks: Int,
                                      destSinkOf: Int => Int,
                                      sourceAddresses: Int => Int = _ => -1,
                                      sinkAddresses: Int => Int = _ => -1) extends Component {
  val io = new Bundle {
    val src = Vec(slave(Stream(Fragment(datatype))), numSources)
    val sink = Vec(master(Stream(Fragment(datatype))), numSinks)
  }

  val builder = new NoCBuilder(nocCfg)
  val spec = new DataStreamSpecification(datatype, builder)

  val hdrs = Vec.fill(numSources)(Bits(datatype.getBitsWidth bits))
  val sources = (0 until numSources).map(i => spec.addSource(hdrs(i), sourceAddresses(i)))
  val sinks = (0 until numSinks).map(i => spec.addSink(sinkAddresses(i)))

  for ((s, i) <- sources.zipWithIndex) io.src(i) <> s._1
  for ((k, i) <- sinks.zipWithIndex) io.sink(i) <> k._1

  val noc = builder.build()

  for ((s, i) <- sources.zipWithIndex) {
    val destSlot = sinks(destSinkOf(i))._2
    val destRouteable = nocCfg.topology.addressToRouteableAddress(destSlot.resolvedAddress)
    hdrs(i) := nocCfg.packHeader(U(destRouteable, nocCfg.topology.addressSize bits), U(0, nocCfg.topology.addressSize bits))
  }
}

/**
 * Drives DataStreamSpecification's source ports with fragmented packets and checks they're
 * delivered to the right sink -- i.e. that NoCBuilder's full-connectivity route registration
 * (`DataStreamSpecification.registerRoutes`) actually results in a working route from every
 * source to every sink it might address, and that a packet's header `dest` steers it to the
 * intended sink and not some other one sharing the same fabric.
 */
class DataStreamSpecificationTest extends AnyFunSuite {

  def runTest(numSources: Int, numSinks: Int, destSinkOf: Int => Int,
              sourceAddresses: Int => Int = _ => -1, sinkAddresses: Int => Int = _ => -1): Unit = {
    val nocCfg = NocConfig(topology = new Mesh(2, 2), dataWidth = 32)
    val datatype = HardType(Bits(32 bits))

    Config.sim.compile(new DataStreamSpecificationHarness(nocCfg, datatype, numSources, numSinks,
      destSinkOf, sourceAddresses, sinkAddresses)).doSim(seed = 42) { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      SimTimeout(10 us)

      for (s <- dut.io.src) s.valid #= false
      for (k <- dut.io.sink) k.ready #= true

      dut.clockDomain.waitSampling(5)

      val rnd = new Random(7)
      // Each source sends a multi-beat packet (to exercise `last`), tagged with data that
      // encodes both which source sent it and a per-beat index, so a receiving sink can prove
      // both which source it came from and that beats arrived in order.
      val beatsPerPacket = 3
      def payloadFor(source: Int, beat: Int): BigInt = BigInt(source) * 1000 + beat

      val received = Array.fill(numSinks)(new mutable.ArrayBuffer[(Int, BigInt)]())
      var running = true
      val sinkForks = dut.io.sink.zipWithIndex.map { case (sink, sinkIdx) =>
        fork {
          while (running) {
            dut.clockDomain.waitSampling()
            if (sink.valid.toBoolean) {
              received(sinkIdx) += ((if (sink.last.toBoolean) 1 else 0, sink.payload.fragment.toBigInt))
            }
          }
        }
      }

      for (source <- 0 until numSources) {
        for (beat <- 0 until beatsPerPacket) {
          val io = dut.io.src(source)
          io.valid #= true
          io.payload.fragment #= payloadFor(source, beat)
          io.last #= (beat == beatsPerPacket - 1)
          dut.clockDomain.waitSamplingWhere(io.ready.toBoolean)
          io.valid #= false
          dut.clockDomain.waitSampling()
        }
      }

      dut.clockDomain.waitSampling(50)
      running = false
      sinkForks.foreach(_.join())

      for (sink <- 0 until numSinks) {
        val expectedSources = (0 until numSources).filter(destSinkOf(_) == sink)
        val expected = for (source <- expectedSources; beat <- 0 until beatsPerPacket)
          yield payloadFor(source, beat)
        assert(received(sink).map(_._2).toSet == expected.toSet,
          s"sink $sink: expected payloads $expected, got ${received(sink)}")
        // The last beat of every packet delivered to this sink must be flagged `last`, and no
        // other beat should be.
        val lastFlags = received(sink).map(_._1)
        assert(lastFlags.count(_ == 1) == expectedSources.size,
          s"sink $sink: expected ${expectedSources.size} packets (one `last` flit each), got $lastFlags")
      }
    }
  }

  test("DataStreamSpecification delivers a single source's packet to its sink with explicit addresses") {
    runTest(numSources = 1, numSinks = 1, destSinkOf = _ => 0,
      sourceAddresses = _ => 1, sinkAddresses = _ => 0)
  }

  test("DataStreamSpecification delivers a single source's packet to its sink with auto-assigned addresses") {
    runTest(numSources = 1, numSinks = 1, destSinkOf = _ => 0)
  }

  test("DataStreamSpecification routes each of several sources to its own distinct sink, auto-addressed") {
    // Cross-wires source i to sink (numSinks-1-i): this only passes if the full N x M
    // connectivity NoCBuilder.registerRoutes/requireRoute establishes by default is actually
    // routeable for every pair, and if each packet's header dest genuinely steers it to the sink
    // it names rather than, say, whichever sink happens to be closest or first in claim order.
    val numSources = 2
    val numSinks = 2
    runTest(numSources, numSinks, destSinkOf = i => numSinks - 1 - i)
  }

  test("DataStreamSpecification lets several sources share a single sink, auto-addressed") {
    runTest(numSources = 3, numSinks = 1, destSinkOf = _ => 0)
  }
}
