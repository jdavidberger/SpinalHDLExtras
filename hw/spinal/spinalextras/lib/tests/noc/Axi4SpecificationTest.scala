package spinalextras.lib.tests.noc

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba4.axi.{Axi4Config, Axi4Shared}
import spinal.lib.bus.misc.SizeMapping
import spinalextras.lib.Config
import spinalextras.lib.noc.NocConfig
import spinalextras.lib.noc.protocols.{Axi4Specification, NoCBuilder}
import spinalextras.lib.noc.topology.Mesh

import scala.collection.mutable
import scala.language.postfixOps

/**
 * Wraps a single master <-> single slave Axi4Specification in a NoC, surfacing both `Axi4Shared`
 * ports as external IO -- the AXI4 analogue of `PipelinedMemoryBusSpecificationHarness`.
 */
class Axi4SpecificationHarness(nocCfg: NocConfig, axiConfig: Axi4Config,
                                masterInputAddress: Int = -1, masterROutputAddress: Int = -1, masterBOutputAddress: Int = -1,
                                slaveInputAddress: Int = -1, slaveOutputAddress: Int = -1) extends Component {
  val io = new Bundle {
    val masterBus = slave(Axi4Shared(axiConfig))
    val slaveBus = master(Axi4Shared(axiConfig))
  }

  val builder = new NoCBuilder(nocCfg)
  val spec = new Axi4Specification(axiConfig, builder)

  spec.addMaster(io.masterBus, masterInputAddress, masterROutputAddress, masterBOutputAddress)
  spec.addSlave(io.slaveBus, SizeMapping(0, BigInt(1) << axiConfig.addressWidth), slaveInputAddress, slaveOutputAddress)

  val noc = builder.build()
}

class Axi4SpecificationTest extends AnyFunSuite {
  val nocCfg = NocConfig(topology = new Mesh(2, 2), dataWidth = 32)
  // Trims every optional Axi4Ax/W/R/B field this gateway doesn't itself rely on (region, burst,
  // lock, cache, qos, size, prot, len) down to just id/addr/write/data/strb/resp/last -- keeps the
  // NoC flit budget small; none of that trimming is required by Axi4Specification itself (see its
  // `require`s), it's purely to keep this testbench's driving code short.
  val axiConfig = Axi4Config(
    addressWidth = 8,
    dataWidth = 8,
    idWidth = 2,
    useId = true,
    useRegion = false,
    useBurst = false,
    useLock = false,
    useCache = false,
    useQos = false,
    useSize = false,
    useProt = false,
    useLen = false,
    useLast = true,
    useResp = true,
    useStrb = true
  )

  test("Axi4Specification routes a write then a read-back through the NoC") {
    Config.sim.compile(new Axi4SpecificationHarness(nocCfg, axiConfig)).doSim(seed = 42) { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      SimTimeout(2 us)

      val io = dut.io
      io.masterBus.arw.valid #= false
      io.masterBus.w.valid #= false
      io.masterBus.r.ready #= true
      io.masterBus.b.ready #= true
      io.slaveBus.arw.ready #= true
      io.slaveBus.w.ready #= true
      io.slaveBus.r.valid #= false
      io.slaveBus.b.valid #= false
      dut.clockDomain.waitSampling(5)

      val mem = mutable.Map[BigInt, BigInt]().withDefaultValue(0)
      var running = true
      // Models the peripheral behind the slave port: accepts arw immediately, then -- for a write
      // -- takes the following w beat, or -- for a read -- replies a cycle later.
      val slaveFork = fork {
        while (running) {
          dut.clockDomain.waitSampling()
          if (io.slaveBus.arw.valid.toBoolean) {
            val addr = io.slaveBus.arw.payload.addr.toBigInt
            val id = io.slaveBus.arw.payload.id.toBigInt
            val write = io.slaveBus.arw.payload.write.toBoolean
            if (write) {
              dut.clockDomain.waitSamplingWhere(io.slaveBus.w.valid.toBoolean)
              mem(addr) = io.slaveBus.w.payload.data.toBigInt
              dut.clockDomain.waitSampling()
              io.slaveBus.b.valid #= true
              io.slaveBus.b.payload.id #= id
              io.slaveBus.b.payload.resp #= 0
              dut.clockDomain.waitSamplingWhere(io.slaveBus.b.ready.toBoolean)
              io.slaveBus.b.valid #= false
            } else {
              dut.clockDomain.waitSampling()
              io.slaveBus.r.valid #= true
              io.slaveBus.r.payload.id #= id
              io.slaveBus.r.payload.data #= mem(addr)
              io.slaveBus.r.payload.resp #= 0
              io.slaveBus.r.payload.last #= true
              dut.clockDomain.waitSamplingWhere(io.slaveBus.r.ready.toBoolean)
              io.slaveBus.r.valid #= false
            }
          }
        }
      }

      def write(addr: BigInt, data: BigInt, id: Int): Unit = {
        io.masterBus.arw.valid #= true
        io.masterBus.arw.payload.write #= true
        io.masterBus.arw.payload.addr #= addr
        io.masterBus.arw.payload.id #= id
        dut.clockDomain.waitSamplingWhere(io.masterBus.arw.ready.toBoolean)
        io.masterBus.arw.valid #= false

        io.masterBus.w.valid #= true
        io.masterBus.w.payload.data #= data
        io.masterBus.w.payload.strb #= 1
        io.masterBus.w.payload.last #= true
        dut.clockDomain.waitSamplingWhere(io.masterBus.w.ready.toBoolean)
        io.masterBus.w.valid #= false

        dut.clockDomain.waitSamplingWhere(io.masterBus.b.valid.toBoolean)
      }

      def read(addr: BigInt, id: Int): BigInt = {
        io.masterBus.arw.valid #= true
        io.masterBus.arw.payload.write #= false
        io.masterBus.arw.payload.addr #= addr
        io.masterBus.arw.payload.id #= id
        dut.clockDomain.waitSamplingWhere(io.masterBus.arw.ready.toBoolean)
        io.masterBus.arw.valid #= false

        dut.clockDomain.waitSamplingWhere(io.masterBus.r.valid.toBoolean)
        io.masterBus.r.payload.data.toBigInt
      }

      write(5, 0x42, 0)
      val got = read(5, 1)
      assert(got == 0x42, s"expected 0x42, got $got")

      // Overwrite and re-check, to make sure this isn't just "first write wins".
      write(5, 0x17, 2)
      val got2 = read(5, 3)
      assert(got2 == 0x17, s"expected 0x17 after overwrite, got $got2")

      running = false
      slaveFork.join()
    }
  }

  test("Axi4Specification lets multiple outstanding reads complete out of order, matched by id") {
    Config.sim.compile(new Axi4SpecificationHarness(nocCfg, axiConfig)).doSim(seed = 7) { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      SimTimeout(2 us)

      val io = dut.io
      io.masterBus.arw.valid #= false
      io.masterBus.w.valid #= false
      io.masterBus.r.ready #= true
      io.masterBus.b.ready #= true
      io.slaveBus.arw.ready #= true
      io.slaveBus.w.ready #= true
      io.slaveBus.r.valid #= false
      io.slaveBus.b.valid #= false
      dut.clockDomain.waitSampling(5)

      // Pre-seed memory directly (bypassing AXI writes -- this test is only about response
      // routing/ordering, not write correctness, which the other test already covers).
      val addrA = BigInt(5)
      val addrB = BigInt(9)
      val mem = mutable.Map[BigInt, BigInt](addrA -> 0xAA, addrB -> 0xBB)

      case class PendingRead(id: Int, addr: BigInt, dueCycle: Long)
      val pending = mutable.ArrayBuffer[PendingRead]()
      // id=1 is issued *second* but finishes *first* -- if the master gateway (or the slave's
      // id-keyed response routing) secretly assumed request order == response order, id=0's data
      // would come back matched to id=1's request (or vice versa) instead of staying correctly
      // paired by id.
      val delayForId = Map(0 -> 12, 1 -> 3)
      var cycleCount = 0L
      var running = true

      val slaveFork = fork {
        while (running) {
          dut.clockDomain.waitSampling()
          cycleCount += 1

          if (io.slaveBus.arw.valid.toBoolean) {
            val id = io.slaveBus.arw.payload.id.toInt
            val addr = io.slaveBus.arw.payload.addr.toBigInt
            assert(!io.slaveBus.arw.payload.write.toBoolean, "this test only issues reads")
            pending += PendingRead(id, addr, cycleCount + delayForId(id))
          }

          pending.filter(_.dueCycle <= cycleCount).sortBy(_.dueCycle).headOption.foreach { req =>
            pending -= req
            io.slaveBus.r.valid #= true
            io.slaveBus.r.payload.id #= req.id
            io.slaveBus.r.payload.data #= mem(req.addr)
            io.slaveBus.r.payload.resp #= 0
            io.slaveBus.r.payload.last #= true
            dut.clockDomain.waitSamplingWhere(io.slaveBus.r.ready.toBoolean)
            io.slaveBus.r.valid #= false
          }
        }
      }

      def issueRead(addr: BigInt, id: Int): Unit = {
        io.masterBus.arw.valid #= true
        io.masterBus.arw.payload.write #= false
        io.masterBus.arw.payload.addr #= addr
        io.masterBus.arw.payload.id #= id
        dut.clockDomain.waitSamplingWhere(io.masterBus.arw.ready.toBoolean)
      }

      // Issue both reads back-to-back, without ever waiting on a response in between -- proving
      // the master gateway doesn't stall new commands on an outstanding one the way
      // PipelinedMemoryBusSpecification's single-outstanding-read gateway would have to.
      issueRead(addrA, 0)
      issueRead(addrB, 1)
      io.masterBus.arw.valid #= false

      val arrivalOrder = mutable.ArrayBuffer[Int]()
      val results = mutable.Map[Int, BigInt]()
      while (results.size < 2) {
        dut.clockDomain.waitSamplingWhere(io.masterBus.r.valid.toBoolean)
        val id = io.masterBus.r.payload.id.toInt
        arrivalOrder += id
        results(id) = io.masterBus.r.payload.data.toBigInt
        dut.clockDomain.waitSampling()
      }

      assert(arrivalOrder == Seq(1, 0),
        s"expected id=1's (shorter-delay) response to arrive before id=0's, got $arrivalOrder")
      assert(results(0) == mem(addrA), s"id=0: expected ${mem(addrA)}, got ${results(0)}")
      assert(results(1) == mem(addrB), s"id=1: expected ${mem(addrB)}, got ${results(1)}")

      running = false
      slaveFork.join()
    }
  }
}
