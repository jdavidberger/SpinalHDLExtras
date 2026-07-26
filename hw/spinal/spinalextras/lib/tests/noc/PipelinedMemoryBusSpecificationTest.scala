package spinalextras.lib.tests.noc

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusConfig}
import spinalextras.lib.Config
import spinalextras.lib.noc.NocConfig
import spinalextras.lib.noc.protocols.{NoCBuilder, PipelinedMemoryBusSpecification}
import spinalextras.lib.noc.topology.Mesh

import scala.collection.mutable
import scala.language.postfixOps
import scala.util.Random

/**
 * Wraps a single master <-> single slave PipelinedMemoryBusSpecification in a NoC, surfacing
 * both PMB ports as external IO: `masterBus` for the testbench to drive as a real bus master,
 * `slaveBus` for the testbench to model as a real bus slave (peripheral).
 */
class PipelinedMemoryBusSpecificationHarness(nocCfg: NocConfig, pmbConfig: PipelinedMemoryBusConfig,
                                              masterInputAddress: Int = -1, masterOutputAddress: Int = -1,
                                              slaveInputAddress: Int = -1, slaveOutputAddress: Int = -1) extends Component {
  val io = new Bundle {
    val masterBus = slave(PipelinedMemoryBus(pmbConfig))
    val slaveBus = master(PipelinedMemoryBus(pmbConfig))
  }

  val builder = new NoCBuilder(nocCfg)
  val spec = new PipelinedMemoryBusSpecification(pmbConfig, builder)

  spec.addMaster(io.masterBus, masterInputAddress, masterOutputAddress)
  spec.addSlave(io.slaveBus, SizeMapping(0, BigInt(1) << pmbConfig.addressWidth), slaveInputAddress, slaveOutputAddress)

  val noc = builder.build()
}

/**
 * Drives PipelinedMemoryBusSpecification's master port with writes/reads and models the slave
 * port as a simple word-addressed memory, checking that every write reads back correctly through
 * the NoC -- i.e. that the request reaches the right node and the response (routed back via the
 * subheader) makes it back to the master with the right data.
 */
class PipelinedMemoryBusSpecificationTest extends AnyFunSuite {

  def runBasicTest(masterInputAddress: Int = -1, masterOutputAddress: Int = -1,
                   slaveInputAddress: Int = -1, slaveOutputAddress: Int = -1): Unit = {
    val nocCfg = NocConfig(topology = new Mesh(2, 2), dataWidth = 32)
    val pmbConfig = PipelinedMemoryBusConfig(addressWidth = 8, dataWidth = 8)

    Config.sim.compile(new PipelinedMemoryBusSpecificationHarness(nocCfg, pmbConfig,
      masterInputAddress, masterOutputAddress, slaveInputAddress, slaveOutputAddress)).doSim(seed = 42) { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      SimTimeout(1 us)

      val io = dut.io
      io.masterBus.cmd.valid #= false
      io.slaveBus.cmd.ready #= true
      io.slaveBus.rsp.valid #= false

      dut.clockDomain.waitSampling(5)

      val mem = mutable.Map[BigInt, BigInt]().withDefaultValue(0)
      var running = true
      // Models the peripheral behind the slave port: a 1-cycle-latency memory.
      val slaveFork = fork {
        while (running) {
          dut.clockDomain.waitSampling()
          if(io.slaveBus.cmd.valid.toBoolean) {
            val addr = io.slaveBus.cmd.address.toBigInt
            if (io.slaveBus.cmd.write.toBoolean) {
              mem(addr) = io.slaveBus.cmd.data.toBigInt
            } else {
              dut.clockDomain.waitSampling()
              io.slaveBus.rsp.valid #= true
              io.slaveBus.rsp.data #= mem(addr)
              dut.clockDomain.waitSampling()
              io.slaveBus.rsp.valid #= false
            }
          }
        }
      }

      def write(addr: BigInt, data: BigInt): Unit = {
        io.masterBus.cmd.valid #= true
        io.masterBus.cmd.write #= true
        io.masterBus.cmd.address #= addr
        io.masterBus.cmd.data #= data
        io.masterBus.cmd.mask #= (BigInt(1) << (pmbConfig.dataWidth / 8)) - 1
        println(s"Writing ${io.masterBus.cmd.data.toBigInt} to ${io.masterBus.cmd.address.toBigInt}")
        dut.clockDomain.waitSamplingWhere(io.masterBus.cmd.ready.toBoolean)
        println(s"Writing done")
        io.masterBus.cmd.valid #= false
      }

      def read(addr: BigInt): BigInt = {
        io.masterBus.cmd.valid #= true
        io.masterBus.cmd.write #= false
        io.masterBus.cmd.address #= addr
        io.masterBus.cmd.data #= 0
        io.masterBus.cmd.mask #= 0
        println(s"Reading ${io.masterBus.cmd.data.toBigInt} to ${io.masterBus.cmd.address.toBigInt}")
        dut.clockDomain.waitSamplingWhere(io.masterBus.cmd.ready.toBoolean)
        io.masterBus.cmd.valid #= false
        println(s"Reading wait")
        dut.clockDomain.waitSamplingWhere(io.masterBus.rsp.valid.toBoolean)
        println(s"Reading done")
        val result = io.masterBus.rsp.data.toBigInt
        dut.clockDomain.waitSampling()
        result
      }

      val rnd = new Random(7)
      val addresses = 0 until 16
      val expected = mutable.Map[BigInt, BigInt]()

      for (addr <- addresses) {
        val data = BigInt(rnd.nextInt(256))
        write(addr, data)
        expected(addr) = data
      }

      for (addr <- addresses) {
        val got = read(addr)
        assert(got == expected(addr), s"address $addr: expected ${expected(addr)}, got $got")
      }

      // Overwrite and re-check a couple of entries, to make sure this isn't just "first write wins".
      for (addr <- Seq(BigInt(addresses.head), BigInt(addresses.last))) {
        val data = BigInt(rnd.nextInt(256))
        write(addr, data)
        expected(addr) = data
        val got = read(addr)
        assert(got == expected(addr), s"address $addr after overwrite: expected ${expected(addr)}, got $got")
      }

      println("Waiting for join")
      running = false
      slaveFork.join()
    }
  }

  test("PipelinedMemoryBusSpecification routes writes/reads correctly with explicit, independently-addressed input/output slots") {
    // Each port's input and output addresses are deliberately different from each other, proving
    // the two address spaces are tracked/resolved independently. Note they must still avoid
    // colliding a sender's injection node with its packet's own destination node (e.g. master's
    // input == slave's output) -- the underlying NoC fabric can't route a "local loopback" where a
    // packet enters and needs to exit at the very same node's local port, which is an existing
    // fabric limitation, not something specific to this specification.
    runBasicTest(masterInputAddress = 0, masterOutputAddress = 1, slaveInputAddress = 2, slaveOutputAddress = 3)
  }

  test("PipelinedMemoryBusSpecification routes writes/reads correctly with auto-assigned node addresses") {
    runBasicTest()
  }
}
