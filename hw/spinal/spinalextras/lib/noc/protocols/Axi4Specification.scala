package spinalextras.lib.noc.protocols

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.{Axi4Arw, Axi4B, Axi4Config, Axi4R, Axi4Shared, Axi4W}
import spinal.lib.bus.misc.AddressMapping
import spinalextras.lib.misc.StreamTools
import spinalextras.lib.misc.StreamTools.CreateFragment
import spinalextras.lib.noc.{Header, NocConfig}

import scala.collection.mutable
import scala.language.postfixOps

/**
 * Exposes an id-tagged AXI4 master/slave fabric on top of a NoC -- the AXI4 analogue of
 * `PipelinedMemoryBusSpecification`, using `Axi4Shared` (address+data merged into one `arw`
 * channel) as the external interface so every transaction still maps to exactly one NoC packet
 * per direction, the same way a PMB command does.
 *
 * Each AXI4 port -- master or slave -- needs a NoC injection address (where it feeds packets into
 * the fabric) the same way PMB ports do, but a master needs *two* independent delivery addresses
 * rather than PMB's one: a slave can't know, until a transaction actually completes, whether the
 * next response it happens to produce is read data or a write acknowledgement, so those two
 * response kinds are routed to two different physical nodes -- `rOutput` for read data, `bOutput`
 * for write responses -- rather than trying to multiplex both onto a single delivery stream.
 *
 * Each transaction (one burst) is carried as exactly one NoC packet in each direction:
 *   - Request: a routing Header flit (`dest` = the target slave's output address), then the
 *     serialized `Axi4Arw` descriptor, then -- only for writes -- one `Axi4W` flit per burst beat
 *     (`Fragment.last` mirrors `w.last`). A read's packet ends on the descriptor flit itself,
 *     since it has no data phase.
 *   - Response: a routing Header flit (`dest` = the requesting master's `bOutput`, for a write
 *     response, or `rOutput`, for read data), then either a single `Axi4B` flit or one `Axi4R`
 *     flit per burst beat (`Fragment.last` mirrors `r.last`).
 *
 * Unlike `PipelinedMemoryBusSpecification`'s single-outstanding-read restriction -- needed there
 * only because PMB's `rsp` is an untagged Flow with no way to say which request it answers -- AXI4
 * carries its `id` field on every response flit, so neither gateway here ever stalls new commands
 * waiting on an old response: a master may have many transactions in flight at once, and a slave
 * may complete them in any order. The one rule this relies on is exactly what any AXI4
 * interconnect already requires of a well-behaved master: never have two transactions outstanding
 * at the same time under the same `id` (an `assert` in `Axi4NocSlave` catches a violation).
 */
class Axi4Specification(axiConfig: Axi4Config, builder: NoCBuilder) extends ProtocolSpecification(builder) {
  private val cfg = builder.cfg
  private val addressSize = cfg.topology.addressSize

  require(axiConfig.useId && axiConfig.idWidth > 0,
    "Axi4Specification requires AXI ids (Axi4Config.useId, idWidth > 0) -- that's what lets multiple transactions stay outstanding at once")
  require(axiConfig.useLast,
    "Axi4Specification requires Axi4Config.useLast to delimit bursts")
  require(cfg.headerApplicationBits >= addressSize,
    s"NoC dataWidth (${cfg.dataWidth}) is too small to carry a return-address subheader for a ${cfg.topology.nodes}-node topology")

  private val arwBits = Axi4Arw(axiConfig).getBitsWidth
  private val wBits = Axi4W(axiConfig).getBitsWidth
  private val rBits = Axi4R(axiConfig).getBitsWidth
  private val bBits = Axi4B(axiConfig).getBitsWidth

  for ((name, bits) <- Seq("Axi4Arw" -> arwBits, "Axi4W" -> wBits, "Axi4R" -> rBits, "Axi4B" -> bBits))
    require(bits <= cfg.dataWidth, s"$name ($bits bits) does not fit in a single NoC flit (${cfg.dataWidth} bits)")

  /** A node's NoC injection point, plus -- unlike `PipelinedMemoryBusSpecification.NodePorts` --
    * two independent delivery points for a master (see class doc for why one isn't enough). */
  case class MasterPorts(input: NodeSlot, rOutput: NodeSlot, bOutput: NodeSlot)
  case class SlavePorts(input: NodeSlot, output: NodeSlot)

  private case class MasterPort(bus: Axi4Shared, ports: MasterPorts)
  private case class SlavePort(bus: Axi4Shared, mapping: AddressMapping, ports: SlavePorts)

  private val masterPorts = new mutable.ArrayBuffer[MasterPort]()
  private val slavePorts = new mutable.ArrayBuffer[SlavePort]()

  /** @param inputAddress Physical NoC node this master injects requests (arw, then w) from; -1 (default) auto-assigns.
    * @param rOutputAddress Physical NoC node this master's read data is delivered to; -1 (default) auto-assigns.
    * @param bOutputAddress Physical NoC node this master's write responses are delivered to; -1 (default) auto-assigns. */
  def addMaster(bus: Axi4Shared, inputAddress: Int = -1, rOutputAddress: Int = -1, bOutputAddress: Int = -1): MasterPorts = {
    require(bus.config == axiConfig, "Axi4Shared config mismatch")
    val ports = MasterPorts(builder.createInputSlot(inputAddress), builder.createOutputSlot(rOutputAddress), builder.createOutputSlot(bOutputAddress))
    masterPorts += MasterPort(bus, ports)
    ports
  }

  /** @param inputAddress Physical NoC node this slave injects responses (r, b) from; -1 (default) auto-assigns.
    * @param outputAddress Physical NoC node this slave's requests are delivered to (what masters route to); -1 (default) auto-assigns. */
  def addSlave(bus: Axi4Shared, mapping: AddressMapping, inputAddress: Int = -1, outputAddress: Int = -1): SlavePorts = {
    require(bus.config == axiConfig, "Axi4Shared config mismatch")
    val ports = SlavePorts(builder.createInputSlot(inputAddress), builder.createOutputSlot(outputAddress))
    slavePorts += SlavePort(bus, mapping, ports)
    ports
  }

  // Every master can address every slave, and every slave's response must reach both of a
  // master's delivery addresses -- same full-connectivity assumption PipelinedMemoryBusSpecification
  // makes, just with one extra route per pair for the split read/write response delivery.
  override def registerRoutes(): Unit = {
    for (m <- masterPorts; s <- slavePorts) {
      builder.requireRoute(m.ports.input, s.ports.output)
      builder.requireRoute(s.ports.input, m.ports.rOutput)
      builder.requireRoute(s.ports.input, m.ports.bOutput)
    }
  }

  override def build(): Unit = {
    for (m <- masterPorts) buildMaster(m)
    for (s <- slavePorts) buildSlave(s)
  }

  private def buildMaster(m: MasterPort): Area = new Area {
    val inputAddress = m.ports.input.resolvedAddress
    val rOutputAddress = m.ports.rOutput.resolvedAddress
    val bOutputAddress = m.ports.bOutput.resolvedAddress

    val masterAdapter = new Axi4NocMaster(cfg, axiConfig,
      cfg.topology.addressToRouteableAddress(rOutputAddress),
      cfg.topology.addressToRouteableAddress(bOutputAddress),
      slavePorts.map(s => (s.mapping, cfg.topology.addressToRouteableAddress(s.ports.output.resolvedAddress))))
    masterAdapter.setName(s"axiMasterAdapter_${cfg.topology.addressName(inputAddress)}")
    masterAdapter.io.bus <> m.bus
    builder.addInput(masterAdapter.io.output, inputAddress)
    builder.addOutput(masterAdapter.io.rInput, rOutputAddress)
    builder.addOutput(masterAdapter.io.bInput, bOutputAddress)
  }.setName(s"axiMaster_${m.ports.input.resolvedAddress}")

  private def buildSlave(s: SlavePort): Area = new Area {
    val inputAddress = s.ports.input.resolvedAddress
    val outputAddress = s.ports.output.resolvedAddress

    val slaveAdapter = new Axi4NocSlave(cfg, axiConfig)
    slaveAdapter.setName(s"axiSlaveAdapter_${cfg.topology.addressName(outputAddress)}")
    slaveAdapter.io.bus <> s.bus
    builder.addOutput(slaveAdapter.io.input, outputAddress)
    builder.addInput(slaveAdapter.io.output, inputAddress)
  }.setName(s"axiSlave_${s.ports.input.resolvedAddress}_${s.ports.output.resolvedAddress}")
}

/** @param rOutputAddress this master's own read-data delivery address (already resolved via
  *                        `addressToRouteableAddress`), embedded as the return-address subheader
  *                        on every outgoing read request.
  * @param bOutputAddress this master's own write-response delivery address, embedded as the
  *                        return-address subheader on every outgoing write request.
  * @param slaveMappings each downstream slave's address hit-test paired with its already-resolved
  *                       (`addressToRouteableAddress`) NoC routing address. */
class Axi4NocMaster(val cfg: NocConfig, val axiConfig: Axi4Config,
                     val rOutputAddress: Int, val bOutputAddress: Int,
                     val slaveMappings: IndexedSeq[(AddressMapping, Int)]) extends Component {
  private val addressSize = cfg.topology.addressSize

  val io = new Bundle {
    val bus = slave(Axi4Shared(axiConfig))

    val rInput = slave(Stream(Fragment(Bits(cfg.dataWidth bits))))
    val bInput = slave(Stream(Fragment(Bits(cfg.dataWidth bits))))
    val output = master(Stream(Fragment(Bits(cfg.dataWidth bits))))
  }
  val bus = io.bus

  val hits = slaveMappings.map(s => s._1.hit(bus.arw.payload.addr))
  when(bus.arw.valid) {
    assert(CountOne(hits) === U(1), "Axi4NocMaster: arw address matched != 1 slave mapping")
  }
  val destNode = MuxOH(hits, slaveMappings.map(s => U(s._2, addressSize bits)))
  val returnAddress = Mux(bus.arw.payload.write, U(bOutputAddress, addressSize bits), U(rOutputAddress, addressSize bits))

  // Request packetizing: [Arw descriptor], then -- only for writes -- one W beat per burst beat
  // (Fragment.last mirrors w.last). A read's packet closes on the descriptor flit itself, since
  // it has no data phase -- that asymmetry (0 vs. len+1 trailing beats, decided by a bit *within*
  // the leading flit) is exactly what `Stream.insertHeader` can't express on its own, hence the
  // explicit two-phase state below rather than reusing it here too.
  val sendingDescriptor = RegInit(True)

  val contentOut = Stream(Fragment(Bits(cfg.dataWidth bits)))
  contentOut.valid := Mux(sendingDescriptor, bus.arw.valid, bus.w.valid)
  contentOut.payload.fragment := Mux(sendingDescriptor,
    bus.arw.payload.asBits.resize(cfg.dataWidth bits),
    bus.w.payload.asBits.resize(cfg.dataWidth bits)
  )
  contentOut.payload.last := Mux(sendingDescriptor, !bus.arw.payload.write, bus.w.payload.last)

  bus.arw.ready := sendingDescriptor && contentOut.ready
  bus.w.ready := !sendingDescriptor && contentOut.ready

  when(contentOut.fire) {
    when(sendingDescriptor) {
      sendingDescriptor := !bus.arw.payload.write
    } otherwise {
      when(bus.w.payload.last) {
        sendingDescriptor := True
      }
    }
  }

  io.output <> contentOut.insertHeader(cfg.packHeader(destNode, returnAddress))

  // Response destuffing needs no phase logic at all -- unlike the request side, every R/B flit
  // is fully self-describing (each carries its own `id`), so responses for different ids never
  // need to be told apart by position in the stream; whatever consumes `bus.r`/`bus.b` handles
  // that matching itself, same as it would talking to any other AXI4 interconnect.
  val (_, bPayload) = StreamTools.takeHead(io.bInput)
  bus.b.valid := bPayload.valid
  bus.b.payload.assignFromBits(bPayload.payload.fragment.resize(bus.b.payload.getBitsWidth bits))
  bPayload.ready := bus.b.ready

  val (_, rPayload) = StreamTools.takeHead(io.rInput)
  bus.r.valid := rPayload.valid
  bus.r.payload.assignFromBits(rPayload.payload.fragment.resize(bus.r.payload.getBitsWidth bits))
  // The wire's own `last` bit (part of the Axi4R serialization above) is redundant with -- and
  // may not even be assigned consistently with -- Fragment framing, so override it from the
  // latter, the same way `Axi4AxUnburstified.unburstify` overrides `addr` post-assignFromBits.
  bus.r.payload.last.removeAssignments()
  bus.r.payload.last := rPayload.payload.last
  rPayload.ready := bus.r.ready
}

class Axi4NocSlave(val cfg: NocConfig, val axiConfig: Axi4Config) extends Component {
  private val addressSize = cfg.topology.addressSize
  private val numIds = 1 << axiConfig.idWidth

  val io = new Bundle {
    val bus = master(Axi4Shared(axiConfig))

    val input = slave(Stream(Fragment(Bits(cfg.dataWidth bits))))
    val output = master(Stream(Fragment(Bits(cfg.dataWidth bits))))
  }
  val bus = io.bus

  val (hdrBits, payload) = StreamTools.takeHead(io.input)
  val hdr = Header(cfg)
  hdr.assignFromBits(hdrBits)
  val sourceAddress = hdr.application(addressSize - 1 downto 0).asUInt

  // Request destuffing: the packet is always [Arw descriptor] then, only for writes, [W beat] *
  // (len+1) -- arw.write, known the instant the descriptor flit arrives, picks which of those two
  // shapes this packet is.
  val awaitingDescriptor = RegInit(True)

  val arw = Axi4Arw(axiConfig)
  arw.assignFromBits(payload.payload.fragment.resize(arw.getBitsWidth bits))

  bus.arw.valid := payload.valid && awaitingDescriptor
  bus.arw.payload := arw

  bus.w.valid := payload.valid && !awaitingDescriptor
  bus.w.payload.assignFromBits(payload.payload.fragment.resize(bus.w.payload.getBitsWidth bits))
  // See the matching override in Axi4NocMaster's R destuffing for why this replaces, rather than
  // trusts, the wire's own `last` bit.
  bus.w.payload.last.removeAssignments()
  bus.w.payload.last := payload.payload.last

  payload.ready := Mux(awaitingDescriptor, bus.arw.ready, bus.w.ready)

  when(payload.fire) {
    when(awaitingDescriptor) {
      awaitingDescriptor := !arw.write
    } otherwise {
      when(payload.payload.last) {
        awaitingDescriptor := True
      }
    }
  }

  // Per-id bookkeeping so a response -- arriving an arbitrary time later, and in any order
  // relative to other ids, since a real AXI4 slave is free to complete different ids out of
  // order -- can still be routed back to whichever master's request set it up. This is the one
  // piece PipelinedMemoryBusSpecification doesn't need: its untagged rsp Flow instead forces a
  // single outstanding read and a receive-order FIFO (`PendingRsp`) to do the same job.
  val idSource = Vec(Reg(UInt(addressSize bits)), numIds)
  val idOutstanding = Vec(RegInit(False), numIds)

  when(bus.arw.fire) {
    assert(!idOutstanding(arw.id),
      "Axi4NocSlave: id already outstanding -- a master issued a second request with this id before the first one completed")
    idOutstanding(arw.id) := True
    idSource(arw.id) := sourceAddress
  }
  when(bus.b.fire) {
    idOutstanding(bus.b.payload.id) := False
  }
  when(bus.r.fire && bus.r.payload.last) {
    idOutstanding(bus.r.payload.id) := False
  }

  val bPacket = bus.b.map(b => CreateFragment(b.asBits.resize(cfg.dataWidth bits), True))
    .insertHeader(cfg.packHeader(idSource(bus.b.payload.id), U(0, addressSize bits)))
  val rPacket = bus.r.map(r => CreateFragment(r.asBits.resize(cfg.dataWidth bits), r.last))
    .insertHeader(cfg.packHeader(idSource(bus.r.payload.id), U(0, addressSize bits)))

  io.output <> StreamArbiterFactory().roundRobin.fragmentLock.on(Seq(bPacket, rPacket))
}
