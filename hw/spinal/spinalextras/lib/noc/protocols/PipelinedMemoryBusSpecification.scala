package spinalextras.lib.noc.protocols

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.AddressMapping
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusCmd, PipelinedMemoryBusConfig}
import spinalextras.lib.misc.StreamTools
import spinalextras.lib.misc.StreamTools.CreateFragment
import spinalextras.lib.noc.{Header, NocConfig}

import scala.collection.mutable
import scala.language.postfixOps

/** A rsp payload paired with the requesting master's NoC node address, queued in receive order. */
case class PendingRsp(rspBits: Int, addressSize: Int) extends Bundle {
  val data = Bits(rspBits bits)
  val dest = UInt(addressSize bits)
}

class PipelinedMemoryNocSlave(val cfg : NocConfig, val pmbConfig : PipelinedMemoryBusConfig) extends Component {
  private val cmdBits = 1 + pmbConfig.addressWidth + pmbConfig.dataWidth + pmbConfig.dataWidth / 8
  private val rspBits = pmbConfig.dataWidth

  val io = new Bundle {
    val bus = master(PipelinedMemoryBus(pmbConfig))

    val input = slave(Stream(Fragment(Bits(cfg.dataWidth bits))))
    val output = master(Stream(Fragment(Bits(cfg.dataWidth bits))))
  }

  private val addressSize = cfg.topology.addressSize
  val bus = io.bus

  val cmdIn = io.input

  val (cmdHeaderBits, cmdPayload) = StreamTools.takeHead(cmdIn)
  val cmdHeader = Header(cfg)
  cmdHeader.assignFromBits(cmdHeaderBits)
  val sourceAddress = cmdHeader.application(addressSize - 1 downto 0).asUInt

  val cmd = PipelinedMemoryBusCmd(pmbConfig)
  cmd.assignFromBits(cmdPayload.payload.fragment.resize(cmdBits bits))

  // Tracks, in receive order, which requesting master a read is owed back to -- depth is
  // generous headroom rather than a hard requirement, since only reads that are actually
  // in flight ever occupy a slot and every master permits just one outstanding read itself.
  val pendingSrc = StreamFifo(UInt(addressSize bits), depth = 4)
  pendingSrc.io.push.valid := cmdPayload.fire && !cmd.write
  pendingSrc.io.push.payload := sourceAddress

  bus.cmd.valid := cmdPayload.valid
  bus.cmd.payload := cmd
  cmdPayload.ready := bus.cmd.ready && (cmd.write || pendingSrc.io.push.ready)

  // bus.rsp is a Flow (no backpressure): it must always be captured the cycle it fires. That's
  // safe here without ever overflowing because pendingSrc (above) already bounds the number of
  // reads in flight at this slave to its own depth, so a same-depth FIFO here can never see more
  // pushes than it has room for.
  val rspRoute = PendingRsp(rspBits, addressSize)
  val rspFifo = StreamFifo(rspRoute, depth = 4)
  rspFifo.io.push.valid := bus.rsp.valid
  rspFifo.io.push.payload.data := bus.rsp.payload.asBits
  rspFifo.io.push.payload.dest := pendingSrc.io.pop.payload
  pendingSrc.io.pop.ready := bus.rsp.valid

  val rspOut = Stream(Fragment(Bits(cfg.dataWidth bits)))
  rspOut.valid := rspFifo.io.pop.valid
  rspOut.payload.fragment := rspFifo.io.pop.payload.data.resize(cfg.dataWidth bits)
  rspOut.payload.last := True
  rspFifo.io.pop.ready := rspOut.ready

  io.output <> rspOut.insertHeader(cfg.packHeader(rspFifo.io.pop.payload.dest, U(0, addressSize bits)))
}

/** @param slaveMappings each downstream slave's address hit-test paired with its already-resolved
  *                       (`addressToRouteableAddress`) NoC routing address. */
class PipelinedMemoryNocMaster(val cfg : NocConfig, val pmbConfig : PipelinedMemoryBusConfig,
                                val address : Int, val slaveMappings : IndexedSeq[(AddressMapping, Int)]) extends Component {
  private val rspBits = pmbConfig.dataWidth

  val io = new Bundle {
    val bus = slave(PipelinedMemoryBus(pmbConfig))

    val input = slave(Stream(Fragment(Bits(cfg.dataWidth bits))))
    val output = master(Stream(Fragment(Bits(cfg.dataWidth bits))))
  }

  private val addressSize = cfg.topology.addressSize
  val bus = io.bus

  val hits = slaveMappings.map(s => s._1.hit(bus.cmd.address))
  when(bus.cmd.valid) {
    assert(CountOne(hits) === U(1), "PipelinedMemoryBusSpecification: master command address matched != 1 slave mapping")
  }
  val destNode = MuxOH(hits, slaveMappings.map(s => U(s._2, addressSize bits)))

  // Only one outstanding read may be in flight at a time -- see class doc for why.
  val waitingRsp = RegInit(False)
  val gatedCmd = bus.cmd.haltWhen(waitingRsp)

  val cmdOut = gatedCmd.map(c => CreateFragment(c.asBits.resize(cfg.dataWidth bits), True))
  when(cmdOut.fire && !gatedCmd.payload.write) {
    waitingRsp := True
  }

  io.output <> cmdOut.insertHeader(cfg.packHeader(destNode, U(address, addressSize bits)))

  val (_, rspPayload) = StreamTools.takeHead(io.input)
  rspPayload.ready := True
  bus.rsp.valid := rspPayload.valid
  bus.rsp.payload.assignFromBits(rspPayload.payload.fragment.resize(rspBits bits))

  when(rspPayload.fire) {
    waitingRsp := False
  }
}

/**
 * Exposes a PipelinedMemoryBus master/slave fabric on top of a NoC -- the PMB analogue of
 * `PipelinedMemoryBusInterconnect`'s addMaster/addSlave, except that instead of building a
 * static crossbar (decoder + arbiter per bus), traffic is routed as NoC flits, and every
 * master/slave's physical NoC node address is optional (pass -1, the default, to have one
 * auto-assigned from whatever nodes are still free).
 *
 * Each PMB transaction is carried as exactly one NoC packet: a routing Header flit (NoC
 * `dest` = the target node) followed by a single payload flit holding the serialized
 * `PipelinedMemoryBusCmd` (request direction) or the rsp data (response direction). A slave
 * node can be shared by several masters, so a response needs a way back to whichever master
 * issued the request -- the Header's `application` bits, otherwise unused once routing has
 * picked a path, carry that requesting master's own node address as a subheader, which the
 * slave gateway echoes back as the `dest` of its response packet.
 *
 * `PipelinedMemoryBusRsp` is a Flow (no backpressure, no per-transaction tag): whichever
 * response shows up next is assumed to answer the oldest outstanding read. NoC routes can have
 * different latencies, so nothing guarantees responses come back in send order once more than
 * one read is in flight -- to preserve PMB's ordering assumption, each master gateway allows only
 * one outstanding (read) transaction at a time, stalling new commands until the previous read's
 * response has been forwarded.
 */
class PipelinedMemoryBusSpecification(pmbConfig: PipelinedMemoryBusConfig, builder: NoCBuilder) extends ProtocolSpecification(builder) {
  private val cfg = builder.cfg
  private val addressSize = cfg.topology.addressSize

  require(cfg.headerApplicationBits >= addressSize,
    s"NoC dataWidth (${cfg.dataWidth}) is too small to carry a return-address subheader for a ${cfg.topology.nodes}-node topology")

  private val cmdBits = 1 + pmbConfig.addressWidth + pmbConfig.dataWidth + pmbConfig.dataWidth / 8
  private val rspBits = pmbConfig.dataWidth

  require(cmdBits <= cfg.dataWidth,
    s"PipelinedMemoryBusCmd ($cmdBits bits: write+address+data+mask) does not fit in a single NoC flit (${cfg.dataWidth} bits)")
  require(rspBits <= cfg.dataWidth,
    s"PipelinedMemoryBusRsp ($rspBits bits) does not fit in a single NoC flit (${cfg.dataWidth} bits)")

  private case class MasterPort(bus: PipelinedMemoryBus, address: NodeSlot)
  private case class SlavePort(bus: PipelinedMemoryBus, mapping: AddressMapping, address: NodeSlot)

  private val masterPorts = new mutable.ArrayBuffer[MasterPort]()
  private val slavePorts = new mutable.ArrayBuffer[SlavePort]()

  /** @param address Physical NoC node this master attaches to; -1 (default) auto-assigns the next free node. */
  def addMaster(bus: PipelinedMemoryBus, address: Int = -1): NodeSlot = {
    require(bus.config == pmbConfig, "PipelinedMemoryBus config mismatch")
    val a = builder.createSlot(address)
    masterPorts += MasterPort(bus, a)
    a
  }

  /** @param address Physical NoC node this slave attaches to; -1 (default) auto-assigns the next free node. */
  def addSlave(bus: PipelinedMemoryBus, mapping: AddressMapping, address: Int = -1): NodeSlot = {
    require(bus.config == pmbConfig, "PipelinedMemoryBus config mismatch")
    val a = builder.createSlot(address)
    slavePorts += SlavePort(bus, mapping, a)
    a
  }

  private def buildMaster(m: MasterPort): Area = new Area {
    val address = m.address.resolvedAddress
    val masterAdapter = new PipelinedMemoryNocMaster(cfg, pmbConfig, address,
      slavePorts.map(s => (s.mapping, cfg.topology.addressToRouteableAddress(s.address.resolvedAddress))))
    masterAdapter.setName(s"masterAdapter_${cfg.topology.addressName(address)}")
    masterAdapter.io.bus <> m.bus
    builder.addInput(masterAdapter.io.output, address)
    builder.addOutput(masterAdapter.io.input, address)
  }.setName(s"pmbMaster_${m.address.resolvedAddress}")

  private def buildSlave(s: SlavePort): Area = new Area {
    val address = s.address.resolvedAddress

    val slaveAdapter = new PipelinedMemoryNocSlave(cfg, pmbConfig)
    slaveAdapter.setName(s"slaveAdapter_${cfg.topology.addressName(address)}")
    slaveAdapter.io.bus <> s.bus
    builder.addOutput(slaveAdapter.io.input, address)
    builder.addInput(slaveAdapter.io.output, address)
  }.setName(s"pmbSlave_${s.address.resolvedAddress}")

  override def build(): Unit = {
    for (m <- masterPorts) buildMaster(m)
    for (s <- slavePorts) buildSlave(s)
  }
}
