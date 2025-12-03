package spinalextras.lib.logging

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim.SimPublic
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.regif.RegInst
import spinal.lib.bus.wishbone.{AddressGranularity, Wishbone, WishboneConfig}
import spinalextras.lib.bus.WishboneGlobalBus
import spinalextras.lib.bus.general.BusSlaveProvider
import spinalextras.lib.formal.StreamFormal.StreamExt
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties, FormalProperty}
import spinalextras.lib.memory.MemoryBackedFifo
import spinalextras.lib.misc.RateLimitFlow
import spinalextras.lib.soc.{DeviceTree, DeviceTreeProvider}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class FlowLogger(val datas: Seq[(Data, ClockDomain)], val logBits: Int = 95, val gtimeTimeout : BigInt = 0xFFFFFFFFL, val stageAllFlows : Boolean = true) extends ComponentWithFormalProperties {
  val signature = datas.map(_.toString()).hashCode().abs
  val io = new Bundle {
    val flows = datas.map(b => slave Flow (Bits(b._1.getBitsWidth bits)))

    val log = master(Stream(Bits(logBits bits)))

    val inactive_channels = in(Bits(datas.length bits)).addTag(crossClockDomain) default(0)
    val manual_trigger = slave Flow (UInt(datas.length bits))

    val dropped_events = out(UInt(32 bits))
    val flush_dropped = in(Bool()) default(False)

    val captured_events = out(UInt(32 bits))
    val sysclk = out(UInt(64 bits))

    val flowFires = datas.zipWithIndex.map(x => (out Bool()).setPartialName(s"flowFires${x._2}_${x._1._1.getName}"))
  }
  SimPublic(io.dropped_events)
  SimPublic(io.captured_events)

  def flows(): Seq[(Flow[Bits], Int)] = io.flows.zipWithIndex

  val index_size: Int = log2Up(flows().map(_._2).max + 1 + 1)

  datas.zip(io.flows).zipWithIndex.foreach(x => {
    val (((inp, cd), port), idx) = x
    if (inp.name != null)
      port.setName(inp.getName())
    println(s"${port} ${idx} has ${port.payload.getWidth} bits ${port.payload.getWidth + index_size} total, ${logBits - port.payload.getWidth - index_size - 1} time bits")
  })

  def assign(signals: Seq[Flow[Bits]]): this.type = {
    signals.zip(io.flows).zipWithIndex.foreach(x => {
      val ((inp, port), idx) = x
      inp <> port
    })

    this
  }

  val syscnt = CounterFreeRun(64 bits)
  io.sysclk := syscnt

  io.captured_events.setAsReg() init (0)
  when(io.log.fire) {
      io.captured_events := io.captured_events + 1
  }

  val dropped_events = Reg(UInt(32 bits)) init (0)
  io.dropped_events := dropped_events
  when(io.flush_dropped) {
    dropped_events := 0
  }

  val time_since_syscnt = Timeout(gtimeTimeout)

  val metadata_stream, syscnt_stream = Stream(Bits(logBits bits))
  val needs_syscnt = RegInit(False)
  needs_syscnt := needs_syscnt | time_since_syscnt

  val meta_id_width = 8
  val syscnt_padding = logBits - index_size - 64
  syscnt_stream.payload := (syscnt.value ## B(0, meta_id_width bits) ## ~B(0, index_size bits)).resize(logBits)
  syscnt_stream.valid := needs_syscnt // We combine with ready here so we can change payload when needs_syscnt is true
  syscnt_stream.addFormalPayloadInvarianceException()

  val eventDropped = CombInit(False)
  when(eventDropped) {
    dropped_events := dropped_events + 1
  }

  metadata_stream.payload := (dropped_events ## U(signature, 32 bits) ## U(datas.size, 10 bits) ## B(1, meta_id_width bits) ## ~B(0, index_size bits)).resize(logBits)
  metadata_stream.valid := RegInit(False) setWhen(time_since_syscnt || eventDropped) clearWhen(metadata_stream.fire)
  metadata_stream.addFormalPayloadInvarianceException()

  val encoded_streams =
    for ((flow, idx) <- flows()) yield {

      val isSameCD = datas(idx)._2 == ClockDomain.current
      val data_log_capture = FlowLoggerDataCapture(this, Bits(flow.payload.getBitsWidth bits), datas(idx), idx)

      data_log_capture.setName(s"data_tap_${flow.name}_${idx}")
      data_log_capture.io.flow <> (if(stageAllFlows && isSameCD) flow.stage() else flow)
      data_log_capture.io.flow_fire <> io.flowFires(idx)
      data_log_capture.io.manual_trigger := io.manual_trigger.fire && io.manual_trigger.payload(idx) === True
      data_log_capture.io.channel_active := ~(io.inactive_channels(idx))

      when(data_log_capture.io.flow_fire && (time_since_syscnt.counter.value >> data_log_capture.time_bits) =/= 0) {
        needs_syscnt := True
      }

      data_log_capture.io.syscnt := syscnt.value
      when(data_log_capture.io.overflow) {
        eventDropped := True
      }
      data_log_capture.io.stamped_stream
    }


  when(syscnt_stream.fire) {
    needs_syscnt := False
    time_since_syscnt.clear()
  }

  io.log <> StreamArbiterFactory.lowerFirst.noLock.on(
    Seq(
      syscnt_stream.m2sPipe().s2mPipe(),
    ) ++ encoded_streams ++
      Seq(metadata_stream)
  ).addFormalPayloadInvarianceException()

  def getTypeName(d: Data): String = {
    d match {
      case b: Bundle => (if(b.getTypeString.contains("$") || b.getTypeString.isEmpty) d.getName() else b.getTypeString)  + "_" + d.getBitsWidth.toString
      case _ => d.getName()  + "_" + d.getBitsWidth.toString
    }
  }

  def getTypeName(d_clk: (Data, ClockDomain)): String = {
    val (d, cd) = d_clk
    getTypeName(d)
  }

  var comments = new ArrayBuffer[String]()
  def add_comments(comments: ArrayBuffer[String]) = {
    this.comments ++= comments
  }

  def create_logger_port(sysBus: BusSlaveProvider, address: BigInt, depth: Int,
                         outputStream : Option[Stream[Bits]] = None) = new Composite(this, "logger_port") {
    //val loggerFifo = StreamFifo(cloneOf(io.log.payload), depth)

    val loggerFifo = new MemoryBackedFifo(cloneOf(io.log.payload), depth, memory_label = "logger_buffer")
    loggerFifo.setName(s"loggerFifo_${depth}")
    loggerFifo.io.push <> io.log.stage()

    val stream = loggerFifo.io.pop.s2mPipe().m2sPipe()

    val checksum = RegInit(B(0, 32 bits))

    when(stream.fire) {
      checksum := (checksum |<< 1) ^ ((stream.payload << 1) | 1).subdivideIn(32 bits).
        fold(B(0, 32 bit))((a, b) => a ^ b.resize(32 bits))
    }

    if(sysBus == null) {
      stream >> outputStream.get
      loggerFifo.io.flush := False
      io.manual_trigger.clearAll()
    }

    val logger_port = sysBus.add_slave_factory("logger_port", SizeMapping(address, 1 KiB), true, true, "dBus", "data")

    val ctrlReg = logger_port.createReadAndWrite(UInt(32 bits), address + 0) init(0)
    val inMemory = RegNext(ctrlReg(0)) init(False)

    logger_port.createReadOnly(UInt(32 bits), address + 4) := RegNext(io.captured_events, init = U(0))
    val memoryStream = stream.clone()
    logger_port.readStreamNonBlocking(memoryStream, address + 8)
    if(outputStream.isEmpty) {
      memoryStream << stream
    } else {
      val demux = StreamDemux(stream, inMemory.asUInt, 2)
      demux(0) >> outputStream.get
      demux(1) >> memoryStream
    }

    logger_port.createReadOnly(Bits(32 bits), address + 20) := checksum
    logger_port.createReadOnly(Bits(32 bits), address + 24) := io.sysclk.resize(32).asBits
    logger_port.createReadOnly(UInt(32 bits), address + 28) := RegNext(loggerFifo.io.occupancy, init = U(0)).resized

    loggerFifo.io.flush := RegNext(logger_port.isWriting(address + 28))

    val manual_trigger = io.manual_trigger.clone()
    logger_port.driveFlow(manual_trigger, address + 32)
    logger_port.createReadOnly(Bits(32 bits), address + 32) := B(datas.size, 32 bits).resized

    io.manual_trigger <> manual_trigger.stage()

    val inactive_channels = Reg(io.inactive_channels.clone()) init(0)
    io.inactive_channels := inactive_channels
    logger_port.readAndWriteMultiWord(inactive_channels, address + 36)

    logger_port.createReadOnly(Bits(32 bits), address + 48) := signature
    logger_port.createReadOnly(UInt(32 bits), address + 52) := RegNext(io.dropped_events, init=U(0))
    io.flush_dropped := RegNext(logger_port.isWriting(address + 52)) init(False)

    io.flowFires.zipWithIndex.foreach(x => {
      val flowCnt = RegInit(U(0, 32 bits))
      logger_port.createReadOnly(UInt(32 bits), address + 56 + x._2 * 4) := flowCnt
      when(x._1) {
        flowCnt := flowCnt + 1
      }
    })

    new DeviceTreeProvider(address) {
      override def compatible : Seq[String] = Seq(s"spinex,event-logger")
      override def baseEntryPath = Seq("/", f"eventLogger@${address.toString(16)}")

      override def appendDeviceTree(dt: DeviceTree): Unit = {
        super.appendDeviceTree(dt)
      }
    }
  }
}
class FlowLoggerPortTestBench() extends ComponentWithFormalProperties {
  val sysBus = WishboneGlobalBus(WishboneConfig(32, 32, addressGranularity = AddressGranularity.BYTE))

  val io = new Bundle {
    val flows = Array.fill(2)(slave(Flow(Bits(3 bits))))
    val bus = slave(new Wishbone(sysBus.config))
    val log = master(Stream(Bits(95 bits)))
  }

  io.bus <> sysBus.add_master("cpu")
  val logger = new FlowLogger(io.flows.map(x => (x.payload, ClockDomain.current)), gtimeTimeout = 5)

  logger.io.flows.zip(io.flows).foreach(x => x._1 <> x._2)


  val portArea = logger.create_logger_port(sysBus, 0, 3, outputStream = Some(io.log))

  override def covers(): Seq[FormalProperty] = Seq(portArea.loggerFifo.io.availability === 0)
}

class FlowLoggerTestBench(inactiveChannels : Boolean = false) extends ComponentWithFormalProperties {
  val io = new Bundle {
    val flows = Array.fill(2)(slave(Flow(Bits(3 bits))))
    val inactive_channels = !inactiveChannels generate in(Bits(2 bits))
    val log = master(Stream(Bits(95 bits)))
  }

  val logger = new FlowLogger(io.flows.map(x => (x.payload, ClockDomain.current)), gtimeTimeout = 5)
  logger.io.manual_trigger.setIdle()

  if(!inactiveChannels) {
    logger.io.inactive_channels <> io.inactive_channels
  } else {
    logger.io.inactive_channels.setAll()
  }

  logger.io.flows.zip(io.flows).foreach(x => x._1 <> x._2)
  logger.io.log <> io.log

  override def covers(): Seq[FormalProperty] = new FormalProperties(this) {
    addFormalProperty(io.log.fire && io.log.payload.resize(10 bits) === 3)
    if(!inactiveChannels) {
      addFormalProperty(io.log.fire && io.log.payload.resize(2 bits) === 1)
      addFormalProperty(io.log.fire && io.log.payload.resize(2 bits) === 0)
    }
  }

  override def formalComponentProperties(): Seq[FormalProperty] = new FormalProperties(this) {
    if(inactiveChannels) {
      when(io.log.fire) {
        addFormalProperty(io.log.payload.resize(2 bits) === 0x3, "Check inactive logic")
      }
    }
  }
}

class FlowLoggerFormalTest extends AnyFunSuite with FormalTestSuite {
  formalTests().foreach(t => test(t._1) { t._2() })

  override def defaultDepth(): Int = 10
  override def CoverConfig() = formalConfig.withCover(20)
  override def ProveConfig() = formalConfig.withProve(5)
  override def BMCConfig() = formalConfig.withBMC(20)

  override def generateRtl() = Seq(
    ("Basic", () => GeneralFormalDut ( () => new FlowLoggerTestBench())),
  )

  override def generateRtlBMC(): Seq[(String, () => Module)] =
    super.generateRtlBMC() ++
      Seq(
        ("Inactive", () => GeneralFormalDut ( () => new FlowLoggerTestBench(true))),
        ("Port", () => GeneralFormalDut ( () => new FlowLoggerPortTestBench())),
      )
}

object FlowLogger {
  def apply(signals: Seq[(Data, Flow[Bits], ClockDomain)]*): FlowLogger = {
    new FlowLogger(signals.flatten.map(x => (x._1, x._3))).assign(signals.flatten.map(_._2))
  }
  def apply(signals: Seq[(Data, Flow[Bits])]*)(implicit ev: DummyImplicit): FlowLogger = {
    new FlowLogger(signals.flatten.map(x => (x._1, ClockDomain.current))).assign(signals.flatten.map(_._2))
  }

  def asFlow[T <: Data](s: Stream[T], stage : Boolean = true): (Data, Flow[Bits]) = {
    var flow = s.toFlowFire
    if(stage) flow = flow.stage()
    (s.payload, flow.setName(s.getName()).map(FlowLogger.asBits).setName(s.getName()))
  }

  def asFlow[T <: Data](s: Flow[T]): (Data, Flow[Bits]) = {
    (s.payload.setName(s.getName()), s.setName(s.getName()).map(FlowLogger.asBits).setName(s.getName()))
  }
  def asFlow(reg : RegInst): (Data, Flow[Bits])  = {
    val map = new HardMap().setName(reg.name)
    reg.getFields.foreach(f => {
      map.add(NamedType(f.hardbit).setName(f.name.replace("--", "reserved")))
    })
    val flow = Flow(reg.readBits.clone())
    flow.valid := RegNext(reg.readBits, init=U(0)) =/= reg.readBits
    flow.payload := reg.readBits
    (map, flow)
  }

  def profile_signal(name : String, evt : Bool, duration : TimeNumber = 1000 ms) = {
    val timeout = Timeout(duration)
    val counter = Reg(UInt(64 bit))
    val eventsPerDuration = Flow(cloneOf(counter))
    val arm = RegInit(False)

    eventsPerDuration.payload := counter
    eventsPerDuration.valid := timeout && arm

    when(evt) {
      counter := counter + 1
      arm := True
    }

    when(eventsPerDuration.fire && counter === 0) {
      arm := False
    }

    when(timeout) {
      timeout.clear()
      counter := 0
    }

    FlowLogger.flows(eventsPerDuration.setName(name))
  }

  def flows[T <: Data](flows: Flow[T]*): Seq[(Data, Flow[Bits])] = {
    flows.map(x => FlowLogger.asFlow(x))
  }

  def streams[T <: Data](flows: Stream[T]*): Seq[(Data, Flow[Bits])] = {
    flows.map(x => FlowLogger.asFlow(x))
  }

  def regs(regs: RegInst*): Seq[(Data, Flow[Bits])] = {
    regs.map(x => FlowLogger.asFlow(x))
  }
  def regs(timeNumber: TimeNumber, rs: RegInst*): Seq[(Data, Flow[Bits])] = {
    regs(rs:_*).map(x => (x._1, RateLimitFlow(timeNumber, x._2)))
  }

  def asBits[T <: Data](t: T): Bits = {
    t match {
      case b: Bundle => asBundleBits(b)
      case _ => t.asBits
    }
  }

  def asBundleBits[T <: Bundle](t: T): Bits = {
    t.asBits
  }
}