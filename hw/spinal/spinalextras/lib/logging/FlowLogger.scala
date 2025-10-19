package spinalextras.lib.logging

import spinal.core._
import spinal.core.sim.SimPublic
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.regif.RegInst
import spinalextras.lib.logging.FlowLoggerUtils.{FlowLoggerCCode, FlowLoggerSqlite, FlowLoggerYaml}
import spinalextras.lib.memory.MemoryBackedFifo
import spinalextras.lib.misc.RateLimitFlow
import spinalextras.lib.soc.{DeviceTree, DeviceTreeProvider}
import spinalextras.lib.tests.WishboneGlobalBus.GlobalBus_t

import java.io.PrintWriter
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class GlobalLogger {
  val signals = new mutable.ArrayBuffer[(Data, Flow[Bits], ClockDomain, Set[String])]()
  val comments = new ArrayBuffer[String]()
  var built = false
  var topComponent = {
    val topComponent = Component.toplevel
    topComponent
  }

  def topify(signal : Flow[Bits]) : Flow[Bits] = {
    if(Component.current == Component.toplevel) {
      return signal
    }
    var name = signal.name
    var intermediate_bus = signal
    var new_bus : Option[Flow[Bits]] = None

    var c = Component.current
    do {
      val ctx = Component.push(c)

      val next_bus = master(signal.clone()).setName(s"logger_${name}")

      if(new_bus.isEmpty) {
        new_bus = Some(next_bus)
      }

      intermediate_bus <> next_bus

      intermediate_bus = next_bus
      ctx.restore()
      c = c.parent
    } while(c != Component.toplevel && c != null)
    assert(c != null)

    intermediate_bus
  }
  def add(tags: Set[String], s: (Data, Flow[Bits])*): Unit = {
    if(built) {
      println(s"Warning: ${s} with tags ${tags} was added after the logger was created")
      return
    }
    assert(ClockDomain.current != null)
    for (elem <- s) {
      assert(elem._1.name != null && elem._1.name.nonEmpty)
      //assert(elem._2.name != null && elem._2.name.size > 0)
    }
    signals.appendAll(s.map(x => (x._1, topify(x._2), ClockDomain.current, tags)))
  }

  var output_path : String = null
  def set_output_path(fn : String): Unit = {
    output_path = fn
  }

  def build(sysBus: GlobalBus_t, address: BigInt, depth: Int, name : String,
            outputStream : Option[Stream[Bits]] = None,
            tags : Set[String] = Set()): Unit = {
    val clockDomain = outputStream.map(_.valid.clockDomain).getOrElse(ClockDomain.current)

    if(built) {
      return
    }
    if(output_path == null) {
      output_path = spinal.core.GlobalData.get.config.targetDirectory
    }
    val signals = this.signals.filter(s => {
      s._4.intersect(tags).nonEmpty || tags.isEmpty
    }).map(s => (s._1, s._2, s._3))

    built = true;
    val loggerName = name
    new ClockingArea(clockDomain) {
      if (signals.nonEmpty) {
        val ctx = Component.push(Component.toplevel)
        val logger = FlowLogger(signals)
        logger.setName(loggerName)
        logger.add_comments(comments)
        FlowLoggerCCode(logger, output_path)
        FlowLoggerSqlite(logger, output_path)
        FlowLoggerYaml(logger, output_path)
        logger.create_logger_port(sysBus, address, depth, outputStream)
        ctx.restore()
      } else {
        outputStream.foreach(_.setIdle())
      }
    }
    output_path = null
  }
  def create_logger_stream(depth: Int, outputStream : Stream[Bits], tags: Set[String] = Set()): Unit = {
    Component.toplevel.addPrePopTask(() => {
      val sysBus : GlobalBus_t = null
      this.build(sysBus, 0, depth, "GlobalLogger", Some(outputStream), tags = tags)
    })
  }
  def create_logger_port(sysBus: GlobalBus_t, address: BigInt, depth: Int, name : String, outputStream : Option[Stream[Bits]] = None, tags : Set[String] = Set()): Unit = {
    sysBus.addPreBuildTask(() => build(sysBus, address, depth, name, outputStream, tags))
    Component.toplevel.addPrePopTask(() => {
      this.build(sysBus, address, depth, name, outputStream, tags)
    })
  }



  def add_comment(str: String) = {
    comments += str
  }
}

object GlobalLogger {
  var sysLogger : Option[GlobalLogger] = None

  def get(): GlobalLogger = {
    if (sysLogger.isEmpty || sysLogger.get.topComponent != Component.toplevel) {
      sysLogger = Some(new GlobalLogger())
    }
    sysLogger.get
  }

  def add_comment(str: String): Unit = {
    get().add_comment(str)
  }

  def apply(tags: Set[String], signals: Seq[(Data, Flow[Bits])]*): Unit = {
    signals.foreach(x => get().add(tags = tags, x:_*))
  }

  def apply(signals: Seq[(Data, Flow[Bits])]*): Unit = {
    signals.foreach(x => get().add(tags = Set(), x:_*))
  }

  def set_output_path(fn : String): Unit = {
    get().set_output_path(fn)
  }
  def create_logger_stream(depth: Int, outputStream : Stream[Bits], tags: Set[String] = Set()) : Unit = {
    get().create_logger_stream(depth, outputStream = outputStream, tags = tags)
  }
  def create_logger_port(sysBus: GlobalBus_t, address: BigInt, depth: Int, name : String = Component.toplevel.name + "Logger",
                         outputStream : Option[Stream[Bits]] = None, tags: Set[String] = Set()): Unit = {
    get().create_logger_port(sysBus, address, depth, name = name, outputStream = outputStream, tags = tags)
  }
}

class FlowLoggerDataCapture(logger : FlowLogger, dataType : HardType[Bits], datum : (Data, ClockDomain), idx : Int) extends Component {
  val io = new Bundle {
    val flow = slave Flow(dataType)
    val manual_trigger = in Bool()
    val channel_active = in Bool()

    val flow_fire = out Bool()
    val needs_syscnt = out Bool()

    val time_since_syscnt = in(logger.time_since_syscnt.counter.value.clone())
    val syscnt = in(logger.syscnt.value.clone())

    val stamped_stream = master(logger.io.log.clone())
  }

  val output_stream = {
    val r = Flow(io.flow.payload.clone())
    r.payload := io.flow.payload
    var manual_trigger =  io.manual_trigger
    if(ClockDomain.current != datum._2) {
      new ClockingArea(datum._2) {
        manual_trigger = BufferCC(manual_trigger)
      }
    }
    r.valid := (io.channel_active && io.flow.valid) || manual_trigger
    if(ClockDomain.current == datum._2) {
      io.flow_fire := r.valid
      //r.stage().toStream
      r.toStream
    } else {
      val rCCed = r.toStream.queue(4, pushClock = datum._2, popClock = ClockDomain.current)
      rCCed.setName(s"${datum._1.name}_fifo_cc")
      io.flow_fire := rCCed.fire
      rCCed
    }
  }
  val logBits = logger.logBits
  val index_size = logger.index_size

  val time_bits = logBits - output_stream.payload.getBitsWidth - index_size
  assert(time_bits > 0, s"${io.flow} has too many bits for logger ${logBits} ${output_stream.payload.getBitsWidth} ${index_size}")

//  if (minimum_time_bits > time_bits) {
//    minimum_time_bits = time_bits
//  }
//  when(output_stream.fire) {
//    report(Seq("Log fire", datum._1.name, output_stream.payload))
//  }

  io.needs_syscnt := False
  when(output_stream.fire && (io.time_since_syscnt >> time_bits) =/= 0) {
    io.needs_syscnt := True
  }

  val stamped_stream = output_stream.map(p => io.syscnt.resize(time_bits bits) ## p ## B(idx, index_size bits))

//  when(stamped_stream.fire) {
//    report(Seq(io.flow.name, io.flow.payload))
//  }

  stamped_stream.stage().s2mPipe() <> io.stamped_stream
}

class ArbiterNode[T <: Data](dataType: HardType[T], cnt : Int) extends Component {
  val io = new Bundle {
    val ins = Array.fill(cnt)(slave Stream(dataType))
    val out = master Stream(dataType)
  }

  io.out <> {
    cnt match {
      case 1 => io.ins.head
      case 2 => {
        val out = io.out.clone()
        io.ins.foreach(_.setBlocked())
        when(io.ins.head.valid) {
          io.ins.head <> out
        } otherwise  {
          io.ins.last <> out
        }
        out
      }
      case n => {
        ArbiterNode(Seq(ArbiterNode(io.ins.drop(1).toSeq), io.ins.head))
      }
    }
  }
}

object ArbiterNode {
  def apply[T <: Data](streams : Seq[Stream[T]]): Stream[T] = {
    streams.size match {
      case 1 => streams(0)
      case n => {
        val tree = new ArbiterNode[T](streams.head.payload, n)
        tree.io.ins.zip(streams).foreach(x => x._1 <> x._2)
        tree.io.out
      }
    }
  }
}

class StreamArbiterTree[T <: Data](dataType: HardType[T], cnt : Int) extends Component {
  val io = new Bundle {
    val ins = Array.fill(cnt)(slave Stream(dataType))
    val out = master Stream(dataType)
  }

  io.out <> {
    cnt match {
      case 1 => io.ins.head
      case 2 => StreamArbiterFactory.lowerFirst.noLock.on(io.ins)
      case n => {
        val (a, b) = io.ins.splitAt(n / 2)
        StreamArbiterTree(Seq(StreamArbiterTree(a), StreamArbiterTree(b)))
      }
    }
  }
}

object StreamArbiterTree {
  def apply[T <: Data](streams : Seq[Stream[T]]): Stream[T] = {
    streams.size match {
      case 1 => streams(0)
      case n => {
        val tree = new StreamArbiterTree[T](streams.head.payload, n)
        tree.io.ins.zip(streams).foreach(x => x._1 <> x._2)
        tree.io.out
      }
    }
  }
}

class FlowLogger(val datas: Seq[(Data, ClockDomain)], val logBits: Int = 95) extends Component {
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

  val time_since_syscnt = Timeout(0xffffffffL)

  val syscnt_stream = Stream(Bits(logBits bits))
  val needs_syscnt = RegInit(False)
  needs_syscnt := needs_syscnt | time_since_syscnt

  val meta_id_width = 8
  val syscnt_padding = logBits - index_size - 64
  syscnt_stream.payload := (syscnt ## B(0, meta_id_width bits) ## ~B(0, index_size bits)).resized
  syscnt_stream.valid := syscnt_stream.ready & needs_syscnt // We combine with ready here so we can change payload when needs_syscnt is true

  val metadata_stream = Stream(Bits(logBits bits))
  metadata_stream.payload := (U(signature, 32 bits) ## U(datas.size, 10 bits) ## B(1, meta_id_width bits) ## ~B(0, index_size bits)).resized
  metadata_stream.valid := RegInit(False) setWhen(time_since_syscnt) clearWhen(metadata_stream.fire)

  var minimum_time_bits = logBits
  val encoded_streams = {
    for ((stream, idx) <- flows()) yield {

      val data_log_capture = new FlowLoggerDataCapture(this, Bits(stream.payload.getBitsWidth bits), datas(idx), idx)
      data_log_capture.setName(s"data_tap_${stream.name}_${idx}")
      data_log_capture.io.flow <> stream
      data_log_capture.io.flow_fire <> io.flowFires(idx)
      data_log_capture.io.manual_trigger := io.manual_trigger.fire && io.manual_trigger.payload(idx) === True
      data_log_capture.io.channel_active := ~(io.inactive_channels(idx))
      when(data_log_capture.io.needs_syscnt) {
        needs_syscnt := True
      }

      data_log_capture.io.time_since_syscnt := time_since_syscnt.counter.value
      data_log_capture.io.syscnt := syscnt.value
      when(data_log_capture.io.stamped_stream.isStall) {
        dropped_events := dropped_events + 1
      }
      data_log_capture.io.stamped_stream
    }
  }

  when(syscnt_stream.fire) {
    needs_syscnt := False
    time_since_syscnt.clear()
  }

  io.log <> StreamArbiterFactory.lowerFirst.noLock.on(encoded_streams ++ Seq(syscnt_stream.stage(), metadata_stream.stage())).stage()
  //io.log <> StreamArbiterTree(encoded_streams ++ Seq(syscnt_stream.stage())).stage()

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

  def create_logger_port(sysBus: GlobalBus_t, address: BigInt, depth: Int,
                         outputStream : Option[Stream[Bits]] = None): Unit = {
    //val loggerFifo = StreamFifo(cloneOf(io.log.payload), depth)
    val loggerFifo = new MemoryBackedFifo(cloneOf(io.log.payload), depth, memory_label = "logger_buffer")
    loggerFifo.setName(s"loggerFifo_${depth}")
    loggerFifo.io.push <> io.log

    val stream = loggerFifo.io.pop.stage()

    val checksum = RegInit(B(0, 32 bits))

    when(stream.fire) {
      checksum := (checksum |<< 1) ^ ((stream.payload << 1) | 1).subdivideIn(32 bits).
        fold(B(0, 32 bit))((a, b) => a ^ b.resize(32 bits))
    }

    if(sysBus == null) {
      stream >> outputStream.get
      loggerFifo.io.flush := False
      io.manual_trigger.clearAll()
      return
    }

    val logger_port = sysBus.add_slave_factory("logger_port", SizeMapping(address, 1 KiB), true, true, "cpu")

    val ctrlReg = logger_port.createReadAndWrite(UInt(32 bits), address + 0) init(0)
    val inMemory = ctrlReg(0)

    logger_port.createReadOnly(UInt(32 bits), address + 4) := RegNext(io.captured_events)
    val memoryStream = stream.clone()
    logger_port.readStreamNonBlocking(memoryStream, address + 8)
    if(outputStream.isEmpty) {
      memoryStream <> stream
    } else {
      val demux = StreamDemux(stream, inMemory.asUInt, 2)
      demux(0) <> outputStream.get
      demux(1) <> memoryStream
    }

    logger_port.createReadOnly(Bits(32 bits), address + 20) := checksum
    logger_port.createReadOnly(Bits(32 bits), address + 24) := io.sysclk.resize(32).asBits
    logger_port.createReadOnly(UInt(32 bits), address + 28) := RegNext(loggerFifo.io.occupancy).resized

    loggerFifo.io.flush := RegNext(logger_port.isWriting(address + 28))

    val manual_trigger = io.manual_trigger.clone()
    logger_port.driveFlow(manual_trigger, address + 32)
    logger_port.createReadOnly(Bits(32 bits), address + 32) := B(datas.size, 32 bits).resized

    io.manual_trigger <> manual_trigger.stage()

    val inactive_channels = Reg(io.inactive_channels.clone()) init(0)
    io.inactive_channels := inactive_channels
    logger_port.readAndWriteMultiWord(inactive_channels, address + 36)

    logger_port.createReadOnly(Bits(32 bits), address + 48) := signature
    logger_port.createReadOnly(UInt(32 bits), address + 52) := RegNext(io.dropped_events)

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
    }
  }


}

object FlowLogger {
  def apply(signals: Seq[(Data, Flow[Bits], ClockDomain)]*): FlowLogger = {
    new FlowLogger(signals.flatten.map(x => (x._1, x._3))).assign(signals.flatten.map(_._2))
  }
  def apply(signals: Seq[(Data, Flow[Bits])]*)(implicit ev: DummyImplicit): FlowLogger = {
    new FlowLogger(signals.flatten.map(x => (x._1, ClockDomain.current))).assign(signals.flatten.map(_._2))
  }

  def asFlow[T <: Data](s: Stream[T]): (Data, Flow[Bits]) = {
    (s.payload, s.toFlowFire.setName(s.getName()).map(FlowLogger.asBits).setName(s.getName()))
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
    flow.valid := RegNext(reg.readBits) =/= reg.readBits
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