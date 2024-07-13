package spinalextras.lib.misc

import spinal.core.sim.SimPublic
import spinal.core._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib._
import spinalextras.lib.{Memories, MemoryRequirement}
import spinalextras.lib.tests.WishboneGlobalBus.GlobalBus_t

import scala.collection.mutable
import scala.language.postfixOps

class GlobalLogger {

  val signals = new mutable.ArrayBuffer[(Data, Flow[Bits])]()

  var built = false
  var topComponent = {
    val topComponent = Component.toplevel

    Component.toplevel.addPrePopTask(() => {
        this.build()
    })
    topComponent
  }

  def topify(signal : Flow[Bits]) : Flow[Bits] = {
    if(Component.current == Component.toplevel) {
      return signal
    }

    var intermediate_bus = signal
    var new_bus : Option[Flow[Bits]] = None

    var c = Component.current
    do {
      val ctx = Component.push(c)

      val next_bus = master(signal.clone())

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

  def add(s: (Data, Flow[Bits])*): Unit = {
    signals.appendAll(s.map(x => (x._1, topify(x._2))))
  }

  def build(): Unit = {
    if(built) {
      return
    }
    built = true;
    if(logger_port.isEmpty) {
      println("No outputs defined for the global logger; not including it.")
    }
    logger_port.foreach(x => {
      val ctx = Component.push(Component.toplevel)
      val logger = FlowLogger(this.signals)
      logger.setName("GlobalLogger")
      logger.codeDefinitions()
      logger.create_logger_port(x._1, x._2, x._3)
      ctx.restore()
    })

  }

  var logger_port : Option[(GlobalBus_t, BigInt, Int)] = None
  def create_logger_port(sysBus: GlobalBus_t, address: BigInt, depth: Int): Unit = {
    logger_port = Some((sysBus, address, depth))
    sysBus.addPreBuildTask(() => build())
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

  def apply(signals: Seq[(Data, Flow[Bits])]*): Unit = {
    signals.foreach(x => get().add(x:_*))
  }
  def create_logger_port(sysBus: GlobalBus_t, address: BigInt, depth: Int): Unit = {
    get().create_logger_port(sysBus, address, depth)
  }
}

class FlowLogger(datas: Seq[Data], logBits: Int = 95) extends Component {
  val io = new Bundle {
    val flows = datas.map(b => slave Flow (Bits(b.getBitsWidth bits)))

    val log = master(Stream(Bits(logBits bits)))

    val inactive_channels = in(Bits(datas.length bits))
    val manual_trigger = slave Flow (UInt(datas.length bits))

    val dropped_events = out(UInt(32 bits))
    val captured_events = out(UInt(32 bits))
    val sysclk = out(UInt(64 bits))
  }
  SimPublic(io.dropped_events)
  SimPublic(io.captured_events)

  def flows(): Seq[(Flow[Bits], Int)] = io.flows.zipWithIndex

  val index_size: Int = log2Up(flows().map(_._2).max + 1 + 1)

  datas.zip(io.flows).zipWithIndex.foreach(x => {
    val ((inp, port), idx) = x
    if (inp.name != null)
      port.setName(inp.getName())
    println(s"${port} ${idx} has ${port.payload.getWidth} bits ${port.payload.getWidth + index_size} total")
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

  val time_since_syscnt = Timeout(0xffffffffL)

  val syscnt_stream = Stream(Bits(logBits bits))
  val needs_syscnt = RegInit(False)
  needs_syscnt := needs_syscnt | time_since_syscnt
  syscnt_stream.payload := (syscnt ## ~B(0, index_size bits)).resized
  syscnt_stream.valid := syscnt_stream.ready & needs_syscnt

  var minimum_time_bits = logBits
  val encoded_streams = {
    for ((stream, idx) <- flows()) yield {
      val output_stream = {
        val r = Flow(stream.payload.clone())
        r.payload := stream.payload
        r.valid := (!io.inactive_channels(idx) && stream.valid) || (io.manual_trigger.fire && io.manual_trigger.payload(idx) === True)
        r.stage()
      }
      val time_bits = logBits - output_stream.payload.getBitsWidth - index_size
      require(time_bits > 0)

      if (minimum_time_bits > time_bits) {
        minimum_time_bits = time_bits
      }

      when(output_stream.fire && (time_since_syscnt.counter.value >> time_bits) =/= 0) {
        needs_syscnt := True
      }

      val stamped_stream = output_stream.map(p => syscnt.resize(time_bits bits) ## p ## B(idx, index_size bits))
      val encoded_fifo = StreamFifo(stamped_stream.payload.clone(), 8)
      encoded_fifo.io.push.payload := stamped_stream.payload
      encoded_fifo.io.push.valid := stamped_stream.valid && encoded_fifo.io.push.ready
      when(stamped_stream.valid && !encoded_fifo.io.push.ready) {
        dropped_events := dropped_events + 1
      }
      encoded_fifo.io.pop
    }
  }

  when(syscnt_stream.fire) {
    needs_syscnt := False
    time_since_syscnt.clear()
  }

  io.log <> StreamArbiterFactory.lowerFirst.noLock.on(encoded_streams ++ Seq(syscnt_stream.stage()))

  def codeDefinitions(): Unit = {
    def getTypeName(d: Data): String = {
      d match {
        case b: Bundle => b.getTypeString
        case _ => d.getName()
      }
    }

    datas.foreach(d => {
      var bitOffset = 0
      if(d.parent != null) {
        d match {
          case b: Bundle => {
            b.elements.foreach(x => {
              val prefix = s"${b.parent.name}_${x._1}"
              println(s"#define ${s"${prefix}_BIT_OFFSET".padTo(32, ' ')} ${bitOffset}")
              println(s"#define ${s"${prefix}_BIT_WIDTH".padTo(32, ' ')} ${x._2.getBitsWidth}")
              bitOffset += x._2.getBitsWidth
            })
          }
          case _ => {
            val prefix = s"${d.getName()}"
            println(s"#define ${s"${prefix}_BIT_OFFSET".padTo(32, ' ')} ${bitOffset}")
            println(s"#define ${s"${prefix}_BIT_WIDTH".padTo(32, ' ')} ${d.getBitsWidth}")
          }
        }
      }
    })

    var defined = new mutable.HashSet[String]()
    println(s"#define ${this.name}_INDEX_BITS ${index_size}")
    for ((flow, idx) <- flows()) {
      val time_bits = logBits - flow.payload.getBitsWidth - index_size
      val key = getTypeName(datas(idx))
      if (!defined.contains(key)) {
        println(s"#define ${key.toUpperCase}_TIME_BIT_WIDTH ${time_bits}")
        defined.add(key)
      }
    }

    for ((flow, idx) <- flows()) {
      println(s"#define ${flow.getName().toUpperCase}_ID ${idx}")
    }

    for (t <- datas.map(getTypeName).toSet[String]) {
      println(s"void ${getName()}_handle_${t}(uint64_t time, uint8_t id, const struct axi_transaction* tx) {")
      println("}")
    }
    println(s"uint8_t ${getName()}_get_id(const struct axi_transaction* tx){ return axi_field(tx, 1, ${index_size}); }")
    println(s"void ${getName()}_handle(const struct axi_transaction* tx){")
    println(s"   static uint64_t gtime = 0;")
    println(s"   uint8_t id = ${getName()}_get_id(tx);")
    println(s"   switch(id) {")
    for ((flow, idx) <- flows()) {
      println(s"   case ${flow.getName().toUpperCase}_ID: {\n" +
        s"      gtime = logger_full_time(tx, gtime, ${getTypeName(datas(idx)).toUpperCase}_TIME_BIT_WIDTH); \n" +
        s"      ${getName()}_handle_${getTypeName(datas(idx))}(gtime, id, tx);")
      println(s"      break;\n    }")
    }
    println(s"   case ${(1 << index_size) - 1}: gtime = axi_field(tx, 1 + ${this.name}_INDEX_BITS, 64); break;")
    println("   default: LOG_WARN(\"Unknown id %d\", id);")
    println(s"  }")
    println(s"}")
  }

  def create_logger_port(sysBus: GlobalBus_t, address: BigInt, depth: Int): Unit = {
    //val loggerFifo = StreamFifo(cloneOf(io.log.payload), depth)
    val loggerFifo = MemoryFifo(cloneOf(io.log.payload), depth)
    loggerFifo.setName(s"loggerFifo_${depth}")
    loggerFifo.io.push <> io.log

    val stream = loggerFifo.io.pop.stage()
    val checksum = RegInit(B(0, 32 bits))

    when(stream.fire) {
      checksum := (checksum |<< 1) ^ ((stream.payload << 1) | 1).subdivideIn(32 bits).
        fold(B(0, 32 bit))((a, b) => a ^ b.resize(32 bits))
    }

    val logger_port = sysBus.add_slave_factory("logger_port", SizeMapping(address, 1 KiB), "cpu")
    logger_port.createReadOnly(UInt(32 bits), address) := RegNext(io.dropped_events)
    logger_port.createReadOnly(UInt(32 bits), address + 4) := RegNext(io.captured_events)
    logger_port.readStreamNonBlocking(stream, address + 8)
    logger_port.createReadOnly(Bits(32 bits), address + 20) := checksum
    logger_port.createReadOnly(Bits(32 bits), address + 24) := io.sysclk.resize(32).asBits
    logger_port.createReadOnly(UInt(32 bits), address + 28) := RegNext(loggerFifo.io.occupancy).resized

    loggerFifo.io.flush := False
    logger_port.onWrite(address + 28)(loggerFifo.io.flush := True)

    val manual_trigger = io.manual_trigger.clone()
    logger_port.driveFlow(manual_trigger, address + 32)
    io.manual_trigger <> manual_trigger.stage()
    io.inactive_channels := logger_port.createReadAndWrite(io.inactive_channels, address + 36) init (0)
  }
}

object FlowLogger {
  def apply(signals: Seq[(Data, Flow[Bits])]*): FlowLogger = {
    new FlowLogger(signals.flatten.map(_._1)).assign(signals.flatten.map(_._2))
  }

  def asFlow[T <: Bundle](s: Stream[T]): (Data, Flow[Bits]) = {
    (s.payload, s.toFlowFire.setName(s.getName()).map(FlowLogger.asBits).setName(s.getName()))
  }

  def asFlow[T <: Data](s: Flow[T]): (Data, Flow[Bits]) = {
    (s.payload.setName(s.getName()), s.setName(s.getName()).map(FlowLogger.asBits).setName(s.getName()))
  }

  def flows[T <: Data](flows: Flow[T]*): Seq[(Data, Flow[Bits])] = {
    flows.map(x => FlowLogger.asFlow(x))
  }

  def asBits[T <: Data](t: T): Bits = {
    t match {
      case b: Bundle => asBundleBits(b)
      case _ => t.asBits
    }
  }

  def asBundleBits[T <: Bundle](t: T): Bits = {
    var bitOffset = 1
    t.elements.foreach(x => {
      val prefix = s"${t.parent.name}_${x._1}"
      println(s"#define ${s"${prefix}_BIT_OFFSET".padTo(32, ' ')} ${bitOffset}")
      println(s"#define ${s"${prefix}_BIT_WIDTH".padTo(32, ' ')} ${x._2.getBitsWidth}")
      bitOffset += x._2.getBitsWidth
    })
    t.asBits
  }
}