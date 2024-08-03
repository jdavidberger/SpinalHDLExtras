package spinalextras.lib.misc

import spinal.core.sim.SimPublic
import spinal.core._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib._
import spinal.lib.bus.regif.RegInst
import spinalextras.lib.{Memories, MemoryRequirement}
import spinalextras.lib.tests.WishboneGlobalBus.GlobalBus_t

import java.io.PrintWriter
import scala.collection.mutable
import scala.language.postfixOps

class GlobalLogger {

  val signals = new mutable.ArrayBuffer[(Data, Flow[Bits], ClockDomain)]()

  var built = false
  var topComponent = {
    val topComponent = Component.toplevel
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
    assert(!built)
    signals.appendAll(s.map(x => (x._1, topify(x._2), ClockDomain.current)))
  }

  def build(sysBus: GlobalBus_t, address: BigInt, depth: Int, name : String): Unit = {
    if(built) {
      return
    }
    built = true;
    val ctx = Component.push(Component.toplevel)
    val logger = FlowLogger(this.signals)
    logger.setName(name)
    logger.codeDefinitions()
    logger.create_logger_port(sysBus, address, depth)
    ctx.restore()

  }

  def create_logger_port(sysBus: GlobalBus_t, address: BigInt, depth: Int, name : String): Unit = {
    sysBus.addPreBuildTask(() => build(sysBus, address, depth, name))
    Component.toplevel.addPrePopTask(() => {
      this.build(sysBus, address, depth, name)
    })
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
  def create_logger_port(sysBus: GlobalBus_t, address: BigInt, depth: Int, name : String = Component.toplevel.name + "Logger"): Unit = {
    get().create_logger_port(sysBus, address, depth, name = name)
  }
}

class FlowLogger(datas: Seq[(Data, ClockDomain)], logBits: Int = 95) extends Component {
  val signature = datas.map(_.toString()).hashCode().abs
  val io = new Bundle {
    val flows = datas.map(b => slave Flow (Bits(b._1.getBitsWidth bits)))

    val log = master(Stream(Bits(logBits bits)))

    val inactive_channels = in(Bits(datas.length bits)).addTag(crossClockDomain)
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
    val (((inp, cd), port), idx) = x
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
        var manual_trigger =  io.manual_trigger.fire && io.manual_trigger.payload(idx) === True
        if(ClockDomain.current != datas(idx)._2) {
          new ClockingArea(datas(idx)._2) {
            manual_trigger = BufferCC(manual_trigger)
          }
        }
        r.valid := (!io.inactive_channels(idx) && stream.valid) || manual_trigger
        if(ClockDomain.current == datas(idx)._2) {
          r.stage().toStream
        } else {
          r.toStream.queue(4, pushClock = datas(idx)._2, popClock = ClockDomain.current)
        }
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
      stamped_stream.ready := True
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
    def getTypeName(d_clk: (Data, ClockDomain)): String = {
      val (d, cd) = d_clk
      d match {
        case b: Bundle => if(b.getTypeString.contains("$")) d.getName() else b.getTypeString
        case _ => d.getName()
      }
    }

    def getCType(data: Data): String = {
      val sizes = Seq(8, 16, 32, 64, 128)
      val pow2 = sizes.filter(_ >= (1 << (log2Up(data.getBitsWidth)))).head
      data match {
        case u : UInt => s"uint${pow2}_t"
        case s : SInt => s"int${pow2}_t"
        case b : Bool => "bool"
        case b : Bits => s"uint${pow2}_t"
        case _ => assert(false); ""
      }
    }

    val file = new PrintWriter(s"${getName()}.h")
    def emit(s : String): Unit = {
      file.write(s)
      file.write("\n");
      file.flush()
      //println(s)
    }

    emit(s"""
               |#define ${this.name}_INDEX_BITS ${index_size}
               |#define ${this.name}_SIGNATURE 0x${signature.toHexString}
               |typedef struct ${getName()}_info_t {
               |   uint32_t dropped_events;
               |   uint32_t captured_events;
               |   uint32_t checksum;
               |   uint32_t sysclk_lsb;
               |   uint32_t fifo_occupancy;
               |   uint32_t inactive_mask;
               |   uint32_t signature;
               |} ${getName()}_info_t;
               |
               |static ${getName()}_info_t ${getName()}_info_get(volatile uint32_t* base) {
               |  return (${getName()}_info_t) {
               |    .dropped_events = base[0],
               |    .captured_events = base[1],
               |    .checksum = base[5],
               |    .sysclk_lsb = base[6],
               |    .fifo_occupancy = base[7],
               |    .inactive_mask = base[9],
               |    .signature = base[10]
               |  };
               |}
               |
               |typedef struct ${getName()}_transaction {
               |  uint32_t l[3];
               |} ${getName()}_transaction;
               |
               |void ${getName()}_handle(const struct ${getName()}_transaction* tx);
               |static bool ${getName()}_poll(volatile uint32_t* ip_location) {
               |  struct ${getName()}_transaction tx = {0};
               |  tx.l[0] = ip_location[2 + 0];
               |  if((tx.l[0] & 1) == 1) {
               |      tx.l[1] = ip_location[2 + 1];
               |      tx.l[2] = ip_location[2 + 2];
               |
               |      //SHELL_OR_LOG(0, "Data %08x %08x %08x", tx.l[2], tx.l[1], tx.l[0]);
               |
               |    ${getName()}_handle(&tx);
               |    return true;
               |  }
               |  return false;
               |}
               |
               |static uint64_t shift(uint64_t d, int16_t shift) {
               |  if(shift >= 0) {
               |    return d >> shift;
               |  }
               |  return d << (-shift);
               |}
               |static uint64_t ${getName()}_parse_field(const struct ${getName()}_transaction* tx, int8_t bit_offset, int8_t bit_width) {
               |    if(bit_width == 0) {
               |        return 0;
               |    }
               |  uint64_t l0 = tx->l[0];
               |  uint64_t l1 = tx->l[1];
               |  uint64_t l2 = tx->l[2];
               |  uint64_t mask = bit_width == 64 ? 0xffffffffffffffffll : ((1ll << bit_width) - 1);
               |  return (shift(l0, bit_offset) |
               |     shift(l1, bit_offset-32) |
               |     shift(l2, bit_offset-64)) & mask;
               |}
               |
               |
               |uint64_t ${getName}_full_time(const struct ${getName()}_transaction* tx, uint64_t gtime, uint8_t time_bit_width) {
               |  uint64_t time_part = ${getName()}_parse_field(tx, 96 - time_bit_width, 64);
               |  if(time_bit_width >= 64) {
               |      return time_part;
               |  }
               |  uint64_t mask_bits = time_bit_width;
               |  uint64_t next_incr = (1LL << mask_bits);
               |  uint64_t mask = next_incr - 1;
               |  uint64_t time_reconstruction = (gtime & ~mask) | time_part;
               |    //SHELL_OR_LOG(g_shell, "T %d %llx %llx %llx %llx", time_bit_width, mask, (gtime & ~mask), gtime, time_part);
               |  if(time_reconstruction < gtime) {
               |      return time_reconstruction + next_incr;
               |  }
               |  return time_reconstruction;
               |}
               |
               |""".stripMargin)

    val defined = new mutable.HashMap[String, mutable.ArrayBuffer[(Flow[Bits], Int)]]()
    for ((flow, idx) <- flows()) {
      val key = getTypeName(datas(idx))
      emit(s"#define ${flow.getName().toUpperCase}_ID ${idx}")
      defined.getOrElseUpdate(key, new mutable.ArrayBuffer[(Flow[Bits], Int)]) += ((flow, idx))
    }

    emit(s"#define ${getName().toUpperCase}_FULL_TIME_ID 0x${((1 << index_size) - 1).toHexString}")

    for(key <- defined.keys) {
      val exemplar = datas(defined(key).head._2)._1
      exemplar match {
        case b : MultiData => {
          emit(s"typedef struct ${getName()}_${key}_t {")
          b.elements.foreach(x => {
            val prefix = s"${x._1}"
            emit(s"\t${getCType(x._2)} ${prefix};")
          })
          emit(s"} ${getName()}_${key}_t;")

          emit(s"#define ${getName()}_${key}_FIELDS(HANDLE_FIELD) \\")
          b.elements.foreach(x => {
            val prefix = s"${x._1}"
            emit(s"\tHANDLE_FIELD(${prefix}) \\")
          })
          emit("")

        }
        case _ => {
          emit(s"typedef ${getCType(exemplar)} ${getName()}_${key}_t;")
        }
      }

//      if(defined.size != 1 || defined(key).head._1.getName() != key) {
//        emit(s"""|${getName()}_${key}_t ${getName()}_parse_${key}(const ${getName()}_transaction* tx, int id) {
//                    |     switch(id) {""".stripMargin)
//        for((flow, idx) <- defined(key)) {
//          emit(s"         case ${flow.getName().toUpperCase}_ID: return ${getName()}_parse_${flow.getName()}(tx);")
//        }
//        emit("     }")
//        emit(s"     return (${getName()}_${key}_t){ 0 };")
//        emit("}")
//      }
    }

    emit(s"const char* ${getName()}_get_id_name(int id) {")
    emit(s"   switch(id) {")
    for ((flow, idx) <- flows()) {
      emit(s"   case ${flow.getName().toUpperCase}_ID: return ${'"' + flow.getName() + '"'};")
    }
    emit("    }")
    emit("    return \"UNKNOWN\";")
    emit("}")

    datas.foreach(d_clk => {
      val (d, cd) = d_clk
      var bitOffset = 0
      val time_bits = logBits - d.getBitsWidth - index_size
      emit(s"#define ${d.getName()}_TIME_BIT_WIDTH ${time_bits}")
        d match {
          case b: MultiData => {
            var parent_name = if(d.parent != null) d.parent.name else s"${d.name}"
            b.elements.foreach(x => {
              val prefix = s"${parent_name}_${x._1}"
              emit(s"#define ${s"${prefix}_BIT_OFFSET".padTo(32, ' ')} ${bitOffset}")
              emit(s"#define ${s"${prefix}_BIT_WIDTH".padTo(32, ' ')} ${x._2.getBitsWidth}")
              bitOffset += x._2.getBitsWidth
            })
//              emit(s"struct ${b.parent.name}_t {")
//              b.elements.foreach(x => {
//                val prefix = s"${x._1}"
//                emit(s"\t${getCType(x._2)} ${prefix};")
//                bitOffset += x._2.getBitsWidth
//              })
//              emit(s"}")
            emit(s"${getName()}_${getTypeName(d_clk)}_t ${getName()}_parse_${parent_name}(const ${getName()}_transaction* tx) {")
            emit(s"\treturn (${getName()}_${getTypeName(d_clk)}_t) {")
            b.elements.foreach(x => {
              val prefix = s"${parent_name}_${x._1}"
              emit(s"\t\t.${x._1} = ${getName()}_parse_field(tx, ${prefix}_BIT_OFFSET + ${this.name}_INDEX_BITS + 1 /* VALID bit */, ${prefix}_BIT_WIDTH),")
            })
            emit("\t};")
            emit("}")

          }
          case _ => {
            val time_bits = logBits - d.getBitsWidth - index_size
            val prefix = s"${d.getName()}"
            emit(s"#define ${prefix}_TIME_BIT_WIDTH ${time_bits}")

            emit(s"#define ${s"${prefix}_BIT_OFFSET".padTo(32, ' ')} ${bitOffset}")
            emit(s"#define ${s"${prefix}_BIT_WIDTH".padTo(32, ' ')} ${d.getBitsWidth}")
            emit(s"${getCType(d)} ${getName()}_parse_${d.getName()}(const ${getName()}_transaction* tx) {")
            emit(s"\treturn ${getName()}_parse_field(tx, ${prefix}_BIT_OFFSET + ${this.name}_INDEX_BITS + 1 /* VALID bit */, ${prefix}_BIT_WIDTH);")
            emit("}")
          }
        }
    })

    emit(s"void ${getName()}_handle_transaction(uint8_t id, const struct ${getName()}_transaction* tx);")
    for (t <- datas.map(getTypeName).toSet[String]) {
      emit(s"void ${getName()}_handle_${t} (uint64_t time, uint8_t id, const ${getName()}_${t}_t pkt);")
    }
    emit(s"uint8_t ${getName()}_get_id(const struct ${getName()}_transaction* tx){ return ${getName()}_parse_field(tx, 1, ${index_size}); }")
    emit(s"void ${getName()}_handle(const struct ${getName()}_transaction* tx){")
    emit(s"   static uint64_t gtime = 0;")
    emit(s"   uint8_t id = ${getName()}_get_id(tx);")
    emit(s"   ${getName()}_handle_transaction(id, tx);")
    emit(s"   switch(id) {")
    for ((flow, idx) <- flows()) {
      emit(s"   case ${flow.getName().toUpperCase}_ID: {\n" +
        s"      gtime = ${getName}_full_time(tx, gtime, ${flow.getName()}_TIME_BIT_WIDTH); \n" +
        s"      ${getName()}_handle_${getTypeName(datas(idx))}(gtime, id, ${getName()}_parse_${flow.getName()}(tx));")
      emit(s"      break;\n    }")
    }
    emit(s"   case ${(1 << index_size) - 1}: gtime = ${getName()}_parse_field(tx, 1 + ${this.name}_INDEX_BITS, 64); break;")
    emit("   default: LOG_WRN(\"Unknown id %d\", id);")
    emit(s"  }")
    emit(s"}")
  }

  def create_logger_port(sysBus: GlobalBus_t, address: BigInt, depth: Int): Unit = {
    //val loggerFifo = StreamFifo(cloneOf(io.log.payload), depth)
    val loggerFifo = new MemoryBackedFifo(cloneOf(io.log.payload), depth)
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

    logger_port.createReadOnly(Bits(32 bits), address + 40) := signature
  }
}

object FlowLogger {
  def apply(signals: Seq[(Data, Flow[Bits], ClockDomain)]*): FlowLogger = {
    new FlowLogger(signals.flatten.map(x => (x._1, x._3))).assign(signals.flatten.map(_._2))
  }
  def apply(signals: Seq[(Data, Flow[Bits])]*)(implicit ev: DummyImplicit): FlowLogger = {
    new FlowLogger(signals.flatten.map(x => (x._1, ClockDomain.current))).assign(signals.flatten.map(_._2))
  }

  def asFlow[T <: Bundle](s: Stream[T]): (Data, Flow[Bits]) = {
    (s.payload, s.toFlowFire.setName(s.getName()).map(FlowLogger.asBits).setName(s.getName()))
  }

  def asFlow[T <: Data](s: Flow[T]): (Data, Flow[Bits]) = {
    (s.payload.setName(s.getName()), s.setName(s.getName()).map(FlowLogger.asBits).setName(s.getName()))
  }
  def asFlow(reg : RegInst): (Data, Flow[Bits])  = {
    val map = new HardMap().setName(reg.name)
    reg.getFields.foreach(f => {
      map.add(NamedType(f.hardbit).setName(f.name))
    })
    val flow = Flow(reg.readBits.clone())
    flow.valid := RegNext(reg.readBits) =/= reg.readBits
    flow.payload := reg.readBits
    (map, flow)
  }

  def flows[T <: Data](flows: Flow[T]*): Seq[(Data, Flow[Bits])] = {
    flows.map(x => FlowLogger.asFlow(x))
  }

  def regs(regs: RegInst*): Seq[(Data, Flow[Bits])] = {
    regs.map(x => FlowLogger.asFlow(x))
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