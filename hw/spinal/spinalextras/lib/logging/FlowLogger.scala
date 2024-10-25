package spinalextras.lib.logging

import spinal.core._
import spinal.core.sim.SimPublic
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.regif.RegInst
import spinalextras.lib.memory.MemoryBackedFifo
import spinalextras.lib.misc.RateLimitFlow
import spinalextras.lib.tests.WishboneGlobalBus.GlobalBus_t

import java.io.PrintWriter
import scala.collection.mutable
import scala.language.postfixOps

class GlobalLogger {
  val signals = new mutable.ArrayBuffer[(Data, Flow[Bits], ClockDomain, Set[String])]()

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
    assert(!built)
    assert(ClockDomain.current != null)
    signals.appendAll(s.map(x => (x._1, topify(x._2), ClockDomain.current, tags)))
  }

  var output_path = "."
  def set_output_path(fn : String): Unit = {
    output_path = fn
  }

  def build(sysBus: GlobalBus_t, address: BigInt, depth: Int, name : String,
            outputStream : Option[Stream[Bits]] = None,
            tags : Set[String] = Set()): Unit = {
    if(built) {
      return
    }
    built = true;
    val signals = this.signals.filter(s => {
      s._4.intersect(tags).nonEmpty || tags.isEmpty
    }).map(s => (s._1, s._2, s._3))

    if(signals.nonEmpty) {
      val ctx = Component.push(Component.toplevel)
      val logger = FlowLogger(signals)
      logger.setName(name)
      logger.codeDefinitions(output_path)
      logger.sqliteHandlers(output_path)
      logger.create_logger_port(sysBus, address, depth, outputStream)
      ctx.restore()
    } else {
      outputStream.foreach(_.setIdle())
    }
  }
  def create_logger_stream(depth: Int, outputStream : Stream[Bits]): Unit = {
    Component.toplevel.addPrePopTask(() => {
      this.build(null, 0, depth, "GlobalLogger", Some(outputStream))
    })
  }
  def create_logger_port(sysBus: GlobalBus_t, address: BigInt, depth: Int, name : String, outputStream : Option[Stream[Bits]] = None, tags : Set[String] = Set()): Unit = {
    sysBus.addPreBuildTask(() => build(sysBus, address, depth, name, outputStream, tags))
    Component.toplevel.addPrePopTask(() => {
      this.build(sysBus, address, depth, name, outputStream, tags)
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

  def apply(tags: Set[String], signals: Seq[(Data, Flow[Bits])]*): Unit = {
    signals.foreach(x => get().add(tags = tags, x:_*))
  }

  def apply(signals: Seq[(Data, Flow[Bits])]*): Unit = {
    signals.foreach(x => get().add(tags = Set(), x:_*))
  }

  def set_output_path(fn : String): Unit = {
    get().set_output_path(fn)
  }
  def create_logger_stream(depth: Int, outputStream : Stream[Bits]) : Unit = {
    get().create_logger_stream(depth, outputStream = outputStream)
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
  when(output_stream.fire) {
    report(Seq("Log fire", datum._1.name, output_stream.payload))
  }

  io.needs_syscnt := False
  when(output_stream.fire && (io.time_since_syscnt >> time_bits) =/= 0) {
    io.needs_syscnt := True
  }

  val stamped_stream = output_stream.map(p => io.syscnt.resize(time_bits bits) ## p ## B(idx, index_size bits))

  when(stamped_stream.fire) {
    report(Seq(io.flow.name, io.flow.payload))
  }

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

class FlowLogger(datas: Seq[(Data, ClockDomain)], val logBits: Int = 95) extends Component {
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
  when(io.flush_dropped) {
    dropped_events := 0
  }

  val time_since_syscnt = Timeout(0xffffffffL)

  val syscnt_stream = Stream(Bits(logBits bits))
  val needs_syscnt = RegInit(False)
  needs_syscnt := needs_syscnt | time_since_syscnt
  val syscnt_padding = logBits - index_size - 64
  syscnt_stream.payload := (syscnt ## ~B(0, syscnt_padding bits)).resized
  syscnt_stream.valid := syscnt_stream.ready & needs_syscnt

  var minimum_time_bits = logBits
  val encoded_streams = {
    for ((stream, idx) <- flows()) yield {

      val data_log_capture = new FlowLoggerDataCapture(this, Bits(stream.payload.getBitsWidth bits), datas(idx), idx)
      data_log_capture.setName(s"data_tap_${stream.name}_${idx}")
      data_log_capture.io.flow <> stream
      data_log_capture.io.flow_fire <> io.flowFires(idx)
      data_log_capture.io.manual_trigger := io.manual_trigger.fire && io.manual_trigger.payload(idx) === True
      data_log_capture.io.channel_active := !io.inactive_channels(idx)
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

  io.log <> StreamArbiterFactory.lowerFirst.noLock.on(encoded_streams ++ Seq(syscnt_stream.stage())).stage()
  //io.log <> StreamArbiterTree(encoded_streams ++ Seq(syscnt_stream.stage())).stage()

  def getTypeName(d: Data): String = {
    d match {
      case b: Bundle => if(b.getTypeString.contains("$") || b.getTypeString.isEmpty) d.getName() else b.getTypeString
      case _ => d.getName()
    }
  }

  def getTypeName(d_clk: (Data, ClockDomain)): String = {
    val (d, cd) = d_clk
    getTypeName(d)
  }

  def sqliteHandlers(output_path : String): Unit = {
    val file = new PrintWriter(s"${output_path}/${getName()}_sqlite.c")
    def emit(s : String): Unit = {
      file.write(s)
      file.write("\n");
      file.flush()
    }

    val defined = new mutable.HashMap[String, mutable.ArrayBuffer[(Flow[Bits], Int)]]()
    for ((flow, idx) <- flows()) {
      val key = getTypeName(datas(idx))
      require(!key.isEmpty)
      defined.getOrElseUpdate(key, new mutable.ArrayBuffer[(Flow[Bits], Int)]) += ((flow, idx))
    }
    emit(
      f"""
         |#include "stdlib.h"
         |#include "sqlite3.h"
         |#include "${getName()}.h"

         |#define ${getName()}_COLUMN_DEF(f)  ", " #f " INTEGER"
         |#define ${getName()}_COLUMN_VIEW(f)  ", '" #f "'," #f
         |#define ${getName()}_QUESTION_MARK(f)  ", ?"
         |""".stripMargin)

    var all_table = new mutable.ListBuffer[String]()
    for(key <- defined.keys) {
      file.write(s"static const char* ${getName()}_${key}_CREATE_TABLE = ")
      emit(s""""CREATE TABLE IF NOT EXISTS ${getName()}_${key} (_key INTEGER PRIMARY KEY, _time INTEGER, _id INTEGER " ${getName()}_${key}_FIELDS(${getName()}_COLUMN_DEF) ")"; """)
      file.write(s"static const char* ${getName()}_${key}_CREATE_VIEW = ")
      emit(s""" "CREATE VIEW IF NOT EXISTS ${getName()}_${key}_JSON AS SELECT *, '${key}' as TYPE, JSON_OBJECT('id', _id, 'time', _time " ${getName()}_${key}_FIELDS(${getName()}_COLUMN_VIEW) ") as JSON  FROM ${getName()}_${key} ORDER BY _time"; """)

      all_table += s"SELECT _id, _time, TYPE, JSON from ${getName()}_${key}_JSON"
    }

    emit(s"""
        |typedef struct SqlLiteCtx {
        |     sqlite3 *db;
        |${defined.keys.map(key => s"     sqlite3_stmt *${key}_insert_stmt;").mkString("\n")}
        |} SqlLiteCtx;
        |static const char* ${getName()}_ALL_EVENTS_CREATE_TABLE = "CREATE VIEW IF NOT EXISTS ALL_EVENTS AS ${all_table.mkString(" UNION ")} ORDER BY _time";
        |
        |#define ${getName()}_COLUMN(f)  ", " #f
        |#define ${getName()}_FORMAT(f) ", %u"
        |#define ${getName()}_FIELD(f) , pkt.f
        |#define ${getName()}_BIND(f)  sqlite3_bind_int64(stmt, idx++, pkt.f);
        |
        |sqlite3 *${getName()}_db = 0;
        |
        |void create_table(sqlite3 * db, const char* create_stmt) {
        |    char* errmsg = 0;
        |    sqlite3_exec(db, create_stmt, 0, 0, &errmsg);
        |    if(errmsg) {
        |         fprintf(stderr, "Create table error in '%s': %s\\n", create_stmt, errmsg);
        |    }
        |}
        |
        |void GlobalLogger_handle_transaction(${getName()}_ctx* ctx, uint8_t id, const struct GlobalLogger_transaction* tx) { }
        |
        |void ${getName()}_init_sql(${getName()}_ctx* ctx, sqlite3 *db) {
        |    SqlLiteCtx* sqlCtx = (SqlLiteCtx*)calloc(sizeof(SqlLiteCtx), 1);
        |    ctx->user = sqlCtx;
        |    sqlCtx->db = db;
        |    sqlite3_exec(db, "PRAGMA synchronous = OFF", NULL, NULL, 0);
        |    sqlite3_exec(db, "PRAGMA journal_mode = OFF", NULL, NULL, 0);
        ${
      defined.keys.toList.flatMap(key => { s"""
        |    create_table(db, ${getName()}_${key}_CREATE_TABLE);
        |    create_table(db, ${getName()}_${key}_CREATE_VIEW);
        |    {
        |    const char* query = "INSERT into ${getName()}_${key} (_time, _id " ${getName()}_${key}_FIELDS(${getName()}_COLUMN) " )  VALUES (?, ? " ${getName()}_${key}_FIELDS(${getName()}_QUESTION_MARK) ")";
        |    int rc = sqlite3_prepare_v2(db, query, -1, &sqlCtx->${key}_insert_stmt, NULL);
        |    if (rc != 0) {
        |         fprintf(stderr, "Prepare error %d for %s: %s\\n", rc, query, sqlite3_errmsg(db));
        |         exit(-1);
        |    }
        |    }
        """
      }).mkString
    }
        |    create_table(db, ${getName()}_ALL_EVENTS_CREATE_TABLE);
        |}""".stripMargin)

    for (t <- datas.map(getTypeName).toSet[String]) {
      emit(s"""
              |void ${getName()}_handle_${t} (${getName()}_ctx* ctx, uint64_t time, uint8_t id, const ${getName()}_${t}_t pkt) {
              |    SqlLiteCtx* sqlCtx = ctx->user;
              |    sqlite3 * db = sqlCtx->db;
              |    char* errmsg = 0;
              |    sqlite3_stmt *stmt = sqlCtx->${t}_insert_stmt;
              |    int idx = 1;
              |    sqlite3_bind_int64(stmt, idx++, time);
              |    sqlite3_bind_int64(stmt, idx++, id);
              |    ${getName()}_${t}_FIELDS(${getName()}_BIND);
              |    int rc = sqlite3_step(stmt);
              |    if (rc != SQLITE_DONE) {
              |        fprintf(stderr, "Insert error %d: %s\\n", rc, sqlite3_errmsg(db));
              |    }
              |    sqlite3_reset(stmt);
              |}
        """.stripMargin)
    }

  }
  def codeDefinitions(output_path : String): Unit = {
    def getCType(data: Data): String = {
      val sizes = Seq(8, 16, 32, 64, 128)
      val pow2 = sizes.filter(_ >= (1 << (log2Up(data.getBitsWidth)))).head
      data match {
        case u : UInt => s"uint${pow2}_t"
        case s : SInt => s"int${pow2}_t"
        case b : Bool => "bool"
        case b : Bits => s"uint${pow2}_t"
        case e : SpinalEnum => s"uint${pow2}_t"
        case e : SpinalEnumCraft[SpinalEnum] => s"uint${pow2}_t"
        case _ => assert(false); ""
      }
    }

    val file = new PrintWriter(s"${output_path}/${getName()}.h")
    def emit(s : String): Unit = {
      file.write(s)
      file.write("\n");
      file.flush()
    }

    val defined = new mutable.HashMap[String, mutable.ArrayBuffer[(Flow[Bits], Int)]]()
    for ((flow, idx) <- flows()) {
      val key = getTypeName(datas(idx))
      defined.getOrElseUpdate(key, new mutable.ArrayBuffer[(Flow[Bits], Int)]) += ((flow, idx))
    }

    //|${flows().map { case (x, idx) => s"#define ${x.getName().toUpperCase}_ID ${idx}" }.mkString("\n")}
    emit(s"""#pragma once
               |//Generated do not edit!
               |#include "stdint.h"
               |#include "stdbool.h"
               |#include "stdio.h"
               |
               |#define ${this.name}_INDEX_BITS ${index_size}
               |#define ${this.name}_SIGNATURE 0x${signature.toHexString}
               |#define ${getName()}_EVENT_COUNT ${flows().size}
               |typedef struct ${getName()}_info_t {
               |   uint32_t ctrl;
               |   uint32_t captured_events;
               |   uint32_t checksum;
               |   uint32_t sysclk_lsb;
               |   uint32_t fifo_occupancy;
               |   uint32_t inactive_mask;
               |   uint32_t signature;
               |   uint32_t dropped_events;
               |   uint32_t event_counter[${flows().size}];
               |} ${getName()}_info_t;
               |
               |static ${getName()}_info_t ${getName()}_info_get(volatile uint32_t* base) {
               |  ${getName()}_info_t rtn = (${getName()}_info_t) {
               |    .ctrl = base[0],
               |    .captured_events = base[1],
               |    .checksum = base[5],
               |    .sysclk_lsb = base[6],
               |    .fifo_occupancy = base[7],
               |    .inactive_mask = base[9],
               |    .signature = base[12],
               |    .dropped_events = base[13]
               |  };
               |  for(int i = 0;i < ${flows().size};i++) {
               |     rtn.event_counter[i] = base[56/4 + i];
               |  }
               |  return rtn;
               |}
               |
               |typedef struct ${getName()}_ctx {
               |    void* user;
               |    uint32_t ctrl;
               |    uint64_t last_timestamp;
               |} ${getName()}_ctx;
               |
               |static void GlobalLogger_enable_memory_dump(GlobalLogger_ctx* ctx, volatile uint32_t* base, bool enable) {
               |  if(ctx->ctrl != enable) {
               |    base[0] = enable;
               |    ctx->ctrl = enable;
               |  }
               |}
               |
               |typedef struct ${getName()}_transaction {
               |  uint32_t l[3];
               |} ${getName()}_transaction;
               |
               |static void ${getName()}_handle(${getName()}_ctx* ctx, const struct ${getName()}_transaction* tx, uint32_t mask);
               |static bool ${getName()}_poll(${getName()}_ctx* ctx, volatile uint32_t* ip_location, uint32_t mask) {
               |  GlobalLogger_enable_memory_dump(ctx, ip_location, 1);
               |  struct ${getName()}_transaction tx = {0};
               |  tx.l[0] = ip_location[2 + 0];
               |  if((tx.l[0] & 1) == 1) {
               |      tx.l[1] = ip_location[2 + 1];
               |      tx.l[2] = ip_location[2 + 2];
               |
               |    ${getName()}_handle(ctx, &tx, mask);
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
               |static uint64_t ${getName}_full_time(const struct ${getName()}_transaction* tx, uint64_t gtime, uint8_t time_bit_width) {
               |  uint64_t time_part = ${getName()}_parse_field(tx, 96 - time_bit_width, 64);
               |  if(time_bit_width >= 64) {
               |      return time_part;
               |  }
               |  uint64_t mask_bits = time_bit_width;
               |  uint64_t next_incr = (1LL << mask_bits);
               |  uint64_t mask = next_incr - 1;
               |  uint64_t time_reconstruction = (gtime & ~mask) | time_part;
               |  if(time_reconstruction + (next_incr/4) < gtime) {
               |      return time_reconstruction + next_incr;
               |  }
               |  return time_reconstruction;
               |}
               |
               |#define ${getName()}_DEFINITIONS(HANDLE_DEFINE) \\
               |${defined.map(d => s"   HANDLE_DEFINE(${d._1})").mkString("\\\n")}
               |
               |""".stripMargin)

    emit(s"#define ${getName().toUpperCase}_FULL_TIME_ID 0x${((1 << index_size) - 1).toHexString}")

    def get_field_name(n : String): String = {
      if(n.matches("[0-9*].*"))
        s"_${n}"
      else n
    }

    for(key <- defined.keys) {
      val exemplar = datas(defined(key).head._2)._1
      exemplar match {
        case b : MultiData => {
          emit(s"typedef struct ${getName()}_${key}_t {")
          b.elements.foreach(x => {
            val prefix = get_field_name(x._1)
            emit(s"\t${getCType(x._2)} ${prefix};")
          })
          emit(s"} ${getName()}_${key}_t;")

          emit(s"#define ${getName()}_${key}_FIELDS(HANDLE_FIELD) \\")
          b.elements.foreach(x => {
            val prefix = get_field_name(x._1)
            emit(s"\tHANDLE_FIELD(${prefix}) \\")
          })
          emit("")

        }
        case _ => {
          emit(s"#define ${getName()}_${key}_FIELDS(HANDLE_FIELD) HANDLE_FIELD(value)")
          emit(s"typedef struct ${getName()}_${key}_t {")
          emit(s"    ${getCType(exemplar)} value;")
          emit(s"} ${getName()}_${key}_t;")
        }
      }
    }

    emit(s"static const char* ${getName()}_get_id_name(int id) {")
    emit(s"   switch(id) {")
    for ((flow, idx) <- flows()) {
      emit(s"   case ${idx}: return ${'"' + flow.getName() + '"'};")
    }
    emit("    }")
    emit("    return \"UNKNOWN\";")
    emit("}")

    val handledTypes = new mutable.HashSet[String]()
    def emitTypeFunctions(d : Data): Unit = {
      val typeName = getTypeName(d)

      var bitOffset = 0
      val time_bits = logBits - d.getBitsWidth - index_size
      emit(s"#define ${d.getName()}_TIME_BIT_WIDTH ${time_bits}")
      d match {
        case b : MultiData => {
          val parent_name = if(d.parent != null) d.parent.name else s"${d.name}"

          if(handledTypes.contains(parent_name))
            return
          handledTypes.add(parent_name)

          b.elements.foreach(x => {
            val prefix = s"${parent_name}_${x._1}"
            emit(s"#define ${s"${prefix}_BIT_OFFSET".padTo(32, ' ')} ${bitOffset}")
            emit(s"#define ${s"${prefix}_BIT_WIDTH".padTo(32, ' ')} ${x._2.getBitsWidth}")
            bitOffset += x._2.getBitsWidth
          })
          emit(s"static ${getName()}_${typeName}_t ${getName()}_parse_${parent_name}(const ${getName()}_transaction* tx) {")
          emit(s"\treturn (${getName()}_${typeName}_t) {")
          b.elements.foreach(x => {
            val prefix = s"${parent_name}_${x._1}"
            emit(s"\t\t.${get_field_name(x._1)} = ${getName()}_parse_field(tx, ${prefix}_BIT_OFFSET + ${this.name}_INDEX_BITS + 1 /* VALID bit */, ${prefix}_BIT_WIDTH),")
          })
          emit("\t};")
          emit("}")
        }
        case _ => {
          val time_bits = logBits - d.getBitsWidth - index_size
          val prefix = s"${d.getName()}"
          emit(
            s"""
               |#define ${prefix}_TIME_BIT_WIDTH ${time_bits}
               |#define ${s"${prefix}_BIT_OFFSET".padTo(32, ' ')} ${bitOffset}
               |#define ${s"${prefix}_BIT_WIDTH".padTo(32, ' ')} ${d.getBitsWidth}
               |static ${getName()}_${typeName}_t ${getName()}_parse_${d.getName()}(const ${getName()}_transaction* tx) {
               |   return (${getName()}_${typeName}_t){ .value = ${getName()}_parse_field(tx, ${prefix}_BIT_OFFSET + ${this.name}_INDEX_BITS + 1 /* VALID bit */, ${prefix}_BIT_WIDTH) };
               |}
               |""".stripMargin)
        }
      }
    }

    datas.foreach(d_clk => {
      val (d, cd) = d_clk
      emitTypeFunctions(d)
    })

    emit(s"void ${getName()}_handle_transaction(${getName()}_ctx* ctx, uint8_t id, const struct ${getName()}_transaction* tx);")
    for (t <- datas.map(getTypeName).toSet[String]) {
      emit(s"void ${getName()}_handle_${t} (${getName()}_ctx* ctx, uint64_t time, uint8_t id, const ${getName()}_${t}_t pkt);")
    }

    emit(
      s"""
         |static uint8_t ${getName()}_get_id(const struct ${getName()}_transaction* tx){ return ${getName()}_parse_field(tx, 1, ${index_size}); }
         |static void ${getName()}_handle(${getName()}_ctx* ctx, const struct ${getName()}_transaction* tx, uint32_t mask){
         |    uint8_t id = ${getName()}_get_id(tx);
         |    if(mask & (1 << id)) return;
         |    ${getName()}_handle_transaction(ctx, id, tx);
         |    switch(id) {
         |""".stripMargin)

    for ((flow, idx) <- flows()) {
      emit(
        s"""   case ${idx}: {
           |      ctx->last_timestamp = ${getName}_full_time(tx, ctx->last_timestamp, ${flow.getName()}_TIME_BIT_WIDTH);
           |      ${getName()}_handle_${getTypeName(datas(idx))}(ctx, ctx->last_timestamp, id, ${getName()}_parse_${flow.getName()}(tx));
           |      break;
           |    }""".stripMargin)
    }
    emit(
      s"""
         |   case ${(1 << index_size) - 1}: ctx->last_timestamp = ${getName()}_parse_field(tx, 1 + ${syscnt_padding}, 64); break;
         |   default: fprintf(stderr, "Unknown id %d\\n", id);
         |  }
         |}
      """.stripMargin)
  }

  def create_logger_port(sysBus: GlobalBus_t, address: BigInt, depth: Int,
                         outputStream : Option[Stream[Bits]] = None): Unit = {
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

    if(sysBus == null) {
      stream >> outputStream.get
      loggerFifo.io.flush := False
      io.manual_trigger.clearAll()
      return
    }

    val logger_port = sysBus.add_slave_factory("logger_port", SizeMapping(address, 1 KiB), "cpu")

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
    }
    )

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