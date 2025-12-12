package spinalextras.lib.logging.FlowLoggerUtils

import spinal.core.{Bits, Bool, Bundle, Data, MultiData, SInt, SpinalEnum, SpinalEnumCraft, UInt, assert, log2Up}
import spinal.lib.Flow
import spinalextras.lib.logging.FlowLogger

import java.io.PrintWriter
import scala.collection.mutable

object FlowLoggerCCode {
  def apply(logger : FlowLogger, output_path : String): Unit = {
    import logger._

    def getCType(data: Data): String = {
      val sizes = Seq(8, 16, 32, 64, 128)
      val pow2 = sizes.filter(_ >= (1 << (log2Up(data.getBitsWidth)))).head
      data match {
        case u : UInt => s"uint${pow2}_t"
        case s : SInt => s"int${pow2}_t"
        case b : Bool => "bool"
        case b : Bits => s"uint${pow2}_t"
        case e : SpinalEnum => s"uint${pow2}_t"
        case e : SpinalEnumCraft[_] => s"uint${pow2}_t"
        case b : Bundle => getCType(b.asBits)
        case _ => assert(false); ""
      }
    }

    val file = new PrintWriter(s"${output_path}/event_logger_defs.h")
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
            |#include "event_logger.h"
            |
            |/****
            |${comments.mkString("\n\n")}
            |**/
            |
            |uint32_t ${name}_INDEX_BITS = ${index_size};
            |uint32_t ${name}_SIGNATURE = 0x${signature.toHexString};
            |uint32_t ${getName()}_EVENT_COUNT = ${flows().size};
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
            |bool ${getName()}_poll(${getName()}_ctx* ctx, volatile uint32_t* ip_location, uint32_t mask) {
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
      assert(n.nonEmpty)

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

    emit(s"const char* ${getName()}_get_id_name(int id) {")
    emit(s"   switch(id) {")
    for ((flow, idx) <- flows()) {
      emit(s"   case ${idx}: return ${'"' + flow.getName() + '"'};")
    }
    emit("    }")
    emit("    return \"UNKNOWN\";")
    emit("}")

    def getParseName(d : Data): String = {
      d match {
        case b: MultiData => {
          val parent_name = if (d.parent != null) d.parent.name else s"${d.name}"
          f"${getName()}_parse_${parent_name}"
        }
        case _ => {
          f"${getName()}_parse_${d.getName()}"
        }
      }
    }

    val handledTypes = new mutable.HashSet[String]()
    def emitTypeFunctions(d : Data): Unit = {
      val typeName = getTypeName(d)

      var bitOffset = 0
      val time_bits = cfg.logBits - d.getBitsWidth - index_size
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
            emit(s"\t\t.${get_field_name(x._1)} = ${getName()}_parse_field(tx, ${prefix}_BIT_OFFSET + ${name}_INDEX_BITS + 1 /* VALID bit */, ${prefix}_BIT_WIDTH),")
          })
          emit("\t};")
          emit("}")
        }
        case _ => {
          val time_bits = cfg.logBits - d.getBitsWidth - index_size
          val prefix = s"${d.getName()}"
          emit(
            s"""
               |#define ${prefix}_TIME_BIT_WIDTH ${time_bits}
               |#define ${s"${prefix}_BIT_OFFSET".padTo(32, ' ')} ${bitOffset}
               |#define ${s"${prefix}_BIT_WIDTH".padTo(32, ' ')} ${d.getBitsWidth}
               |static ${getName()}_${typeName}_t ${getName()}_parse_${d.getName()}(const ${getName()}_transaction* tx) {
               |   return (${getName()}_${typeName}_t){ .value = ${getName()}_parse_field(tx, ${prefix}_BIT_OFFSET + ${name}_INDEX_BITS + 1 /* VALID bit */, ${prefix}_BIT_WIDTH) };
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
           |      ${getName()}_handle_${getTypeName(datas(idx))}(ctx, ctx->last_timestamp, id, ${getParseName(datas(idx)._1)}(tx));
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
}