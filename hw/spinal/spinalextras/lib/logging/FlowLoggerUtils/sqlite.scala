package spinalextras.lib.logging.FlowLoggerUtils

import spinal.core.Bits
import spinal.lib.Flow
import spinalextras.lib.logging.FlowLogger

import java.io.PrintWriter
import scala.collection.mutable

object FlowLoggerSqlite {
  def apply(flowLogger : FlowLogger, output_path : String): Unit = {
    import flowLogger._

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
         |#include "event_logger_defs.h"

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
}