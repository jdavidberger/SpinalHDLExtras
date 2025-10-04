"""
event_db.py

Create SQLite tables from YAML event definitions and insert decoded event dictionaries.

Usage:
    db = EventDB("events.db")
    db.create_tables_from_yaml("spec.yaml")
    db.insert_event(decoded_event_dict)  # decoded_event_dict is the dict produced by your decoder

Author: generated
"""

import sqlite3
import yaml
import json
import re
from typing import Any, Dict, List, Tuple, Optional
import os
import time
from event_logger import *

# -------------------------
# Naming / normalization
# -------------------------
_non_alnum_re = re.compile(r"[^0-9A-Za-z_]+" )
_multi_underscore_re = re.compile(r"__+")

def normalize_identifier(name: str) -> str:
    """
    Keep case, replace illegal characters with '_' and collapse multiple underscores,
    then strip leading/trailing underscores.
    """
    s = _non_alnum_re.sub("_", name)
    s = _multi_underscore_re.sub("_", s)
    s = s.strip("_")
    if not s:
        s = "_"
    return s

# -------------------------
# Schema builder utilities
# -------------------------

PrimitiveCol = Tuple[str, str]  # (colname, sqlite_type)
EnumCol = Tuple[str, str, List[str]]  # (colname, sqlite_type, enum_list) - sqlite_type is TEXT for enum


class EventSchema:
    """
    Holds the flattened schema for an event type.

    fields: ordered list of tuples describing columns; tuple types:
       - ("colname", "INTEGER" or "TEXT" or "BOOLEAN")
       - ("colname", "ENUM", enum_list)  -> used to create two columns: colname TEXT, colname_idx INTEGER
    """
    def __init__(self, event_name: str):
        self.event_name = event_name
        self.cols: List[Tuple] = []  # see above
        # quick lookup: mapping normalized col -> original spec
        self._col_index: Dict[str, Tuple] = {}

    def add_col(self, raw_colname: str, spec: Tuple):
        """
        raw_colname: original column base name (will be normalized)
        spec: ("INTEGER"/"TEXT"/"BOOLEAN") or ("ENUM", enum_list)
        """
        colname = normalize_identifier(raw_colname)
        # ensure uniqueness: if column exists, append numeric suffix
        base = colname
        i = 1
        while colname in self._col_index:
            colname = f"{base}_{i}"
            i += 1

        # store as (colname, spec)
        if isinstance(spec, tuple) and spec[0] == "ENUM":
            # for enums, spec = ("ENUM", enum_list)
            self.cols.append((colname, "ENUM", spec[1]))
            self._col_index[colname] = ("ENUM", spec[1])
        else:
            # primitive: spec is sqlite type string
            self.cols.append((colname, spec))
            self._col_index[colname] = (spec,)

    def iter_sql_columns(self) -> List[Tuple[str, str]]:
        """
        Return list of (column_name, sqlite_type) for CREATE TABLE.
        For ENUM yields two columns: (colname, TEXT) and (colname_idx, INTEGER)
        """
        out: List[Tuple[str, str]] = []
        for col in self.cols:
            if col[1] == "ENUM":
                name = col[0]
                out.append((name, "TEXT"))
                out.append((f"{name}_idx", "INTEGER"))
            else:
                out.append((col[0], col[1]))
        # timestamp will be added separately by caller if desired
        return out

# -------------------------
# Walk the YAML type defs and build flattened schema
# -------------------------

def _is_enum_list(t) -> bool:
    return isinstance(t, list) and len(t) > 0 and all(isinstance(x, str) for x in t)

def _is_composite_list(t) -> bool:
    # list of dicts representing nested fields, per your parse_type else branch
    return isinstance(t, list) and len(t) > 0 and all(isinstance(x, dict) for x in t)

def _primitive_spec_from_dict(spec: dict) -> Optional[str]:
    """
    Given a dict like {"UInt": 32} or {"Bool":1} or {"SInt":16} or {"Bits": 8}
    Return sqlite type as string: "INTEGER" or "BOOLEAN" etc.
    Caller must also extract bit widths if needed elsewhere.
    """
    key = next(iter(spec.keys()))
    value = spec[key]

    if key in ("UInt", "Bits"):
        if value > 32:
            return "UNSIGNED BIGINT"
        return "UNSIGNED INTEGER"
    if key == "SInt":
        if value > 32:
            return "BIGINT"
        return "INTEGER"
    if key == "Bool":
        return "BOOLEAN"
    return None


def flatten_type(prefix: str, tdef: Any, schema: EventSchema) -> None:
    """
    Recursively flatten the type definition `tdef` into columns appended to schema.
    prefix: column name prefix (may be empty)
    tdef: can be
       - enum list (list of strings)
       - primitive dict { "UInt": N } or {"Bool":1}, {"SInt":N}, {"Bits":N}
       - dict: { fieldname: nested_type } OR a primitive dict treated above
       - list of dicts: [ { field1: type1 }, { field2: type2 }, ... ] (composite)
    """
    if _is_enum_list(tdef):
        # enum stored as two columns: TEXT and INTEGER index
        name = prefix
        schema.add_col(name, ("ENUM", list(tdef)))
        return

    if isinstance(tdef, dict):
        # Check if it's a primitive dict (single key with integer value)
        if len(tdef) == 1 and isinstance(next(iter(tdef.values())), int):
            # primitive
            sqlite_type = _primitive_spec_from_dict(tdef)
            if sqlite_type is None:
                raise ValueError(f"Unknown primitive type in spec: {tdef}")
            schema.add_col(f"${'value' if len(prefix) == 0 else prefix}", sqlite_type)
            return

        # Otherwise it's a mapping of named fields
        for key, val in tdef.items():
            new_prefix = f"{prefix}_{key}" if prefix else key
            flatten_type(new_prefix, val, schema)
        return

    if _is_composite_list(tdef):
        # list of dicts where each element is a { field: type }
        for elem in tdef:
            if not isinstance(elem, dict) or len(elem) != 1:
                raise ValueError("Unsupported composite list element: must be single-key dict")
            key = next(iter(elem.keys()))
            val = elem[key]
            new_prefix = f"{prefix}_{key}" if prefix else key
            flatten_type(new_prefix, val, schema)
        return

    raise ValueError(f"Unhandled type definition during flatten: {tdef!r}")

# -------------------------
# EventDB class
# -------------------------

class EventDB:
    def __init__(self, db_path: str):
        self.set_path(db_path)

        # in-memory mapping: event_name -> EventSchema
        self.schemas: Dict[str, EventSchema] = {}
        self.insert_statements = {}

    def close(self):
        self.conn.close()

    def set_path(self, db_path):
        self.db_path = db_path
        if self.db_path is not None:
            self.conn = sqlite3.connect(self.db_path)
            self.conn.row_factory = sqlite3.Row
            self.cursor = self.conn.cursor()
            self.conn.execute("PRAGMA journal_mode = OFF;")
            self.conn.execute("PRAGMA synchronous = 0;")
            self.conn.execute("PRAGMA cache_size = 1000000;")
            #self.conn.execute("PRAGMA locking_mode = EXCLUSIVE;")
            self.conn.execute("PRAGMA temp_store = MEMORY;")


    def create_tables_from_yaml(self, yaml_path: str):
        with open(yaml_path, "r") as f:
            data = yaml.safe_load(f)

        event_defs = data.get("event_definitions", [])

        if self.db_path is None:
            signature = data.get("signature", "nosig")
            yaml_base = os.path.splitext(os.path.basename(yaml_path))[0]
            timestamp = time.strftime("%Y-%m-%d_%H-%M-%S")

            self.set_path(f"{yaml_base}_{signature:08X}_{timestamp}.sqlite")

        # create ALL_EVENTS first
        self._create_all_events_table()

        for ev in event_defs:
            name = ev["name"]
            schema = EventSchema(name)
            tdef = ev.get("type", {})
            # flatten tdef into schema
            flatten_type("", tdef, schema)
            # ensure timestamp column exists for each event
            # We'll add a 'timestamp' column explicitly in SQL creation step
            self._create_event_table(schema)
            self.schemas[name] = schema

        return data

    def _create_all_events_table(self):
        cur = self.conn.cursor()
        cur.execute("""
            CREATE TABLE IF NOT EXISTS ALL_EVENTS (
                event_id INTEGER NOT NULL,
                event_name TEXT NOT NULL,
                timestamp REAL NOT NULL,
                json_data TEXT NOT NULL
            )
        """)
        self.conn.commit()

    def _create_event_table(self, schema: EventSchema):
        table_name = normalize_identifier(schema.event_name)
        cols = schema.iter_sql_columns()

        # add timestamp column at front
        sql_cols = [("event_id", "INTEGER"), ("timestamp", "REAL")] + cols
        # build create statement
        col_defs = ",\n  ".join(f"{cname} {ctype}" for cname, ctype in sql_cols)
        sql = f"CREATE TABLE IF NOT EXISTS \"{table_name}\" (\n  {col_defs}\n)"
        cur = self.conn.cursor()
        cur.execute(sql)
        self.conn.commit()

    def _get_insert(self, schema_name):
        if schema_name in self.insert_statements:
            return self.insert_statements[schema_name]

        schema = self.schemas[schema_name]
        sql_cols = ["event_id", "timestamp"]
        placeholders = ["?", "?"]

        for col in schema.cols:
            if col[1] == "ENUM":
                colname = col[0]
                sql_cols.append(colname)
                placeholders.append("?")
                sql_cols.append(f"{colname}_idx")
                placeholders.append("?")
            else:
                # primitive
                colname = col[0]
                sql_cols.append(colname)
                placeholders.append("?")

        # Build and execute insert into specific event table
        cols_sql = ", ".join(f"\"{c}\"" for c in sql_cols)
        ph_sql = ", ".join(placeholders)

        table_name = normalize_identifier(schema_name)
        insert_sql = f"INSERT INTO \"{table_name}\" ({cols_sql}) VALUES ({ph_sql})"
        self.insert_statements[schema_name] = insert_sql

        return insert_sql

    def _get_values(self, schema_name, event):
        ev_id = event.get("event_id")
        ev_name = event.get("event")
        if ev_name is None:
            raise ValueError("Event missing 'event' name")

        value = event.get("value", {})
        timestamp = event.get("timestamp", 0)
        values = [ev_id, timestamp]
        schema = self.schemas[schema_name]
        for col in schema.cols:
            if col[1] == "ENUM":
                # col is (colname, "ENUM", enum_list)
                colname = col[0]
                enum_list = col[2]
                txt_value = value.get(colname)

                try:
                    idx_value = enum_list.index(txt_value) if txt_value in enum_list else None
                except ValueError:
                    idx_value = None

                values.append(txt_value)
                values.append(idx_value)
            else:
                # primitive
                colname = col[0]
                # put raw value (or None)
                val = value.get(colname) if isinstance(value, dict) else value

                # Convert bool to int for sqlite if necessary
                if col[1] == "BOOLEAN" and val is not None:
                    # Accept python bool or 0/1
                    if isinstance(val, bool):
                        values.append(1 if val else 0)
                    else:
                        # try to coerce
                        try:
                            values.append(1 if int(val) else 0)
                        except Exception:
                            values.append(None)
                else:
                    values.append(val)

        return values

    # -------------------------
    # Insertion helpers
    # -------------------------
    def insert_event(self, event: Dict[str, Any]):
        """
        Insert event dict into the correct per-event table and also into ALL_EVENTS.
        event is expected to be:
           {
             "event": <event_name>,
             "timestamp": <int>,
             "value": { ... }   # flattened dict as produced by your decoder
           }
        """
        ev_id = event.get("event_id")
        ev_name = event.get("event")
        if ev_name is None:
            raise ValueError("Event missing 'event' name")

        if ev_name == "time_sync":
            return

        if ev_name not in self.schemas:
            raise ValueError(f"Unknown event type (not in schema): {ev_name}")

        insert_sql = self._get_insert(ev_name)
        values = self._get_values(ev_name, event)

        cur = self.cursor
        try:
            cur.execute(insert_sql, values)
        except Exception as e:
            print(insert_sql, values, event)
            raise e

        timestamp = event.get("timestamp", 0)
        # Also insert into ALL_EVENTS: store value JSON (the original dict), plus event_name and timestamp
        all_json = json.dumps(event["value"], default=str)
        cur.execute("INSERT INTO ALL_EVENTS (event_id, event_name, timestamp, json_data) VALUES (?, ?, ?, ?)",
                    (ev_id, ev_name, timestamp, all_json))

        self.conn.commit()

    # -------------------------
    # Convenience / utilities
    # -------------------------
    def table_exists(self, name: str) -> bool:
        cur = self.conn.cursor()
        cur.execute("SELECT name FROM sqlite_master WHERE type='table' AND name=?", (normalize_identifier(name),))
        return cur.fetchone() is not None

    def get_schema_columns(self, event_name: str) -> List[Tuple[str, str]]:
        """Return list of (column_name, column_type) for a given event table"""
        if event_name not in self.schemas:
            raise ValueError("Unknown event")
        table_name = normalize_identifier(event_name)
        cur = self.conn.cursor()
        cur.execute(f"PRAGMA table_info(\"{table_name}\")")
        return [(row["name"], row["type"]) for row in cur.fetchall()]

# -------------------------
# If run as script: simple demo
# -------------------------
if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description="Create SQLite tables from YAML event defs.")
    parser.add_argument("yaml", help="YAML spec path")
    parser.add_argument("--db", help="SQLite DB path", default=None)
    parser.add_argument("--input", help="Binary input file to decode and insert (optional).")

    args = parser.parse_args()

    db = EventDB(args.db)
    event_config = db.create_tables_from_yaml(args.yaml)
    print("Created tables for event definitions in", db.db_path)

    print_time = time.time()
    row_idx = 0

    data_stream = open_data_stream(args.input)
    for event in decode_log_stream(data_stream, event_config):
        row_idx = row_idx + 1
        now = time.time()
        if print_time + 1 < now:
            print(f"Captured {row_idx / (now - print_time)} per second")
            print_time = now
            row_idx = 0
        db.insert_event(event)

    db.close()
