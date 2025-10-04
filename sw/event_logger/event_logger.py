import yaml
import struct
import sys

from jtag_tap import *

# ---- Bitfield helpers translated from your C ----

def shift(d: int, s: int) -> int:
    return d >> s if s >= 0 else d << -s

def global_logger_parse_field(tx, bit_offset: int, bit_width: int) -> int:
    if bit_width == 0:
        return 0
    l0, l1, l2 = tx  # 3 x uint32 words
    mask = (1 << bit_width) - 1 if bit_width != 64 else 0xFFFFFFFFFFFFFFFF
    return (
        shift(l0, bit_offset) |
        shift(l1, bit_offset - 32) |
        shift(l2, bit_offset - 64)
    ) & mask

def global_logger_full_time(tx, gtime: int, time_bit_width: int) -> int:
    time_part = global_logger_parse_field(tx, 96 - time_bit_width, 64)
    if time_bit_width >= 64:
        return time_part
    next_incr = 1 << time_bit_width
    mask = next_incr - 1
    time_reconstruction = (gtime & ~mask) | time_part
    if time_reconstruction + (next_incr // 4) < gtime:
        return time_reconstruction + next_incr
    return time_reconstruction


# ---- Parse YAML into usable structures ----

def load_event_definitions(path):
    with open(path, "r") as f:
        data = yaml.safe_load(f)

    return data


# ---- Streaming decoder ----

def decode_log_stream(stream, event_def_metadata):
    gtime = 0  # Last timestamp

    index_size = event_def_metadata["index_size"]
    event_defs = event_def_metadata["event_definitions"]
    clock_scale = 1. / float(event_def_metadata["clock_freq"])
    syscnt_padding = event_def_metadata["log_bits"] - index_size - 64

    while True:
        raw = next(stream)#.read(12)  # 3 * uint32_t
        if raw is None:
            break

        tx = None
        if isinstance(raw, list):
            tx = raw
        else:
            l0, l1, l2 = struct.unpack("<III", raw)
            tx = (l0, l1, l2)

        # Decode index
        index = global_logger_parse_field(tx, 1, index_size)

        if index == (1 << index_size) - 1:
            gtime = global_logger_parse_field(tx, 1 + syscnt_padding, 64)

            yield {
                "event_id": index,
                "event": "time_sync",
                "cycle": gtime,
                "timestamp": gtime * clock_scale,
            }
            continue

        if index >= len(event_defs):
            yield {"error": f"Invalid index {index}", "tx": tx}
            continue

        event_def = event_defs[index]

        # Decode timestamp
        time_bits = event_def["time_bits"]
        timestamp = global_logger_full_time(tx, gtime, time_bits)
        gtime = timestamp

        # Decode fields
        fields = {}
        bit_offset = index_size + 1  # Skip VALID + index

        def parse_type(tdef, offset):
            if isinstance(tdef, list) and isinstance(tdef[0], str):
                spec = tdef
                width = (len(spec) - 1).bit_length()
                idx = global_logger_parse_field(tx, offset, width)
                return (spec[idx] if idx < len(spec) else f"INVALID({idx})"), offset + width
            elif isinstance(tdef, dict):
                spec = tdef
                key = next(iter(tdef.keys()))
                width = next(iter(tdef.values()))
                is_primitive = len(spec) == 1 and isinstance(width, int)

                is_uint = ("UInt" == key or "Bits" == key)
                if is_primitive:
                    if is_uint and width < 64:
                        return global_logger_parse_field(tx, offset, width), offset + width
                    elif "Bool" == key:
                        return bool(global_logger_parse_field(tx, offset, width)), offset + width
                    elif "SInt" == key or (is_uint and width >= 64):
                        raw = global_logger_parse_field(tx, offset, width)
                        # Sign-extend
                        sign_bit = 1 << (width - 1)
                        if raw & sign_bit:  # negative number
                            raw -= 1 << width
                        return raw, offset + width
                    else:
                        raise ValueError(f"Unknown type spec in {tdef}", spec)
                raise ValueError(f"Unknown! type spec in {tdef}")
            else:
                out = {}
                for elem in tdef:
                    key = list(elem.keys())[0]
                    val, offset = parse_type(elem[key], offset)
                    out[key] = val
                return out, offset

        fields, _ = parse_type(event_def["type"], bit_offset)

        yield {
            "event_id": index,
            "event": event_def["name"],
            "cycle": timestamp,
            "timestamp": timestamp * clock_scale,
            "value": fields
        }

def open_data_stream(file_arg = None):
    data_stream = None
    if file_arg is not None:
        file = sys.stdin.buffer if file_arg == "-" else open(file_arg, "rb")
        def data_stream_from_file():
            while True:
                yield file.read(12)
        data_stream = data_stream_from_file()
    else:
        url = 'ftdi://ftdi:0x6010/1'
        jtag = Jtag(url)
        data_stream = jtag.capture_event_stream()
    return data_stream

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("yaml")
    parser.add_argument("file", nargs="?", default=None)
    args = parser.parse_args()

    event_def_metadata = load_event_definitions(args.yaml)
    data_stream = open_data_stream(args.file)

    print_time = time.time()
    row_idx = 0

    for evt in decode_log_stream(data_stream, event_def_metadata):
        row_idx = row_idx + 1
        now = time.time()
        if print_time + 1 < now:
            print(f"Captured {row_idx / (now - print_time)} per second")
            print_time = now
            row_idx = 0
        print(evt)
