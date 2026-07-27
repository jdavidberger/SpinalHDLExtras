#!/usr/bin/env python3
"""Regenerates this directory's RouterNode-internals diagrams (README.md's
lead figures). Usage, from this directory:

    python3 generate_router_diagrams.py ring   # -> router-node.svg
    python3 generate_router_diagrams.py mesh   # -> mesh-router-node.svg

No third-party dependencies (stdlib only). To edit which example paths are
shown as "currently granted", or add a companion diagram for another
topology, edit the `grants`/`ports` lists in the `__main__` block below and
re-run -- everything else (layout, crossbar candidates, colors) is derived
automatically from the port list and vc_count.

Generates a RouterNode-internals crossbar diagram SVG for an arbitrary
canonical port list. Port 0 must be Local. Reused for both the 3-port
Ring example and the 5-port Mesh interior-node example.

Color convention: every crossbar candidate line is colored by its SOURCE
port (LINE_COLORS[src_idx]), faint. A chosen set of "currently granted"
example paths is drawn bold/opaque in that same source-port color, and
that color is carried through every stage -- external input, StreamDemux
fan-out, StreamFifo, FlitRouter, the crossbar line, the allocator's line
into OutputPort -- so a reader can trace one packet's whole path by eye.

The grant list deliberately includes:
  - one input port with TWO simultaneously granted lanes (its two VCs,
    each committed to a different destination) -- showing that a single
    physical input link can carry two independent, concurrently
    wormhole-locked flows.
  - one output port whose VirtualIdAllocator/OutputPort has TWO
    simultaneously granted incoming candidates (from two different
    sources, landing on its two destination VC lanes) -- showing the
    same concurrency from the merge side.
A destination with 2 grants has its final external-output segment left
neutral (not single-colored) since it's genuinely time-multiplexing two
distinctly-colored flows past that point.
"""
import sys
import textwrap

COLORS = {
    "ext": "#4b6fa8",
    "fifo_fill": "#eef6ee", "fifo_stroke": "#4a8a55", "fifo_text": "#1a3320",
    "fr_fill": "#f2f2f2", "fr_stroke": "#888", "fr_text": "#2a2a2a",
    "via_fill": "#f5eef7", "via_stroke": "#8a4a97", "via_text": "#2f1834",
    "op_fill": "#fdf3e3", "op_stroke": "#b8863b", "op_text": "#3a2c10",
    "demux_fill": "#eef2f8", "demux_stroke": "#4a6a97",
    "boundary": "#9aa0a8",
}

# bright/saturated, keyed by canonical port index (0 = Local)
LINE_COLORS = ["#e63946", "#1f6feb", "#1ea672", "#f2790a", "#9d4edd"]

LANE_H = 50
LABEL_H = 14
UNIT_H = LABEL_H + LANE_H
LANE_GAP = 10
PAD = 12
ROW_GAP = 30
LOCAL_ANNOTATION_RESERVE = 112

X_EXT_IN0, X_IP0 = 0, 106
X_DEMUX0, X_DEMUX1 = 118, 174
X_FIFO0, X_FIFO1 = 196, 284
X_IP1 = 300
X_ARR0, X_ARR1 = 284, 303
X_FR0, X_FR1 = 305, 395
X_VIA0, X_VIA1 = 680, 870
X_OP0, X_OP1 = 900, 1040
VIA_W = X_VIA1 - X_VIA0
OP_W = X_OP1 - X_OP0
VIA_H = 108


def esc(s):
    return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")


def wrap_candidate(text, width=34):
    return textwrap.wrap(text, width=width, break_long_words=False)[:2]


def wrap_tokens(tokens, width=95):
    lines, cur = [], []
    cur_len = 0
    for t in tokens:
        add = len(t) + (2 if cur else 0)
        if cur and cur_len + add > width:
            lines.append(", ".join(cur))
            cur, cur_len = [], 0
        cur.append(t)
        cur_len += len(t) + (2 if len(cur) > 1 else 0)
    if cur:
        lines.append(", ".join(cur))
    return lines


def candidate_desc(ports, n, i, vc_count):
    lanes_per_port = [1 if k == 0 else vc_count for k in range(n)]
    total = sum(lanes_per_port)
    if i == 0:
        return f"{total} candidates {chr(8212)} every port's every VC (Local included)"
    cnt = total - lanes_per_port[i]
    name = ports[i][0]
    vcs = "/".join(f"vc{v}" for v in range(vc_count))
    return f"{cnt} candidates {chr(8212)} every port except {name}'s own {vcs}"


def build(ports, vc_count, title, subtitle_lines, out_path, grants, canvas_w=1220):
    """grants: list of (src_name, vc, dst_name) -- currently-granted example paths.
    Each (src_name, vc) pair must be unique (a VC lane can only be granted to one
    destination at a time), but a dst_name MAY appear more than once (up to
    vc_count times) -- that's exactly how one output's VirtualIdAllocator can have
    several concurrently granted incoming candidates."""
    n = len(ports)
    name_idx = {p[0]: i for i, p in enumerate(ports)}
    lanes_per_port = [1 if i == 0 else vc_count for i in range(n)]

    grant_by_lane = {}          # (src_idx, vc) -> (dst_idx, color)
    grants_by_dst = {}          # dst_idx -> [(src_idx, vc, color), ...]
    grants_by_src_port = {}     # src_idx -> [(vc, dst_idx, color), ...]  (for input-side coloring)
    for gs, gvc, gd in grants:
        si, di = name_idx[gs], name_idx[gd]
        color = LINE_COLORS[si % len(LINE_COLORS)]
        assert (si, gvc) not in grant_by_lane, f"lane ({gs},vc{gvc}) granted twice"
        grant_by_lane[(si, gvc)] = (di, color)
        grants_by_dst.setdefault(di, []).append((si, gvc, color))
        grants_by_src_port.setdefault(si, []).append((gvc, di, color))
    for di, lst in grants_by_dst.items():
        assert len(lst) <= vc_count, f"output {ports[di][0]} over-granted ({len(lst)} > {vc_count} VCs)"

    def block_height(lanes):
        return lanes * UNIT_H + (lanes - 1) * LANE_GAP

    header_ty = 46 + 16 * len(subtitle_lines) + 16
    top_margin = header_ty + 60

    centers = []
    y = top_margin
    for i in range(n):
        bh = block_height(lanes_per_port[i])
        container_top = y
        center = container_top + PAD + bh / 2
        container_bottom = container_top + PAD * 2 + bh
        centers.append(center)
        y = container_bottom + ROW_GAP
        if ports[i][0] == "Local":
            y += LOCAL_ANNOTATION_RESERVE

    boundary_bottom = y - ROW_GAP + 10
    legend_top = boundary_bottom + 46
    canvas_h = legend_top + 148
    ext_out_x = canvas_w - 14

    svg = []
    svg.append(f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {canvas_w} {canvas_h}" '
                f'font-family="Segoe UI, Helvetica, Arial, sans-serif">')
    svg.append('<defs>')
    svg.append('<marker id="arrowGray" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" '
                'orient="auto-start-reverse"><path d="M0,0 L10,5 L0,10 z" fill="#555"/></marker>')
    for idx, c in enumerate(LINE_COLORS):
        svg.append(f'<marker id="arrowC{idx}" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="6" markerHeight="6" '
                    f'orient="auto-start-reverse"><path d="M0,0 L10,5 L0,10 z" fill="{c}"/></marker>')
        svg.append(f'<marker id="arrowB{idx}" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="8" markerHeight="8" '
                    f'orient="auto-start-reverse"><path d="M0,0 L10,5 L0,10 z" fill="{c}"/></marker>')
    svg.append('</defs>')

    cx = canvas_w / 2
    svg.append(f'<rect x="0" y="0" width="{canvas_w}" height="{canvas_h}" fill="#ffffff"/>')
    svg.append(f'<text x="{cx}" y="26" text-anchor="middle" font-size="18" font-weight="600" '
                f'fill="#1c2b40">{esc(title)}</text>')
    ty = 46
    for line in subtitle_lines:
        svg.append(f'<text x="{cx}" y="{ty}" text-anchor="middle" font-size="12.5" fill="#555">{esc(line)}</text>')
        ty += 16
    grant_tokens = [f"{gs}{chr(183)}vc{gvc}{chr(8594)}{gd}" for gs, gvc, gd in grants]
    lead = f"bold colored paths = {len(grants)} example concurrently-granted wormhole-locked routes: "
    grant_lines = wrap_tokens(grant_tokens, width=100)
    svg.append(f'<text x="{cx}" y="{ty+2:.0f}" text-anchor="middle" font-size="11" font-style="italic" '
                f'fill="#8a5a1a">{esc(lead + grant_lines[0])}</text>')
    ty += 16
    for extra in grant_lines[1:]:
        svg.append(f'<text x="{cx}" y="{ty+2:.0f}" text-anchor="middle" font-size="11" font-style="italic" '
                    f'fill="#8a5a1a">{esc(extra)}</text>')
        ty += 16

    svg.append(f'<rect x="90" y="{ty+18:.0f}" width="{canvas_w-195}" height="{boundary_bottom-(ty+18):.0f}" rx="18" '
                f'fill="none" stroke="{COLORS["boundary"]}" stroke-width="1.5" stroke-dasharray="7,5"/>')
    svg.append(f'<text x="106" y="{ty+40:.0f}" font-size="13" font-weight="600" fill="#555">RouterNode(cfg, address)</text>')

    lane_points = {}

    for i, (name,) in enumerate(ports):
        center = centers[i]
        L = lanes_per_port[i]
        bh = block_height(L)
        block_top = center - bh / 2
        container_top = block_top - PAD
        container_bottom = block_top + bh + PAD

        src_grants = grants_by_src_port.get(i, [])   # this port as a source: [(vc, dst_idx, color), ...]
        dst_grants = grants_by_dst.get(i, [])        # this port as a destination: [(src_idx, vc, color), ...]
        in_color = src_grants[0][2] if src_grants else COLORS["ext"]
        # a destination fed by >1 concurrent grant is genuinely time-multiplexed past the
        # OutputPort -- leave its final external segment neutral rather than pick one color.
        out_color = dst_grants[0][2] if len(dst_grants) == 1 else COLORS["ext"]

        svg.append(f'<!-- ===== PORT {name} (row center={center:.0f}) ===== -->')
        svg.append(f'<text x="8" y="{center-10:.0f}" font-size="12" font-weight="600" '
                    f'fill="{in_color}">{esc(name)}</text>')

        if L > 1:
            svg.append(f'<line x1="{X_EXT_IN0}" y1="{center:.0f}" x2="{X_DEMUX0-4}" y2="{center:.0f}" '
                        f'stroke="{in_color}" stroke-width="{3 if src_grants else 2}" marker-end="url(#arrowGray)"/>')
            dh = 44
            svg.append(f'<rect x="{X_DEMUX0}" y="{center-dh/2:.0f}" width="{X_DEMUX1-X_DEMUX0}" height="{dh}" '
                        f'rx="7" fill="{COLORS["demux_fill"]}" stroke="{COLORS["demux_stroke"]}" stroke-width="1.2"/>')
            svg.append(f'<text x="{(X_DEMUX0+X_DEMUX1)/2:.0f}" y="{center-4:.0f}" text-anchor="middle" '
                        f'font-size="8" font-weight="600" fill="{COLORS["demux_stroke"]}">Stream</text>')
            svg.append(f'<text x="{(X_DEMUX0+X_DEMUX1)/2:.0f}" y="{center+7:.0f}" text-anchor="middle" '
                        f'font-size="8" font-weight="600" fill="{COLORS["demux_stroke"]}">Demux</text>')
        else:
            svg.append(f'<line x1="{X_EXT_IN0}" y1="{center:.0f}" x2="{X_FIFO0-4}" y2="{center:.0f}" '
                        f'stroke="{in_color}" stroke-width="{3 if src_grants else 2}" marker-end="url(#arrowGray)"/>')

        svg.append(f'<text x="{X_IP0+6}" y="{container_top-6:.0f}" font-size="11" font-style="italic" '
                    f'fill="#777">InputPort</text>')
        svg.append(f'<rect x="{X_IP0}" y="{container_top:.0f}" width="{X_IP1-X_IP0}" '
                    f'height="{container_bottom-container_top:.0f}" rx="10" fill="none" '
                    f'stroke="{COLORS["boundary"]}" stroke-width="1.2" stroke-dasharray="4,3"/>')

        for vc in range(L):
            unit_top = block_top + vc * (UNIT_H + LANE_GAP)
            box_y = unit_top + LABEL_H
            box_center_y = box_y + LANE_H / 2
            label = "vc0 (only lane)" if L == 1 else f"vc{vc}"
            lane_grant = grant_by_lane.get((i, vc))
            lane_color = lane_grant[1] if lane_grant else None

            svg.append(f'<text x="{X_FIFO0}" y="{unit_top+10:.0f}" font-size="10" font-weight="600" '
                        f'fill="#555">{esc(label)}</text>')

            if L > 1:
                demux_y = center
                fifo_ly = box_center_y
                fline_color = lane_color or "#9aa0a8"
                fline_w = 2.2 if lane_grant else 1.1
                svg.append(f'<path d="M{X_DEMUX1},{demux_y:.0f} C {X_FIFO0-16:.0f},{demux_y:.0f} '
                            f'{X_FIFO0-16:.0f},{fifo_ly:.0f} {X_FIFO0-2},{fifo_ly:.0f}" fill="none" '
                            f'stroke="{fline_color}" stroke-width="{fline_w}" marker-end="url(#arrowGray)"/>')

            fifo_stroke = lane_color or COLORS["fifo_stroke"]
            fifo_sw = 2.4 if lane_grant else 1.3
            svg.append(f'<rect x="{X_FIFO0}" y="{box_y:.0f}" width="{X_FIFO1-X_FIFO0}" height="{LANE_H}" rx="8" '
                        f'fill="{COLORS["fifo_fill"]}" stroke="{fifo_stroke}" stroke-width="{fifo_sw}"/>')
            fifo_cx = (X_FIFO0 + X_FIFO1) / 2
            svg.append(f'<text x="{fifo_cx:.0f}" y="{box_center_y-4:.0f}" text-anchor="middle" font-size="9.5" '
                        f'font-weight="600" fill="{COLORS["fifo_text"]}">StreamFifo</text>')
            svg.append(f'<text x="{fifo_cx:.0f}" y="{box_center_y+10:.0f}" text-anchor="middle" font-size="8" '
                        f'fill="{COLORS["fifo_text"]}">(depth=vcDepth)</text>')

            svg.append(f'<line x1="{X_ARR0}" y1="{box_center_y:.0f}" x2="{X_ARR1}" y2="{box_center_y:.0f}" '
                        f'stroke="{lane_color or "#555"}" stroke-width="{2.2 if lane_grant else 1.4}" '
                        f'marker-end="url(#arrowGray)"/>')

            fr_stroke = lane_color or COLORS["fr_stroke"]
            fr_sw = 2.4 if lane_grant else 1.3
            svg.append(f'<rect x="{X_FR0}" y="{box_y:.0f}" width="{X_FR1-X_FR0}" height="{LANE_H}" rx="8" '
                        f'fill="{COLORS["fr_fill"]}" stroke="{fr_stroke}" stroke-width="{fr_sw}"/>')
            fr_cx = (X_FR0 + X_FR1) / 2
            svg.append(f'<text x="{fr_cx:.0f}" y="{box_center_y-7:.0f}" text-anchor="middle" font-size="9.5" '
                        f'font-weight="600" fill="{COLORS["fr_text"]}">FlitRouter</text>')
            svg.append(f'<text x="{fr_cx:.0f}" y="{box_center_y+5:.0f}" text-anchor="middle" font-size="8" '
                        f'font-style="italic" fill="{COLORS["fr_text"]}">resolveDestPort</text>')
            svg.append(f'<text x="{fr_cx:.0f}" y="{box_center_y+16:.0f}" text-anchor="middle" font-size="7" '
                        f'fill="#666">reg: held to last flit</text>')

            lane_points[(i, vc)] = (X_FR1, box_center_y)

        svg.append(f'<rect x="{X_VIA0}" y="{center-VIA_H/2:.0f}" width="{VIA_W}" height="{VIA_H}" rx="10" '
                    f'fill="{COLORS["via_fill"]}" stroke="{COLORS["via_stroke"]}" stroke-width="1.3"/>')
        via_cx = (X_VIA0 + X_VIA1) / 2
        vtop = center - VIA_H / 2
        svg.append(f'<text x="{via_cx:.0f}" y="{vtop+22:.0f}" text-anchor="middle" font-size="13" '
                    f'font-weight="600" fill="{COLORS["via_text"]}">VirtualIdAllocator</text>')
        svg.append(f'<text x="{via_cx:.0f}" y="{vtop+38:.0f}" text-anchor="middle" font-size="10.5" '
                    f'fill="{COLORS["via_text"]}">output = {esc(name)}</text>')
        svg.append(f'<text x="{via_cx:.0f}" y="{vtop+53:.0f}" text-anchor="middle" font-size="10" '
                    f'font-style="italic" fill="{COLORS["via_text"]}">GrantTableCrossbar</text>')
        cand_lines = wrap_candidate(candidate_desc(ports, n, i, vc_count))
        for k, cl in enumerate(cand_lines):
            svg.append(f'<text x="{via_cx:.0f}" y="{vtop+71+k*13:.0f}" text-anchor="middle" font-size="9.5" '
                        f'fill="#6a4570">{esc(cl)}</text>')

        op_h = 60
        # assign each concurrently-granted incoming candidate its own destination VC slot
        dst_slot_color = {}
        for slot, (_, _, gcolor) in enumerate(dst_grants):
            dst_slot_color[slot] = gcolor
        for vc in range(vc_count):
            ly = center - (vc_count - 1) * 8 + vc * 16
            gcolor = dst_slot_color.get(vc)
            lcolor = gcolor or COLORS["via_stroke"]
            lw = 2.6 if gcolor else 1.4
            svg.append(f'<line x1="{X_VIA1}" y1="{ly:.0f}" x2="{X_OP0-2}" y2="{ly:.0f}" '
                        f'stroke="{lcolor}" stroke-width="{lw}" marker-end="url(#arrowGray)"/>')
        svg.append(f'<rect x="{X_OP0}" y="{center-op_h/2:.0f}" width="{OP_W}" height="{op_h}" rx="10" '
                    f'fill="{COLORS["op_fill"]}" stroke="{COLORS["op_stroke"]}" stroke-width="1.3"/>')
        op_cx = (X_OP0 + X_OP1) / 2
        svg.append(f'<text x="{op_cx:.0f}" y="{center-4:.0f}" text-anchor="middle" font-size="12" font-weight="600" '
                    f'fill="{COLORS["op_text"]}">OutputPort</text>')
        svg.append(f'<text x="{op_cx:.0f}" y="{center+13:.0f}" text-anchor="middle" font-size="9.5" '
                    f'fill="{COLORS["op_text"]}">StreamArbiter</text>')

        svg.append(f'<line x1="{X_OP1+2}" y1="{center:.0f}" x2="{ext_out_x}" y2="{center:.0f}" '
                    f'stroke="{out_color}" stroke-width="{3 if len(dst_grants) == 1 else 2}" '
                    f'marker-end="url(#arrowGray)"/>')
        svg.append(f'<text x="{ext_out_x:.0f}" y="{center-op_h/2-10:.0f}" text-anchor="end" font-size="12" '
                    f'font-weight="600" fill="{out_color}">{esc(name)}</text>')
        if len(dst_grants) > 1:
            svg.append(f'<text x="{ext_out_x:.0f}" y="{center+op_h/2+16:.0f}" text-anchor="end" font-size="8.5" '
                        f'font-style="italic" fill="#6b6f76">{len(dst_grants)} concurrent grants '
                        f'time-multiplexed here</text>')

        if name == "Local":
            ann_y = center + op_h / 2 + 24
            ann_w = 260
            svg.append(f'<rect x="{X_OP0}" y="{ann_y:.0f}" width="{ann_w}" height="58" rx="8" fill="none" '
                        f'stroke="{COLORS["op_stroke"]}" stroke-width="1" stroke-dasharray="3,3"/>')
            svg.append(f'<line x1="{op_cx:.0f}" y1="{center+op_h/2:.0f}" x2="{op_cx:.0f}" y2="{ann_y:.0f}" '
                        f'stroke="{COLORS["op_stroke"]}" stroke-width="1" stroke-dasharray="3,3"/>')
            ann_lines = [
                "Local port only: Topology.createNodes wraps",
                "io.outputs(Local) with an extra demux +",
                "lowerFirst.fragmentLock merge, stripping the",
                f"vc tag {chr(8212)} this step is outside RouterNode itself.",
            ]
            for k, ln in enumerate(ann_lines):
                svg.append(f'<text x="{X_OP0+10}" y="{ann_y+16+k*12:.0f}" font-size="9" fill="#7a5a20">{esc(ln)}</text>')

    svg.append('<!-- ===== CROSSBAR CANDIDATE LINES ===== -->')
    svg.append('<!-- generic rule: a port may target any output except its own canonical port, '
                'UNLESS that port is Local (canonical port 0), which is exempt and may target itself. -->')
    via_left_x = X_VIA0

    def allowed(src_idx, dst_idx):
        return True if src_idx == 0 else src_idx != dst_idx

    label_added = False
    for i in range(n):
        L = lanes_per_port[i]
        color = LINE_COLORS[i % len(LINE_COLORS)]
        for vc in range(L):
            x0, y0 = lane_points[(i, vc)]
            lane_grant = grant_by_lane.get((i, vc))
            granted_dst = lane_grant[0] if lane_grant else None
            for j in range(n):
                if not allowed(i, j):
                    continue
                yc = centers[j]
                is_hl = lane_grant is not None and j == granted_dst
                marker = f"url(#arrowB{i % len(LINE_COLORS)})" if is_hl else f"url(#arrowC{i % len(LINE_COLORS)})"
                if i == j:
                    peak_y = y0 + 70
                    path = (f"M{x0},{y0:.0f} C {x0+100:.0f},{peak_y:.0f} "
                            f"{via_left_x-100:.0f},{peak_y:.0f} {via_left_x},{yc:.0f}")
                    if is_hl:
                        svg.append(f'<path d="{path}" fill="none" stroke="{color}" stroke-width="3" '
                                   f'stroke-opacity="0.95" marker-end="{marker}"/>')
                    else:
                        svg.append(f'<path d="{path}" fill="none" stroke="{color}" stroke-width="1.6" '
                                   f'stroke-opacity="0.5" marker-end="{marker}"/>')
                    if not label_added:
                        svg.append(f'<text x="{(x0+via_left_x)/2:.0f}" y="{peak_y+16:.0f}" text-anchor="middle" '
                                    f'font-size="9" font-style="italic" fill="#6b6f76">Local{chr(183)}vc0 '
                                    f'{chr(8594)} Local-out: a locally-addressed packet loops back to '
                                    f'local delivery</text>')
                        label_added = True
                    continue
                midx = (x0 + via_left_x) / 2
                path = f"M{x0},{y0:.0f} C {midx:.0f},{y0:.0f} {midx:.0f},{yc:.0f} {via_left_x},{yc:.0f}"
                if is_hl:
                    svg.append(f'<path d="{path}" fill="none" stroke="{color}" stroke-width="3" '
                               f'stroke-opacity="0.95" marker-end="{marker}"/>')
                else:
                    svg.append(f'<path d="{path}" fill="none" stroke="{color}" stroke-width="1.3" '
                               f'stroke-opacity="0.3" marker-end="{marker}"/>')

    leg_y = legend_top
    svg.append('<!-- ===== LEGEND ===== -->')
    svg.append(f'<text x="106" y="{leg_y:.0f}" font-size="12" font-weight="600" fill="#1c2b40">Legend</text>')
    leg_y += 10
    svg.append(f'<rect x="106" y="{leg_y:.0f}" width="16" height="16" rx="3" fill="{COLORS["demux_fill"]}" '
                f'stroke="{COLORS["demux_stroke"]}"/>')
    svg.append(f'<text x="128" y="{leg_y+12:.0f}" font-size="11" fill="#333">StreamDemux {chr(8212)} splits the '
                f'physical link into vc lanes (skipped when a port has only 1 lane)</text>')
    leg_y += 20
    svg.append(f'<rect x="106" y="{leg_y:.0f}" width="16" height="16" rx="3" fill="{COLORS["fifo_fill"]}" '
                f'stroke="{COLORS["fifo_stroke"]}"/>')
    svg.append(f'<text x="128" y="{leg_y+12:.0f}" font-size="11" fill="#333">StreamFifo(depth=vcDepth) '
                f'{chr(8212)} per-VC buffering, one per lane</text>')
    leg_y += 20
    svg.append(f'<rect x="106" y="{leg_y:.0f}" width="16" height="16" rx="3" fill="{COLORS["fr_fill"]}" '
                f'stroke="{COLORS["fr_stroke"]}"/>')
    svg.append(f'<text x="128" y="{leg_y+12:.0f}" font-size="11" fill="#333">FlitRouter {chr(8212)} holds resolved '
                f'dest port until last flit (registered)</text>')
    leg_y -= 40
    svg.append(f'<rect x="640" y="{leg_y:.0f}" width="16" height="16" rx="3" fill="{COLORS["via_fill"]}" '
                f'stroke="{COLORS["via_stroke"]}"/>')
    svg.append(f'<text x="662" y="{leg_y+12:.0f}" font-size="11" fill="#333">VirtualIdAllocator {chr(8212)} one '
                f'GrantTableCrossbar per output port</text>')
    leg_y += 20
    svg.append(f'<rect x="640" y="{leg_y:.0f}" width="16" height="16" rx="3" fill="{COLORS["op_fill"]}" '
                f'stroke="{COLORS["op_stroke"]}"/>')
    svg.append(f'<text x="662" y="{leg_y+12:.0f}" font-size="11" fill="#333">OutputPort {chr(8212)} StreamArbiter '
                f'merges {vc_count} granted VC streams onto one physical link</text>')
    leg_y += 20
    svg.append(f'<line x1="640" y1="{leg_y+8:.0f}" x2="670" y2="{leg_y+8:.0f}" stroke="{LINE_COLORS[0]}" '
                f'stroke-width="3"/>')
    svg.append(f'<text x="676" y="{leg_y+12:.0f}" font-size="11" fill="#333">bold = a granted path (this example '
                f'set); faint = other reachable candidates, not currently granted</text>')
    leg_y += 26
    svg.append(f'<text x="106" y="{leg_y:.0f}" font-size="10.5" fill="#555">Line/label color identifies the '
                f'origin port, carried through every stage of that port\'s granted path {chr(8212)} note one input '
                f'below with two granted lanes, and one output with two concurrent grants sharing its VCs.</text>')
    leg_y += 16
    svg.append(f'<text x="106" y="{leg_y:.0f}" font-size="10.5" fill="#555">Candidate lines never target their own '
                f'canonical port (no U-turns) {chr(8212)} except Local, which may target Local (self-delivery).</text>')

    svg.append('</svg>')

    with open(out_path, "w") as f:
        f.write("\n".join(svg))
    return out_path


if __name__ == "__main__":
    which = sys.argv[1] if len(sys.argv) > 1 else "ring"
    if which == "ring":
        ports = [("Local",), ("ClockWise",), ("CounterClockWise",)]
        vc_count = 2
        title = "RouterNode internals"
        subtitle = [
            "Illustrative 3-port node (connectivityIn = connectivityOut = 3) — port names shown are "
            "Ring's (Local · ClockWise · CounterClockWise);",
            "a Mesh interior node has 5 ports instead — see the companion diagram. virtualChannels = 2.",
        ]
        out = "router-node.svg"
        # ClockWise has both its VCs granted (two concurrent flows from one input);
        # CounterClockWise-out receives two concurrent grants (from ClockWise and Local).
        grants = [
            ("ClockWise", 0, "Local"),
            ("ClockWise", 1, "CounterClockWise"),
            ("Local", 0, "CounterClockWise"),
            ("CounterClockWise", 0, "ClockWise"),
        ]
    else:
        ports = [("Local",), ("West",), ("East",), ("North",), ("South",)]
        vc_count = 2
        title = "RouterNode internals — Mesh interior node"
        subtitle = [
            "Illustrative 5-port node (connectivityIn = connectivityOut = 5) — canonical ports "
            "Local · West · East · North · South;",
            "a corner/edge Mesh node simply omits the ports it doesn't have. virtualChannels = 2. "
            "See the companion 3-port (Ring) diagram for the same structure at smaller scale.",
        ]
        out = "mesh-router-node.svg"
        # West has both its VCs granted (two concurrent flows from one input);
        # East-out receives two concurrent grants (from West and Local), filling both its VC lanes.
        grants = [
            ("West", 0, "Local"),
            ("West", 1, "East"),
            ("Local", 0, "East"),
            ("East", 1, "South"),
            ("South", 0, "North"),
            ("North", 1, "West"),
        ]

    path = build(ports, vc_count, title, subtitle, out, grants=grants)
    print("wrote", path)
