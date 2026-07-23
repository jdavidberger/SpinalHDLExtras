# Network-on-Chip (`spinalextras.lib.noc`)

A configurable, topology-agnostic on-chip packet network with virtual-channel
flow control and wormhole routing. A `NoC` instance is a `Stream`-based fabric
of `RouterNode`s wired together according to a pluggable `Topology`
(Mesh, Torus, Ring, Tree, or Star); producers/consumers attach at any node's
local port and address each other by node number.

This document describes the architecture as implemented in
[`lib/noc/`](.) and its `topology`/`virtualchannels` sub-packages. Class and
field names below are verbatim from the source.

## Contents

- [Top-level component](#top-level-component)
- [Configuration](#configuration)
- [Addressing &amp; topologies](#addressing--topologies)
- [Flit and packet format](#flit-and-packet-format)
- [Router node internals](#router-node-internals)
- [Virtual-channel allocation](#virtual-channel-allocation)
- [Wormhole routing across hops](#wormhole-routing-across-hops)
- [Component relationships](#component-relationships)
- [Building a NoC](#building-a-noc)

---

## Top-level component

`NoC(cfg: NocConfig)` exposes one flit-level `Stream` port pair per node and
builds the entire interconnect internally via `cfg.topology.createNodes(this)`.
Port 0 of every node ("LOCAL") is always the one wired to the NoC's external
boundary; every other port connects to a neighbor per the topology's routing
tables.

```mermaid
flowchart LR
  classDef ext fill:#e8eef7,stroke:#4b6fa8,color:#1c2b40
  classDef fabric fill:#fdf3e3,stroke:#b8863b,color:#3a2c10

  In0["io.inputs(0)"]:::ext --> R0["RouterNode 0"]:::fabric
  In1["io.inputs(1)"]:::ext --> R1["RouterNode 1"]:::fabric
  In2["io.inputs(2)"]:::ext --> R2["RouterNode 2"]:::fabric
  In3["io.inputs(3)"]:::ext --> R3["RouterNode 3"]:::fabric

  R0 --> Out0["io.outputs(0)"]:::ext
  R1 --> Out1["io.outputs(1)"]:::ext
  R2 --> Out2["io.outputs(2)"]:::ext
  R3 --> Out3["io.outputs(3)"]:::ext

  subgraph Fabric["Interconnect wired by cfg.topology.createNodes(this)"]
    direction LR
    R0 <--> R1
    R1 <--> R2
    R0 <--> R2
    R2 <--> R3
  end
```

Two construction paths sit on top of `NoC`:

- **`NoC.apply(processors: Seq[NocProcessor], cfg)`** — resizes
  `cfg.topology` to `processors.size` and connects each `NocProcessor` to one
  node's `io.inputs(idx)` / `io.outputs(idx)`.
- **`NoCDesign`** — a builder: `addInput` / `addBitsInput` /
  `addOutput` / `addBitsOutput` accumulate endpoints, `.create()` sizes the
  topology and assembles the `NoC` (see [Building a NoC](#building-a-noc)).

`configureInputNode` / `configureOutputNode` handle packetizing raw
`Stream(Fragment(Bits))` traffic into flits and back — either directly, or via
a register-mapped CSR (`exit_node` register: `destination` + `vcid` fields)
for firmware-driven routing.

## Configuration

All behavior is parameterized by a single `NocConfig`:

| Field | Default | Meaning |
|---|---|---|
| `topology` | `new Mesh()` | Topology object — determines node count, per-node port count, addressing, and routing function |
| `dataWidth` | 32 | Bits per flit's `datum` field |
| `virtualChannels` | 2 | VC lanes multiplexed onto each physical link |
| `vcDepth` | 2 | Depth (in flits) of each per-VC input FIFO |
| `virtualChannelMode` | `Static` | `Static` (dest VC = source VC) or `Dynamic` (VC reassigned to any free lane) |
| `virtualChannelArbitrationPolicy` | `RoundRobin` | `RoundRobin` or `LowestFirst` — candidate arbitration inside `GrantTable` |

Derived: `headerApplicationBits = dataWidth − topology.addressSize`,
`virtualChannelBits = log2Up(virtualChannels)`.

## Addressing &amp; topologies

Every `Topology` provides `nodes`, `addressSize`, a routing function
`resolveDestPort(dest, curr)`, and `resolveNeighborAddress(address, port)` used
once at elaboration time to wire every link. Per-node port *count* varies with
position — a mesh corner has fewer ports than an interior node — via
`nodePortIndicesForCanonicalPorts(address)`.

```mermaid
flowchart TB
  classDef trait fill:#f2f2f2,stroke:#888,color:#222
  T["Topology (trait)<br/>resolveDestPort · resolveNeighborAddress · createNodes"]:::trait
  T --> Mesh["Mesh(x, y)<br/>dimension-order (X→Y) routing"]
  Mesh --> Torus["Torus(x, y)<br/>wraps X and Y;<br/>shortest-direction-around-ring per axis"]
  T --> Ring["Ring(n)<br/>shortest direction around one cycle"]
  T --> Tree["Tree(n, maxChildren)<br/>preorder DFS numbering,<br/>range-membership routing"]
  Tree -. "Star(n) = Tree(n, n)" .-> Star["Star(n)<br/>single hub, n−1 direct leaves"]
```

### Mesh — dimension-order (X-then-Y) routing

`Mesh(3, 2)`: address = `x * gridSize._2 + y`. `resolveDestPort` compares `x`
first (WEST/EAST), then `y` (NORTH/SOUTH); corner and edge nodes simply omit
the ports they don't need.

```mermaid
flowchart TB
  classDef node fill:#eef6ee,stroke:#4a8a55,color:#1a3320
  subgraph "y = 0"
    N0["node_0 (x0,y0)"]:::node
    N2["node_2 (x1,y0)"]:::node
    N4["node_4 (x2,y0)"]:::node
  end
  subgraph "y = 1"
    N1["node_1 (x0,y1)"]:::node
    N3["node_3 (x1,y1)"]:::node
    N5["node_5 (x2,y1)"]:::node
  end
  N0 <--> N2
  N2 <--> N4
  N1 <--> N3
  N3 <--> N5
  N0 <--> N1
  N2 <--> N3
  N4 <--> N5
```

### Torus — mesh with wraparound, ring routing per axis

Same grid as Mesh, but `createAddress` wraps modulo the grid size and every
node keeps all 5 ports (no edges). `resolveDestPort` calls the shared
`Ring.apply(delta, curr, size)` primitive independently per axis, picking
whichever direction is shorter around that axis's wraparound.

```mermaid
flowchart TB
  classDef node fill:#eef6ee,stroke:#4a8a55,color:#1a3320
  linkStyle default stroke:#4a8a55
  subgraph "y = 0"
    N0["node_0"]:::node
    N2["node_2"]:::node
    N4["node_4"]:::node
  end
  subgraph "y = 1"
    N1["node_1"]:::node
    N3["node_3"]:::node
    N5["node_5"]:::node
  end
  N0 <--> N2
  N2 <--> N4
  N1 <--> N3
  N3 <--> N5
  N0 <--> N1
  N2 <--> N3
  N4 <--> N5
  N0 -.wrap.-> N4
  N1 -.wrap.-> N5
```

*(illustrative — the wrap edges shown are the extra links a torus adds over
the equivalent mesh; `Torus(3,2)`'s Y-wrap is degenerate since Y only has 2
rows)*

### Ring — shortest direction around one cycle

`Ring(6)`: every node has exactly 3 ports — LOCAL, `ClockWise`,
`CounterClockWise`. `Ring.apply` compares `dest − curr` against `size/2` to
pick the shorter direction; this primitive is reused per-axis by `Torus`.

```mermaid
flowchart LR
  classDef node fill:#f5eef7,stroke:#8a4a97,color:#2f1834
  N0["node_0"]:::node --> N1["node_1"]:::node --> N2["node_2"]:::node --> N3["node_3"]:::node --> N4["node_4"]:::node --> N5["node_5"]:::node --> N0
```

### Tree / Star — preorder DFS addressing, range-membership routing

`Tree(totalNodes, maxChildren)` numbers nodes by preorder DFS, so each
subtree occupies a contiguous `[lo, hi]` address range and `resolveDestPort`
is a constant range compare per child (no divide/mod in hardware). Ports:
`LOCAL = 0`, `UP = 1` (absent at the root), `DOWN(i) = 2 + i` (one per actual
child). `Star(n)` is simply `Tree(n, n)` — a single hub whose "maxChildren"
covers every remaining node, so it collapses to one level.

```mermaid
flowchart TB
  classDef node fill:#eef2f8,stroke:#4a6a97,color:#182338
  N0["node_0 (root)"]:::node
  N0 --> N1["node_1<br/>subtree [1,5]"]:::node
  N0 --> N6["node_6<br/>subtree [6,9]"]:::node
  N1 --> N2["node_2<br/>subtree [2,3]"]:::node
  N1 --> N4["node_4<br/>subtree [4,5]"]:::node
  N2 --> N3["node_3 (leaf)"]:::node
  N4 --> N5["node_5 (leaf)"]:::node
  N6 --> N7["node_7 (leaf)"]:::node
  N6 --> N8["node_8 (leaf)"]:::node
  N6 --> N9["node_9 (leaf)"]:::node
```

*(illustrative partitioning for `Tree(10, 2)` — actual subtree sizes are
computed by `buildSubtree`, which splits remaining nodes evenly across
children, earlier children absorbing any remainder)*

```mermaid
flowchart LR
  classDef node fill:#f8f0e6,stroke:#a87a3b,color:#3a2810
  Hub["node_0 (hub)"]:::node
  Hub --- L1["node_1"]:::node
  Hub --- L2["node_2"]:::node
  Hub --- L3["node_3"]:::node
  Hub --- L4["node_4"]:::node
  Hub --- L5["node_5"]:::node
  Hub --- L6["node_6"]:::node
  Hub --- L7["node_7"]:::node
```

`Star(8) = Tree(8, 8)` — every leaf is a direct child of the hub.

On the wire, `Header.dest` carries `topology.addressSize` bits. For most
topologies this is the linear node index directly; **Mesh and Torus instead
pack `x` into the low bits and `y` into the high bits**
(`addressToRouteableAddress` / `routeableAddressToAddress`), so the on-wire
address differs from the Scala-side linear index used for wiring.

## Flit and packet format

The physical-link unit is a `Flit`, wrapped in `Fragment[Flit]` (adds a
`last` bit) traveling over a `Stream`. A **packet** is one or more
consecutive flits on the same `(port, vc)` up to `last`. The first flit's
`datum` is always a bit-packed `Header`, sized to exactly fill `dataWidth`
bits (`headerApplicationBits = dataWidth − addressSize`).

```mermaid
classDiagram
  class Flit {
    vc : UInt log2Up(virtualChannels)
    datum : Bits dataWidth
  }
  class Header {
    application : Bits dataWidth-addressSize
    dest : UInt addressSize
  }
  class FragmentFlit["Fragment~Flit~"] {
    flit : Flit
    last : Bool
  }
  FragmentFlit *-- Flit
  Header ..> Flit : bit-packed into datum\nof the first flit of a packet
```

```mermaid
flowchart LR
  classDef hdr fill:#fdf3e3,stroke:#b8863b,color:#3a2c10
  classDef pay fill:#e8eef7,stroke:#4b6fa8,color:#1c2b40
  F0["flit 0<br/>datum = Header{dest, application}<br/>last = 0"]:::hdr --> F1["flit 1<br/>datum = payload<br/>last = 0"]:::pay --> F2["flit 2<br/>datum = payload<br/>last = 0"]:::pay --> FN["flit N<br/>datum = payload<br/>last = 1"]:::pay
```

Internally, once a packet's output port has been resolved, `RouterNode`
tags subsequent flits with a `RoutedFlit { flit: Flit, routedNode: UInt }`
so the destination port doesn't need to be recomputed per flit.

## Router node internals

`RouterNode(cfg, address)` has `connectivityIn = connectivityOut =`
the number of canonical ports this node actually has. Each physical input
port owns one `StreamFifo` per VC (`InputPort`, depth `cfg.vcDepth`) — this
is the actual flow-control boundary (ordinary `Stream` ready/valid
backpressure, not an explicit credit protocol). A header-decode stage per
`(input port, vc)` resolves the destination port once per packet and hands
off to a shared `VirtualIdAllocator`, which arbitrates all contending
packets for output ports and destination VC lanes. Each physical output
port then merges its VC lanes with a priority arbiter (`OutputPort`).

```mermaid
flowchart TB
  classDef ext fill:#e8eef7,stroke:#4b6fa8,color:#1c2b40
  classDef stage fill:#fdf3e3,stroke:#b8863b,color:#3a2c10
  classDef alloc fill:#f5eef7,stroke:#8a4a97,color:#2f1834

  ExtIn["io.inputs(port)"]:::ext --> IP["InputPort<br/>StreamDemux by vc →<br/>per-vc StreamFifo(depth = vcDepth)"]:::stage
  IP --> HD["Header decode (per input × vc)<br/>idle: decode Header, resolveDestPort(dest, address)<br/>routed: tag flits as RoutedFlit{flit, routedNode}"]:::stage
  HD -->|"routedFlits(port)(vc)"| VIA["VirtualIdAllocator<br/>GrantTable + VcSelector + VcRouter<br/>Static or Dynamic mode"]:::alloc
  VIA -->|"allocatedFlits(port)(vc)"| OP["OutputPort<br/>StreamArbiter(lowerFirst, transactionLock)<br/>merges VC lanes onto one link"]:::stage
  OP --> ExtOut["io.outputs(port)"]:::ext
```

## Virtual-channel allocation

`VirtualIdAllocator` picks the mechanism per `cfg.virtualChannelMode`:

- **Static** — a packet's destination VC lane always equals its source VC
  id. Per `(output port, vc)`, a `GrantTable(connectivityIn, 1)` arbitrates
  only among the input ports contending for that one fixed lane.
- **Dynamic** — any `(input port, source vc)` may be granted *any* free
  destination VC lane on an output. Per output port, one
  `GrantTable(connectivityIn × virtualChannels, virtualChannels)` arbitrates
  the full candidate set; `retag` rewrites the winning flit's `vc` field.

```mermaid
flowchart LR
  classDef box fill:#f5eef7,stroke:#8a4a97,color:#2f1834
  subgraph Static["Static — per (output port, vc lane)"]
    direction LR
    S_in["contending input ports"] --> S_gt["GrantTable(connectivityIn, 1)"]:::box --> S_vr["VcRouter"]:::box --> S_out["allocatedFlits(out, vc)"]
  end
  subgraph Dynamic["Dynamic — per output port"]
    direction LR
    D_in["(input port × source vc)<br/>candidates = connectivityIn × vcCount"] --> D_gt["GrantTable(candidateCount, vcCount)"]:::box --> D_vr["VcRouter"]:::box --> D_out["allocatedFlits(out, any free vc)"]
  end
```

Inside `GrantTable`, a `candidateSelector` (`VcSelector`, policy =
`RoundRobin` or `LowestFirst`) picks a requester and a `laneSelector`
(always plain priority — lanes are interchangeable) picks a free lane;
when both agree, the grant is latched and held until the granted stream's
`last` fires (`io.release`), which is what makes this **wormhole routing**:
once granted, a packet's whole path is locked and later flits skip
re-arbitration.

```mermaid
flowchart TB
  classDef box fill:#f5eef7,stroke:#8a4a97,color:#2f1834
  Req["io.request(candidate)"] --> CS["candidateSelector: VcSelector<br/>(RoundRobin / LowestFirst)"]:::box
  Free["free lanes"] --> LS["laneSelector: VcSelector<br/>(priority, lanes are interchangeable)"]:::box
  CS --> M{"both chosen &amp; valid?"}
  LS --> M
  M -->|yes| G["grant(lane)(candidate) := True<br/>held until packet's last flit"]:::box
  Rel["io.release(vc) = allocatedFlits.lastFire"] --> Clr["clear grant row"]
```

At the physical output link, `OutputPort` merges the granted VC lanes with
`StreamArbiterFactory().lowerFirst.transactionLock` — lower-VC-id priority,
whole-packet locking, so flits from different packets never interleave on
the wire even though several VCs share one physical link.

## Wormhole routing across hops

```mermaid
sequenceDiagram
  participant Src as Source (external, node A)
  participant A as RouterNode A
  participant B as RouterNode B
  participant C as RouterNode C (dest)

  Src->>A: header flit (dest=C, vc=v), last=0
  A->>A: decode header · resolveDestPort → toward B
  A->>A: allocator grants (in←A, out→B, vc')
  A->>B: header flit forwarded
  B->>B: decode header · resolveDestPort → toward C
  B->>B: allocator grants (in←A-side, out→C, vc'')
  B->>C: header flit forwarded
  Note over A,C: latched vc register at each hop —<br/>later flits reuse the same granted path,<br/>no re-arbitration (wormhole)
  Src->>A: payload flit 1
  A->>B: payload flit 1
  B->>C: payload flit 1
  Src->>A: payload flit N, last=1
  A->>B: payload flit N, last=1
  B->>C: payload flit N, last=1
  Note over A,B,C: last=1 fires GrantTable.release at every hop —<br/>path torn down, lanes free for the next packet
```

Because each VC lane is buffered and arbitrated independently, one packet
stalled on a shared physical link does not head-of-line-block a different
packet occupying another VC — verified directly by the `manyToOne` /
`floodPackets` scenarios in `lib/tests/NoCConcurrence.scala`.

## Component relationships

```mermaid
classDiagram
  class NoC {
    cfg : NocConfig
    io.inputs : Stream~Fragment~Flit~~[]
    io.outputs : Stream~Fragment~Flit~~[]
    configureInputNode()
    configureOutputNode()
  }
  class NocConfig {
    topology : Topology
    dataWidth : Int
    virtualChannels : Int
    vcDepth : Int
    virtualChannelMode
    virtualChannelArbitrationPolicy
  }
  class Topology {
    <<trait>>
    nodes : Int
    resolveDestPort()
    resolveNeighborAddress()
    createNodes()
  }
  Topology <|-- Mesh
  Mesh <|-- Torus
  Topology <|-- Ring
  Topology <|-- Tree
  Tree <.. Star : Star(n) = Tree(n,n)

  class RouterNode {
    io.inputs[]
    io.outputs[]
  }
  class InputPort {
    per-vc StreamFifo(depth=vcDepth)
  }
  class OutputPort {
    StreamArbiter(lowerFirst, transactionLock)
  }
  class VirtualIdAllocator
  class GrantTable
  class VcSelector
  class VcRouter

  NoC --> NocConfig
  NoC --> Topology : uses
  NoC "1" --> "N" RouterNode : createNodes()
  RouterNode --> InputPort
  RouterNode --> OutputPort
  RouterNode --> VirtualIdAllocator
  VirtualIdAllocator --> GrantTable
  VirtualIdAllocator --> VcRouter
  GrantTable --> VcSelector
```

## Building a NoC

```mermaid
flowchart LR
  classDef step fill:#e8eef7,stroke:#4b6fa8,color:#1c2b40
  A["NoCDesign(cfg)"]:::step --> B["addInput / addBitsInput<br/>(per producer)"]:::step
  A --> C["addOutput / addBitsOutput<br/>(per consumer)"]:::step
  B --> D[".create()"]:::step
  C --> D
  D --> E["sizes topology to<br/>max(inputs.size, outputs.size)"]:::step
  E --> F["zips into TupleProcessor list<br/>(nulls pad unused sides)"]:::step
  F --> G["NoC.apply(processors, cfg)"]:::step
  G --> H["fully-wired NoC component"]:::step
```

For manually-built endpoints, `NoC.apply(processors: Seq[NocProcessor], cfg)`
skips the builder and connects a pre-existing list of
`Stream[Fragment[Flit]]` pairs directly.

### Test harnesses

- `lib/tests/NoCPathing.scala` — single-packet delivery correctness across
  every entry in `NocConfig.testConfigurations()` (all five topologies ×
  VC count × VC mode × arbitration policy).
- `lib/tests/NoCConcurrence.scala` — forks one sender per source node,
  reconstructs packets per `(node, vc)` on receive, and asserts both
  correctness *and* genuine overlap-in-flight (`overlapExists`) — including
  a `manyToOne` scenario that deliberately contends multiple senders on one
  destination's inbound link across distinct VCs to stress VC isolation.
