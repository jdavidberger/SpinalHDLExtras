# SpinalHDLExtras

A library of reusable [SpinalHDL](https://github.com/SpinalHDL/SpinalHDL) components, protocol
adapters, and verification infrastructure, plus the SpineX RISC-V SoC built on top of it. The
library source lives under `hw/spinal/spinalextras/lib`.

## Bus Fabric & Interconnect (`lib/bus`)

- **Protocol adapters** — `Wb2Axi4`, `Axi4ToPipelinedMemoryBus`, `PipelinedMemoryBusToWishbone`,
  bridging between Wishbone, AXI4, and SpinalHDL's `PipelinedMemoryBus`.
- **`GlobalBus`** — a trait for building up a bus (Wishbone/PMB/APB-style) by registering masters
  and slaves incrementally across a design, then materializing the interconnect and address map
  once elaboration is complete.
- **`MultiInterconnect` / `MultiInterconnectBusses`** — connects several masters to several slaves
  across differing bus types with a shared address decode, tagging support (`MultiInterconnectByTag`).
- **`lib/bus/general`** — a bus-agnostic slave/arbiter/decoder abstraction (`GeneralBusInterface`,
  `GeneralBusArbiter`, `GeneralBusDecoder`, `GeneralBusTimeout`) used to implement request/response
  arbitration once and reuse it under different concrete bus protocols.
- **`lib/bus/simple`** — lightweight `PipelinedMemoryBus` helpers: a slave factory, a width adapter,
  and an AXI4-to-PMB bridge.
- **`LMMI`** — Lattice Memory Master Interface support, including a peripheral mapper.
- **`WishboneStage`**, **`DirectBus`**, **`AXIBusLogger`** — pipeline staging, direct point-to-point
  buses, and AXI bus traffic logging.

## On-Chip Network / NoC (`lib/noc`)

A parameterizable virtual-channel network-on-chip generator:

- **`NoC` / `NoCBuilder` / `NocConfig`** — top-level NoC component and a builder API for wiring
  protocol endpoints onto network nodes with automatic address assignment.
- **`lib/noc/topology`** — interchangeable network topologies: `Mesh`, `Ring`, `Torus`, `Tree`, `Star`.
- **`lib/noc/virtualchannels`** — virtual-channel allocation and arbitration policy (round-robin,
  lowest-first) used to avoid protocol-level deadlock, including escape-VC handling for cyclic
  topologies (ring/torus).
- **`lib/noc/protocols`** — adapters that let existing bus/stream protocols (AXI4, PipelinedMemoryBus,
  generic data streams) ride over the NoC fabric via a common `ProtocolSpecification`.
- **`FlitRouter` / `RouterNode` / `Flit`** — the per-node routing/switching logic and flit format.
- **`NocDebug` / `NocGateCount`** — simulation-side deadlock/stall introspection and area estimation.

## Arbitration (`lib/misc/arbitration`)

Generic resource-grant primitives used by the NoC and elsewhere: `GrantTable` (candidate/lane grant
state with per-pairing allow masks), `GrantTableArbiter`, `GrantTableCrossbar`, `GrantTableStreamRouter`,
and `ChannelSelector`.

## Memory & FIFOs (`lib/memory`)

- **`HardwareMemory` / `WideHardwareMemory` / `StackedHardwareMemory` / `MemBackedHardwardMemory`** —
  abstractions over on-chip RAM primitives with configurable read/write ports, width stacking, and
  simulation-backed memory models.
- **FIFOs** — `MemoryFifo`, `MemoryBackedFifo`, `PipelinedMemoryBusFIFO`, `StridedAccessFIFO(Reader)`
  (strided/scatter access patterns), and `MemoryPoolFIFOs` (many logical FIFOs sharing one pooled
  backing memory, with a factory for allocating them).
- **`PriorityQueue`** — a hardware priority queue with configurable comparators.
- **`PipelinedMemoryBusBuffer` / `PipelinedMemoryBusMemory` / `StreamToBuffer`** — buffering and
  memory-mapped access helpers for `PipelinedMemoryBus`.
- **`MemoryRequirement`** — a declarative way to describe a component's memory needs so backing
  storage can be planned/allocated centrally.

## DMA (`lib/dma`)

Scatter-gather DMA engine building blocks: `ScatterGatherBase` (descriptor-based config/engine),
`MemoryToStream`, and `StreamToMemory` for moving data between system memory and `Stream` interfaces.

## Formal Verification Framework (`lib/formal`)

- **`HasFormalProperties` / `ComponentWithFormalProperties`** — a trait-based convention for
  attaching input assumptions and correctness assertions to any component/area, composable across
  a design hierarchy.
- **`FormalProperty` / `FormalData` / `FormalMasterSlave`** — property/state bookkeeping and
  master/slave-relative formal contracts.
- **`lib/formal/fillins`** — ready-made formal properties for SpinalHDL/library types that don't
  ship their own: `Stream`, `Fragment`, `Bundle`, `Axi4`, `Wishbone`, `PipelinedMemoryBus`,
  `StateMachine`, stream arbiters/forks, plus an `EquivalenceRegistry` for cross-checking equivalent
  implementations.
- **`lib/testing`** (`FormalTestSuite`) — reflection-based harness that discovers formal properties
  on a component and generates SymbiYosys-driven ScalaTest cases for it automatically.
- Broad formal coverage lives in `lib/tests/formal` — arbiters, crossbars, FIFOs, memories, bus
  adapters, stream width adapters, and a `SbyTest` SymbiYosys runner.

## Logging & Tracing (`lib/logging`)

- **`GlobalLogger` / `FlowLogger` / `SignalLogger`** — a simulation/hardware logging framework:
  components register signals/flows against a global logger, which streams captured data out
  (`FlowLoggerDataCapture`) to SQLite for offline analysis.
- **`PipelinedMemoryBusLogger` / `WishboneBusLogger`** — bus-transaction tracing for the two
  supported memory-mapped protocols.
- **`lib/logging/FlowLoggerUtils`** — Python-side helpers (`sqlite.scala`-driven schema, `yaml.scala`,
  `code.scala`) for decoding captured logger output.

## Clocking (`lib/clocking`)

`ClockSelection` (glitch-free clock muxing/selection), `PLLs` (a `PLL` trait plus a `SimulationPLL`
model), and `ClockUtils` for clock-domain-crossing helpers. `FixedFrequencyWithError.scala` and
`lib/misc/ClockMeasure.scala` / `ClockSpecification.scala` support frequency-with-tolerance
specifications and runtime frequency measurement.

## I/O, Blackboxes & Lattice Support (`lib/io`, `lib/blackbox`, `lib/lattice`)

- **`lib/io`** — technology-independent DDR I/O (`DDR`, `GenericDDR`) and `TristateBuffer` abstractions.
- **`lib/blackbox/lattice/lifcl`** — SpinalHDL blackbox wrappers for Lattice CrossLink-NX (LIFCL)
  primitives: PLL, oscillators (`OSCA`/`OSCD`), clock dividers/sync (`ECLKDIV`/`ECLKSYNC`/`DCS`),
  delay lines (`DELAYA`/`DELAYB`/`DLLDEL`/`DDRDLL`), block/distributed RAMs (`DP16K`, `SP512K`,
  `PDPSC16K`/`512K`, `DPSC512K`), JTAG (`JTAG`, `JTAGH19`), USB2/3 PHY (`USB23`), watchdog (`WDT`),
  GSR, and a soft MIPI D-PHY receiver (`dphy_rx`).
- **`lib/blackbox/memories`** — SPI flash model (`W25Q128JVxIM`).
- **`lib/blackbox/opencores`** — wrapper for the OpenCores `i2c_master_top`.
- **`lib/lattice`** — `IPX` (Lattice IP-core/`.ipx` metadata generation) and `LatticeMemories`
  (technology mapping for generic memory requests onto Lattice RAM primitives).

## MIPI CSI/DSI (`lib/mipi`)

A MIPI D-PHY/CSI-2 pixel pipeline: `MIPIConfig`/`MIPIIO` (PHY-level config and I/O), `MIPIPacketHeader`
(CSI-2 packet parsing), `byte2pixel` (byte-stream to pixel unpacking for various RAW/YUV formats),
`PixelFlow` (a `Flow`-based pixel bus with `PixelFlowMetaProvider`/`PixelFlow2Fragment` adapters).
`lib/blackbox/lattice/lifcl/lattice/MIPIToPixel.scala` wires the soft D-PHY receiver into this pipeline.

## Peripherals (`lib/peripherals`, `lib/soc/peripherals`)

- **`lib/peripherals/i2c`** — a full I2C master stack (`I2cMasterBitCtrl`, `I2cMasterByteCtrl`,
  `I2cMaster`) with a simulation model.
- **`lib/soc/peripherals`** — APB3-wrapped peripherals for SpineX: timer, UART (custom and
  `UART16550`), I2C, a system configuration/control block (`SpinexConfigCtrl`), and QSPI XIP flash
  (`XipFlashPlugin`).

## SpineX SoC (`lib/soc`)

A configurable RISC-V SoC (VexRiscv core) with a plugin-based peripheral architecture:

- **`SpineX` / `SpinexConfig` / `SpinexIPGen`** — the top-level SoC component, its configuration
  case class, and IP-packaging entry point.
- **`lib/soc/spinex/plugins`** — VexRiscv-style plugins that attach peripherals to the CPU/bus:
  `PeripheralBus`, `TimerPlugin`, `I2CPlugin`/`OpenCoresI2CPlugin`, `Uart16550CtrlPlugin`,
  `JTagPlugin`, `EventLoggerPlugin`, `IdentificationPlugin`.
- **`lib/soc/bus/WishbonePlugin`** — Wishbone bus attachment for the SoC.
- **`CSREventManager`** — a control/status-register-driven event manager.
- **`DeviceTree`** — builds a device tree blob describing the assembled SoC for firmware/Linux.
- **`SpinexSim`** — simulation entry point for the SoC.

## IP Generation & Build Tooling (`lib/ipgen`, `lib/impl`)

- **`IPGenerator`** — reads a YAML/JSON design description (via Jackson), generates the schema for
  it, and drives SpinalHDL generation of the described design (SpineX variants, MIPI byte2pixel,
  etc.) — the engine behind `mains/SpinalHDLExtrasIPGen.scala`.
- **`ImplementationSpecificFactory`** — selects vendor/technology-specific implementations of a
  component at elaboration time.
- **`Config` / `Constraints`** — shared `SpinalConfig` defaults (target device, reset style, etc.)
  and a constraint-collection API (clock definitions, max-skew, clock groups, false paths) that
  downstream synthesis flows consume.

## Debug (`lib/debug`)

`JtagLogger` and `StreamJtagInstrCtrl` — JTAG-based instrumentation/control and logging over the
debug port.

## General-Purpose Utilities (`lib/misc`)

Grab-bag of hardware and host-side helpers used throughout the library: stream utilities
(`StreamTools`, `StreamFifoExt`, `StreamFragmentWidthAdapter`, `StreamWidthAdapterWithOccupancy`,
`StreamJaggedData`, `FragmentFIFO`), clock/reset helpers (`AsyncToSyncReset`, `ClockMeasure`,
`ClockSpecification`, `DelayedSignal`), counters (`CounterTools`), CDC (`AsyncStream`), a global
signal registry (`GlobalSignals`), an auto-interconnect generator (`AutoInterconnect`) that wires up
unconnected component IOs by name, a bit-packing helper (`VariableWidthBits`), register-building
helpers (`RegisterTools`), rate limiting (`RateLimitFlow`), a byte/bit slip encoder (`SlipEncoder`),
math types (`Rational`, `Complex`), a maximal-length LFSR-based `RandomNumberGenerator`, and
`Obfuscater` (renames/obfuscates a generated netlist's signal names).

## Tests & Examples

- **`lib/tests`** — ScalaTest simulation testbenches for the components above (FIFOs, memory pools,
  bus adapters, PLL config, DDR, random-number generator, auto-interconnect, etc.).
- **`lib/tests/noc`** and **`lib/tests/formal`** — NoC-specific and formal-property-specific test suites.
- **`examples/`** — small standalone usage examples (e.g. `EventLoggerButtons`).
- **`mains/`** — CLI entry points: IP generation (`SpinalHDLExtrasIPGen`), a minimal SpineX build
  (`SpinexMinimal`), PLL generation for Lattice (`GenerateLatticePLL`), and a FIFO test case runner.
