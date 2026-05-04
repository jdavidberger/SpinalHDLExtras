# Scatter-Gather DMA IP

A parameterisable scatter-gather DMA implemented in SpinalHDL, with a C driver.
Two channel directions are provided — `MemoryToStream` and `StreamToMemory` —
both inheriting from a shared abstract base, `ScatterGatherBase`.

---

## Channels

| Class | Direction | Stream port |
|---|---|---|
| `MemoryToStream` | Memory → RTL stream | `txStream` — master `Stream[Fragment[Bits]]` |
| `StreamToMemory` | RTL stream → Memory | `rxStream` — slave `Stream[Fragment[Bits]]` |

Both expose a **PipelinedMemoryBus slave** for CSR access and a
**PipelinedMemoryBus master** for system-memory access, plus an active-high
`irq` output.

---

## Compile-time parameters (`DmaConfig`)

| Parameter | Default | Description |
|---|---|---|
| `dataWidth` | 32 | Stream word width in bits; must be a power-of-2 number of bytes |
| `addrWidth` | 32 | Memory address width in bits |
| `lengthWidth` | 16 | Descriptor length field width in bits (max 64 kB per descriptor at default) |
| `burstLen` | 8 | Maximum outstanding memory read commands in flight |
| `fifoDepth` | 64 | Internal elastic FIFO depth in words |

---

## Stream interface

Both channels use `Stream[Fragment[Bits(dataWidth bits)]]`.

- `fragment` carries one data word
- `last` is asserted on the final word of each descriptor's transfer

For `StreamToMemory`, an early `last` on `rxStream` terminates the current
descriptor before its full `length` bytes are consumed — allowing variable-length
packet framing without wasting descriptor space.

---

## Descriptor format

Each descriptor occupies **four 32-bit words** in system memory.
Descriptors must be accessible to both the CPU and the DMA IP, and are
recommended to be 16-byte aligned.

```
Offset   Field     Bits    Description
──────   ───────   ─────   ────────────────────────────────────────────
+0x00    nextPtr   31:0    Physical address of the next descriptor.
                           Ignored when endOfChain flag is set.
+0x04    addr      31:0    Physical address of the data buffer.
+0x08    length    15:0    Buffer size in bytes.
         flags     31:24   Control flags (see below).
+0x0C    status    —       Written back by the IP on completion (read-only to software).
                           [0]    done (always 1 on completion)
                           [15:8] error code (0 = OK)
```

### Flags (bits 31:24 of word at +0x08)

| Bit | Name | Description |
|---|---|---|
| 0 | `interruptOnComplete` | Assert the IRQ output when this descriptor completes |
| 1 | `endOfChain` | Stop the engine after this descriptor and clear ENABLE |

---

## CSR register map

The control/status registers are accessed over the PipelinedMemoryBus slave
port. All offsets are byte offsets from the IP base address; registers are
32 bits wide.

| Offset | Name | Access | Description |
|---|---|---|---|
| `0x00` | `CTRL` | R/W | `[0]` enable — set to start the engine; cleared automatically on endOfChain. `[1]` soft-reset — clears BYTES_DONE, IRQ_PENDING, and STATUS_ERR. |
| `0x04` | `STATUS` | R | `[0]` running. `[1]` idle. `[11:4]` last error code. |
| `0x08` | `IRQ` | R/W1C | `[0]` interrupt pending. Write `1` to clear. |
| `0x0C` | `IRQ_EN` | R/W | `[0]` global interrupt enable. |
| `0x10` | `HEAD_PTR` | R/W | Physical address of the first descriptor in the chain. |
| `0x14` | `CUR_PTR` | R | Physical address of the descriptor currently being processed. |
| `0x18` | `BYTES_DONE` | R | Cumulative bytes transferred since the last enable. Cleared on soft-reset. |

### Startup sequence

```
1. Write HEAD_PTR  ← physical address of the first descriptor
2. Write IRQ_EN   ← 0x1  (optional; required for interrupt-driven use)
3. Write CTRL     ← 0x1  (enable)
4. Wait: either poll STATUS[1] (idle) or wait for an IRQ
5. Write IRQ      ← 0x1  to acknowledge the interrupt (W1C)
```

---

## Architecture

```
MemoryToStream
══════════════════════════════════════════════════════════════════════

                              ScatterGatherBase
  ┌────────────┐  ┌──────────────────────────────────────────────────────────────┐
  │  io.ctrl   │  │                                                              │
  │ (PMBus     ├─►│  BusSlaveFactory                                             │
  │  slave)    │  │  ┌─────────────────────────────────────────────────────┐     │
  └────────────┘  │  │ CTRL  STATUS  IRQ  IRQ_EN  HEAD_PTR  CUR_PTR        │     │
                  │  │ BYTES_DONE                                          │     │
                  │  └──────────────────────────┬──────────────────────────┘     │
                  │                             │ regHeadPtr / isRunning         │
                  │                    ┌────────▼────────┐                       │
                  │                    │   Fetch FSM     │                       │
                  │                    │  FETCH_W0/W1/W2 │◄── fetchBus.rsp       │
                  │                    │  PRESENT        ├──► fetchBus.cmd       │
                  │                    └────────┬────────┘                       │
                  │                             │ fetchedDesc                    │
                  │                    Stream[Descriptor]                        │
                  │                             │                                │
                  └─────────────────────────────┼────────────────────────────────┘
                                                │
                             MemoryToStream     │
  ┌─────────────────────────────────────────────┼──────────────────────────────┐
  │                                    ┌────────▼────────┐                     │
  │                                    │  Data-mover FSM │                     │
  │                                    │  WAIT_DESC      │                     │
  │                                    │  ISSUE_READS    │◄── dataBus.rsp      │
  │                                    │  DRAIN          ├──► dataBus.cmd      │
  │                                    │  DONE           ├──► completionIn     │
  │                                    └────────┬────────┘                     │
  │                                             │ push                         │
  │                                    ┌────────▼────────┐                     │
  │                                    │   StreamFifo    │                     │
  │                                    └────────┬────────┘                     │
  │                                             │ pop                          │
  │                                    ┌────────▼─────────┐                    │
  │  txStream ◄────────────────────────│ Stream[Fragment] │                    │
  │  (master)                          └──────────────────┘                    │
  └────────────────────────────────────────────────────────────────────────────┘

  completionIn ──► WriteBack FSM ──► writeBackBus.cmd (write desc+0x0C)
                   (ScatterGatherBase)     │
                        │ regIrqPending    │
                        ▼                  │
                     io.irq                │
                                           │
               ┌───────────────────────────┼──────────────┐
               │  PipelinedMemoryBusArbiter│              │
               │                           │              │
               │  fetchBus.cmd   ──────────┤              │
               │  writeBackBus.cmd ────────┼──► io.mem    │
               │  dataBus.cmd    ──────────┘   (master)   │
               └──────────────────────────────────────────┘


StreamToMemory
══════════════════════════════════════════════════════════════════════

                              ScatterGatherBase
  ┌────────────┐  ┌──────────────────────────────────────────────────────────────┐
  │  io.ctrl   │  │                                                              │
  │ (PMBus     ├─►│  BusSlaveFactory                                             │
  │  slave)    │  │  ┌─────────────────────────────────────────────────────┐     │
  └────────────┘  │  │ CTRL  STATUS  IRQ  IRQ_EN  HEAD_PTR  CUR_PTR        │     │
                  │  │ BYTES_DONE                                          │     │
                  │  └──────────────────────────┬──────────────────────────┘     │
                  │                             │ regHeadPtr / isRunning         │
                  │                    ┌────────▼────────┐                       │
                  │                    │   Fetch FSM     │                       │
                  │                    │  FETCH_W0/W1/W2 │◄── fetchBus.rsp       │
                  │                    │  PRESENT        ├──► fetchBus.cmd       │
                  │                    └────────┬────────┘                       │
                  │                             │ fetchedDesc                    │
                  │                    Stream[Descriptor]                        │
                  │                             │                                │
                  └─────────────────────────────┼────────────────────────────────┘
                                                │
                              StreamToMemory    │
  ┌─────────────────────────────────────────────┼───────────────────────────────┐
  │                                    ┌────────▼────────┐                      │
  │  rxStream ────────────────────┐    │  Data-mover FSM │                      │
  │  (slave)                      │    │  WAIT_DESC      │                      │
  │                               │    │  WRITE_DATA     ├──► dataBus.cmd       │
  │                            push    │  DONE           ├──► completionIn      │
  │                        ┌─────▼─┐   └────────▲────────┘                      │
  │                        │Stream │            │ pop                           │
  │                        │ FIFO  ├────────────┘                               │
  │                        └───────┘                                            │
  └─────────────────────────────────────────────────────────────────────────────┘

  completionIn ──► WriteBack FSM ──► writeBackBus.cmd (write desc+0x0C)
                   (ScatterGatherBase)     │
                        │ regIrqPending    │
                        ▼                  │
                     io.irq                │
                                           │
               ┌───────────────────────────┼──────────────┐
               │  PipelinedMemoryBusArbiter│              │
               │                           │              │
               │  fetchBus.cmd   ──────────┤              │
               │  writeBackBus.cmd ────────┼──► io.mem    │
               │  dataBus.cmd    ──────────┘   (master)   │
               └──────────────────────────────────────────┘

```

### Design notes

**Elastic FIFOs.** A `StreamFifo` sits between the memory bus and the user
stream on both channels. For `MemoryToStream` this absorbs read latency and
allows burst commands to be issued ahead of the consumer. For `StreamToMemory`
it absorbs incoming stream data while the bus is occupied with descriptor
fetches or write-backs.

**Completion write-back.** When the data-mover FSM finishes a descriptor it
pushes a `DescCompletion` onto a shared internal stream. The `writeBackFsm`
in the base class picks this up and issues a 32-bit write to
`descriptor + 0x0C`, stamping the done bit and error code into system memory.
The IRQ is raised only after this write is accepted by the arbiter, so
software polling `desc->status` will always observe the write-back before the
interrupt fires.

---

## File structure

```
├── sw/dma
│   ├── sg_dma.h             C driver — register definitions, descriptor struct,
│   │                        base and typed channel API
│   ├── sg_dma.c             C driver — implementation
│   └── sg_dma_example.c     Usage examples (IRQ-driven, polled, ring reuse)
│
└── hw/spinal/spinalextras/lib/dma/
    ├── ScatterGatherBase.scala   Abstract base: CSRs, fetch FSM, write-back FSM,
    │                             arbiter construction
    ├── MemoryToStream.scala      Memory → stream channel
    └── StreamToMemory.scala      Stream → memory channel
```

---

## C driver

The driver is split into a shared base (`sg_dma_t`) and two thin typed
wrappers (`sg_dma_m2s_t` for `MemoryToStream`, `sg_dma_s2m_t` for
`StreamToMemory`) that provide type-safe entry points without adding any
code or data overhead.

### Key types

```c
/* Descriptor — must be in DMA-visible memory, 16-byte aligned */
typedef struct {
    uint32_t next_ptr;  /* physical address of next descriptor, or 0  */
    uint32_t addr;      /* physical address of data buffer             */
    uint32_t word2;     /* flags[31:24] | length[15:0]                 */
    uint32_t status;    /* written by IP on completion; zero before use */
} sg_dma_desc_t;

/* Completion callback — invoked from sg_dma_poll() or sg_dma_irq_handler() */
typedef void (*sg_dma_completion_cb_t)(sg_dma_desc_t *desc,
                                       uint8_t        error_code,
                                       void          *user);
```

### Pluggable register accessors

`sg_dma_init()` accepts `reg_read` and `reg_write` function pointers. Pass
`NULL` for both to use the default bare-metal `volatile uint32_t *`
dereference. Supplying custom accessors allows the same driver to work under
OS MMIO wrappers or in a simulation model without any `#ifdef`.

### Completion model

`sg_dma_poll()` walks the descriptor array from `next_to_reap` forward,
invoking the completion callback for each descriptor whose `status` word has
`done=1` (the hardware write-back at `+0x0C`). It stops at the first
not-yet-done descriptor, so it is safe to call from a polling loop or a
timer tick.

`sg_dma_irq_handler()` clears the IRQ pending bit in hardware (W1C) before
harvesting, not after. This means a completion that arrives while the handler
is running is not lost — the IP will re-assert the IRQ line after the handler
returns if another descriptor has completed in the meantime.

### Typical usage — IRQ-driven MemoryToStream

```c
static sg_dma_desc_t descs[3];

static void on_done(sg_dma_desc_t *desc, uint8_t err, void *user) {
    /* called once per completed descriptor */
}

sg_dma_m2s_t ch;
sg_dma_m2s_init(&ch, MMIO_BASE, NULL, NULL, descs, 3, on_done, NULL);
sg_dma_reset(&ch.base);

uint32_t addrs[]   = { buf_a_phys, buf_b_phys, buf_c_phys };
uint16_t lengths[] = { 256, 128, 64 };
sg_dma_build_chain(descs, 3, addrs, lengths, /*irq_on_last=*/true);

sg_dma_m2s_submit(&ch, (uint32_t)&descs[0]);
/* IRQ fires when the last descriptor completes → on_done() is called */
```

### Typical usage — polled StreamToMemory

```c
static sg_dma_desc_t descs[1];
static uint8_t       buf[1024];

sg_dma_s2m_t ch;
sg_dma_s2m_init(&ch, MMIO_BASE, NULL, NULL, descs, 1, NULL, NULL);
sg_dma_reset(&ch.base);

uint32_t addrs[]   = { (uint32_t)buf };
uint16_t lengths[] = { sizeof(buf) };
sg_dma_build_chain(descs, 1, addrs, lengths, /*irq_on_last=*/false);

sg_dma_s2m_submit(&ch, (uint32_t)&descs[0]);
while (sg_dma_poll(&ch.base) == 0) { /* spin or yield */ }
```

### Re-arming a descriptor ring

To reuse a descriptor array for continuous reception, call
`sg_dma_build_chain()` again (which zeroes all `status` words), reset
`ch.base.next_to_reap = 0`, then call submit again. No re-initialisation of
the driver instance is required. See `sg_dma_example.c` for a complete
ring-reuse example.