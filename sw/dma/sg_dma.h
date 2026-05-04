/**
 * sg_dma.h — Scatter-Gather DMA driver (shared base)
 *
 * Covers both MemoryToStream (memory → RTL stream) and
 * StreamToMemory  (RTL stream → memory) IP instances.
 *
 * Hardware descriptor layout (all fields little-endian 32-bit words):
 *
 *   +0x00  next_ptr   – physical address of next descriptor (or 0)
 *   +0x04  addr       – data buffer physical address
 *   +0x08  word2      – flags[31:24] | reserved[23:16] | length[15:0]
 *   +0x0C  status     – written back by IP on completion (read-only to SW)
 *                        [0]    done
 *                        [15:8] error code
 *
 * CSR register map (byte offsets from IP base address):
 *
 *   0x00  CTRL        [0] enable   [1] soft-reset
 *   0x04  STATUS      [0] running  [1] idle  [11:4] last error code
 *   0x08  IRQ         [0] pending  (W1C: write 1 to clear)
 *   0x0C  IRQ_EN      [0] global interrupt enable
 *   0x10  HEAD_PTR    physical address of first descriptor
 *   0x14  CUR_PTR     address of descriptor in progress (read-only)
 *   0x18  BYTES_DONE  cumulative bytes transferred (read-only)
 */

#ifndef SG_DMA_H
#define SG_DMA_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ------------------------------------------------------------------ */
/*  Register offsets                                                    */
/* ------------------------------------------------------------------ */

#define SG_DMA_REG_CTRL        0x00u
#define SG_DMA_REG_STATUS      0x04u
#define SG_DMA_REG_IRQ         0x08u
#define SG_DMA_REG_IRQ_EN      0x0Cu
#define SG_DMA_REG_HEAD_PTR    0x10u
#define SG_DMA_REG_CUR_PTR     0x14u
#define SG_DMA_REG_BYTES_DONE  0x18u

/* ------------------------------------------------------------------ */
/*  CTRL register bits                                                  */
/* ------------------------------------------------------------------ */

#define SG_DMA_CTRL_ENABLE      (1u << 0)
#define SG_DMA_CTRL_SOFT_RESET  (1u << 1)

/* ------------------------------------------------------------------ */
/*  STATUS register bits / fields                                       */
/* ------------------------------------------------------------------ */

#define SG_DMA_STATUS_RUNNING   (1u << 0)
#define SG_DMA_STATUS_IDLE      (1u << 1)
#define SG_DMA_STATUS_ERR_SHIFT 4u
#define SG_DMA_STATUS_ERR_MASK  (0xFFu << SG_DMA_STATUS_ERR_SHIFT)

/* ------------------------------------------------------------------ */
/*  IRQ register bits (W1C on bit 0)                                    */
/* ------------------------------------------------------------------ */

#define SG_DMA_IRQ_PENDING      (1u << 0)

/* ------------------------------------------------------------------ */
/*  IRQ_EN register bits                                                */
/* ------------------------------------------------------------------ */

#define SG_DMA_IRQ_EN_GLOBAL    (1u << 0)

/* ------------------------------------------------------------------ */
/*  Descriptor word 2 packing                                           */
/*                                                                      */
/*  word2 = (flags << 24) | (length & 0xFFFF)                          */
/* ------------------------------------------------------------------ */

#define SG_DMA_DESC_FLAGS_SHIFT         24u
#define SG_DMA_DESC_FLAGS_IRQ_ON_DONE   (1u << 0)   /* flag bit 0 */
#define SG_DMA_DESC_FLAGS_END_OF_CHAIN  (1u << 1)   /* flag bit 1 */
#define SG_DMA_DESC_LENGTH_MASK         0x0000FFFFu

/* ------------------------------------------------------------------ */
/*  Descriptor write-back status word (at +0x0C, written by IP)        */
/* ------------------------------------------------------------------ */

#define SG_DMA_STATUS_DONE_BIT      (1u << 0)
#define SG_DMA_STATUS_ERRCODE_SHIFT 8u
#define SG_DMA_STATUS_ERRCODE_MASK  (0xFFu << SG_DMA_STATUS_ERRCODE_SHIFT)

/* ------------------------------------------------------------------ */
/*  Descriptor structure                                                */
/*                                                                      */
/*  Must be placed in memory visible to both the CPU and the DMA IP.   */
/*  Alignment to 16 bytes is recommended so that no descriptor         */
/*  straddles a cache line boundary.                                    */
/* ------------------------------------------------------------------ */

typedef struct sg_dma_desc {
    uint32_t next_ptr;  /* +0x00: physical addr of next descriptor, 0 = none */
    uint32_t addr;      /* +0x04: physical addr of data buffer               */
    uint32_t word2;     /* +0x08: flags[31:24] | length[15:0]                */
    uint32_t status;    /* +0x0C: written back by IP; zero before submission */
} __attribute__((aligned(16))) sg_dma_desc_t;

/* Convenience: build word2 from separate fields */
static inline uint32_t sg_dma_desc_word2(uint8_t flags, uint16_t length)
{
    return ((uint32_t)flags << SG_DMA_DESC_FLAGS_SHIFT) |
           ((uint32_t)length & SG_DMA_DESC_LENGTH_MASK);
}

/* Read the error code from a completed descriptor's status word */
static inline uint8_t sg_dma_desc_error(const sg_dma_desc_t *desc)
{
    return (uint8_t)((desc->status & SG_DMA_STATUS_ERRCODE_MASK)
                     >> SG_DMA_STATUS_ERRCODE_SHIFT);
}

/* True once the IP has written the completion status into this descriptor */
static inline bool sg_dma_desc_is_done(const sg_dma_desc_t *desc)
{
    return (desc->status & SG_DMA_STATUS_DONE_BIT) != 0u;
}

/* ------------------------------------------------------------------ */
/*  Completion callback                                                 */
/*                                                                      */
/*  Called from sg_dma_irq_handler() or sg_dma_poll() when one or      */
/*  more descriptors complete.                                          */
/*                                                                      */
/*  @param desc         the completed descriptor                        */
/*  @param error_code   0 = success, non-zero = IP error code          */
/*  @param user         opaque pointer supplied at driver init          */
/* ------------------------------------------------------------------ */

typedef void (*sg_dma_completion_cb_t)(sg_dma_desc_t *desc,
                                       uint8_t        error_code,
                                       void          *user);

/* ------------------------------------------------------------------ */
/*  Register accessor type                                              */
/*                                                                      */
/*  The driver is hardware-independent by design: callers supply read   */
/*  and write functions so it works on bare-metal (direct pointer       */
/*  dereference) and on platforms with MMIO access wrappers alike.     */
/* ------------------------------------------------------------------ */

typedef uint32_t (*sg_dma_reg_read_fn_t) (uintptr_t base, uint32_t offset);
typedef void     (*sg_dma_reg_write_fn_t)(uintptr_t base, uint32_t offset,
                                          uint32_t value);

/* ------------------------------------------------------------------ */
/*  Base driver instance                                                */
/* ------------------------------------------------------------------ */

typedef struct sg_dma {
    uintptr_t             base;         /* MMIO base address of the IP       */
    sg_dma_reg_read_fn_t  reg_read;     /* register read accessor            */
    sg_dma_reg_write_fn_t reg_write;    /* register write accessor           */

    sg_dma_desc_t        *descs;        /* pointer to descriptor array       */
    uint32_t              desc_count;   /* number of entries in array        */

    /* Completion tracking: index of the next descriptor we expect the
     * IP to complete.  Advances as we harvest finished descriptors in
     * poll / IRQ handler.                                                */
    uint32_t              next_to_reap;

    sg_dma_completion_cb_t completion_cb;   /* may be NULL for poll-only use */
    void                  *completion_user; /* opaque, forwarded to callback  */
} sg_dma_t;

/* ------------------------------------------------------------------ */
/*  Base driver API                                                     */
/* ------------------------------------------------------------------ */

/**
 * sg_dma_init() — initialise a driver instance.
 *
 * Does not touch hardware; call sg_dma_reset() after this if the IP
 * may be in an unknown state.
 *
 * @param dma           driver instance to initialise
 * @param base          MMIO base address of the IP
 * @param reg_read      register read accessor (NULL = use default)
 * @param reg_write     register write accessor (NULL = use default)
 * @param descs         caller-allocated descriptor array (must be DMA-visible)
 * @param desc_count    number of entries in descs[]
 * @param cb            completion callback, or NULL for poll-only use
 * @param user          opaque value forwarded to the callback
 */
void sg_dma_init(sg_dma_t             *dma,
                 uintptr_t             base,
                 sg_dma_reg_read_fn_t  reg_read,
                 sg_dma_reg_write_fn_t reg_write,
                 sg_dma_desc_t        *descs,
                 uint32_t              desc_count,
                 sg_dma_completion_cb_t cb,
                 void                 *user);

/**
 * sg_dma_reset() — issue a soft-reset and wait for IDLE.
 *
 * Clears BYTES_DONE, IRQ_PENDING, and STATUS_ERR in hardware.
 * Resets next_to_reap to 0 in the driver instance.
 */
void sg_dma_reset(sg_dma_t *dma);

/**
 * sg_dma_build_chain() — populate a contiguous range of descriptors.
 *
 * Fills descs[0..count-1] as a linked list.  The last descriptor has
 * END_OF_CHAIN set (and IRQ_ON_DONE if irq_on_last is true).
 * All status words are cleared to 0 before the chain is submitted.
 *
 * @param descs         descriptor array (must be DMA-visible)
 * @param count         number of scatter-gather entries
 * @param addrs         array of `count` buffer physical addresses
 * @param lengths       array of `count` transfer lengths in bytes
 * @param irq_on_last   if true, request an IRQ when the last descriptor
 *                      completes; intermediate descriptors do not request IRQs
 */
void sg_dma_build_chain(sg_dma_desc_t *descs,
                        uint32_t       count,
                        const uint32_t addrs[],
                        const uint16_t lengths[],
                        bool           irq_on_last);

/**
 * sg_dma_start() — point the IP at the descriptor chain and enable it.
 *
 * Enables the global interrupt in hardware if a completion callback was
 * supplied at init time.
 *
 * @param dma           driver instance
 * @param head_phys     physical address of the first descriptor in the chain
 */
void sg_dma_start(sg_dma_t *dma, uint32_t head_phys);

/**
 * sg_dma_stop() — clear the enable bit.
 *
 * The IP finishes its current descriptor before stopping; it does not
 * abort a transfer mid-word.
 */
void sg_dma_stop(sg_dma_t *dma);

/**
 * sg_dma_is_idle() — return true if the STATUS register shows IDLE.
 */
bool sg_dma_is_idle(const sg_dma_t *dma);

/**
 * sg_dma_bytes_done() — read the cumulative byte counter.
 */
uint32_t sg_dma_bytes_done(const sg_dma_t *dma);

/**
 * sg_dma_poll() — harvest completed descriptors without an IRQ.
 *
 * Walks descriptors from next_to_reap forward, calling the completion
 * callback for each one whose status word shows done=1.  Stops at the
 * first descriptor that is not yet done.
 *
 * Safe to call from a polling loop or a timer tick.
 *
 * @return number of descriptors reaped this call
 */
uint32_t sg_dma_poll(sg_dma_t *dma);

/**
 * sg_dma_irq_handler() — call from the platform IRQ dispatcher.
 *
 * Clears the pending IRQ in hardware (W1C), then calls sg_dma_poll()
 * to harvest and report any completed descriptors.
 *
 * @return number of descriptors reaped
 */
uint32_t sg_dma_irq_handler(sg_dma_t *dma);

/**
 * sg_dma_irq_enable() / sg_dma_irq_disable() — gate the IP's IRQ line.
 */
void sg_dma_irq_enable(sg_dma_t *dma);
void sg_dma_irq_disable(sg_dma_t *dma);

/* ------------------------------------------------------------------ */
/*  Default MMIO accessors (bare-metal direct pointer dereference)     */
/*                                                                      */
/*  Pass NULL for reg_read / reg_write in sg_dma_init() to use these.  */
/* ------------------------------------------------------------------ */

uint32_t sg_dma_default_read (uintptr_t base, uint32_t offset);
void     sg_dma_default_write(uintptr_t base, uint32_t offset, uint32_t value);

/* ------------------------------------------------------------------ */
/*  Typed channel wrappers                                              */
/* ------------------------------------------------------------------ */

/**
 * MemoryToStream channel  (IP reads from memory, drives an RTL stream)
 *
 * Use sg_dma_m2s_submit() instead of sg_dma_start() to get a
 * type-checked entry point that documents the data direction.
 */
typedef struct {
    sg_dma_t base;
} sg_dma_m2s_t;

static inline void sg_dma_m2s_init(sg_dma_m2s_t          *ch,
                                    uintptr_t              mmio_base,
                                    sg_dma_reg_read_fn_t   reg_read,
                                    sg_dma_reg_write_fn_t  reg_write,
                                    sg_dma_desc_t         *descs,
                                    uint32_t               desc_count,
                                    sg_dma_completion_cb_t cb,
                                    void                  *user)
{
    sg_dma_init(&ch->base, mmio_base, reg_read, reg_write,
                descs, desc_count, cb, user);
}

/**
 * Submit a pre-built descriptor chain to a MemoryToStream channel.
 * The descriptors should point at source buffers already filled
 * with the data to be sent to the stream.
 */
static inline void sg_dma_m2s_submit(sg_dma_m2s_t *ch, uint32_t head_phys)
{
    sg_dma_start(&ch->base, head_phys);
}

/**
 * StreamToMemory channel  (IP accepts an RTL stream, writes to memory)
 *
 * Descriptors should point at destination buffers large enough to hold
 * the incoming stream data.
 */
typedef struct {
    sg_dma_t base;
} sg_dma_s2m_t;

static inline void sg_dma_s2m_init(sg_dma_s2m_t          *ch,
                                    uintptr_t              mmio_base,
                                    sg_dma_reg_read_fn_t   reg_read,
                                    sg_dma_reg_write_fn_t  reg_write,
                                    sg_dma_desc_t         *descs,
                                    uint32_t               desc_count,
                                    sg_dma_completion_cb_t cb,
                                    void                  *user)
{
    sg_dma_init(&ch->base, mmio_base, reg_read, reg_write,
                descs, desc_count, cb, user);
}

/**
 * Submit a pre-built descriptor chain to a StreamToMemory channel.
 * The descriptors should point at destination buffers into which
 * the IP will write received stream data.
 */
static inline void sg_dma_s2m_submit(sg_dma_s2m_t *ch, uint32_t head_phys)
{
    sg_dma_start(&ch->base, head_phys);
}

#ifdef __cplusplus
}
#endif

#endif /* SG_DMA_H */
