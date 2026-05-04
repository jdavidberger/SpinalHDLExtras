/**
 * sg_dma.c — Scatter-Gather DMA driver (shared base implementation)
 */

#include "sg_dma.h"

#include <string.h>  /* memset */

/* ------------------------------------------------------------------ */
/*  Internal helpers                                                    */
/* ------------------------------------------------------------------ */

static inline uint32_t _read(const sg_dma_t *dma, uint32_t offset)
{
    return dma->reg_read(dma->base, offset);
}

static inline void _write(const sg_dma_t *dma, uint32_t offset, uint32_t val)
{
    dma->reg_write(dma->base, offset, val);
}

/* ------------------------------------------------------------------ */
/*  Default MMIO accessors                                              */
/* ------------------------------------------------------------------ */

uint32_t sg_dma_default_read(uintptr_t base, uint32_t offset)
{
    return *(volatile uint32_t *)(base + offset);
}

void sg_dma_default_write(uintptr_t base, uint32_t offset, uint32_t value)
{
    *(volatile uint32_t *)(base + offset) = value;
}

/* ------------------------------------------------------------------ */
/*  Initialisation                                                      */
/* ------------------------------------------------------------------ */

void sg_dma_init(sg_dma_t              *dma,
                 uintptr_t              base,
                 sg_dma_reg_read_fn_t   reg_read,
                 sg_dma_reg_write_fn_t  reg_write,
                 sg_dma_desc_t         *descs,
                 uint32_t               desc_count,
                 sg_dma_completion_cb_t cb,
                 void                  *user)
{
    memset(dma, 0, sizeof(*dma));

    dma->base       = base;
    dma->reg_read   = reg_read  ? reg_read  : sg_dma_default_read;
    dma->reg_write  = reg_write ? reg_write : sg_dma_default_write;
    dma->descs      = descs;
    dma->desc_count = desc_count;
    dma->next_to_reap   = 0;
    dma->completion_cb   = cb;
    dma->completion_user = user;
}

/* ------------------------------------------------------------------ */
/*  Reset                                                               */
/* ------------------------------------------------------------------ */

void sg_dma_reset(sg_dma_t *dma)
{
    /* Assert soft-reset.  The hardware clears BYTES_DONE, IRQ_PENDING,
     * and STATUS_ERR on the rising edge of this bit (onWrite(0x00) in RTL). */
    _write(dma, SG_DMA_REG_CTRL, SG_DMA_CTRL_SOFT_RESET);

    /* De-assert soft-reset.  The IP returns to IDLE. */
    _write(dma, SG_DMA_REG_CTRL, 0u);

    /* Spin until the STATUS register confirms IDLE.
     * On a real system you may want a timeout here. */
    while (!sg_dma_is_idle(dma)) { /* busy-wait */ }

    dma->next_to_reap = 0;
}

/* ------------------------------------------------------------------ */
/*  Descriptor chain builder                                            */
/* ------------------------------------------------------------------ */

void sg_dma_build_chain(sg_dma_desc_t *descs,
                        uint32_t       count,
                        const uint32_t addrs[],
                        const uint16_t lengths[],
                        bool           irq_on_last)
{
    uint32_t i;

    for (i = 0; i < count; i++) {
        uint8_t  flags = 0u;
        bool     is_last = (i == count - 1u);

        /* Only the final descriptor ends the chain */
        if (is_last) {
            flags |= SG_DMA_DESC_FLAGS_END_OF_CHAIN;
            if (irq_on_last) {
                flags |= SG_DMA_DESC_FLAGS_IRQ_ON_DONE;
            }
        }

        /* Link to the next descriptor, or 0 for the last one.
         * The IP ignores next_ptr when END_OF_CHAIN is set, but we
         * write 0 anyway to avoid stale pointers if the descriptor
         * array is reused. */
        descs[i].next_ptr = is_last
                            ? 0u
                            : (uint32_t)(uintptr_t)(&descs[i + 1]);
        descs[i].addr     = addrs[i];
        descs[i].word2    = sg_dma_desc_word2(flags, lengths[i]);

        /* Clear the write-back word so sg_dma_desc_is_done() returns
         * false before the IP touches it. */
        descs[i].status   = 0u;
    }
}

/* ------------------------------------------------------------------ */
/*  Start / stop                                                        */
/* ------------------------------------------------------------------ */

void sg_dma_start(sg_dma_t *dma, uint32_t head_phys)
{
    /* Program the descriptor chain head */
    _write(dma, SG_DMA_REG_HEAD_PTR, head_phys);

    /* Enable the hardware IRQ line if the caller registered a callback */
    if (dma->completion_cb != NULL) {
        sg_dma_irq_enable(dma);
    }

    /* Set ENABLE — the IP starts fetching from HEAD_PTR immediately */
    _write(dma, SG_DMA_REG_CTRL, SG_DMA_CTRL_ENABLE);
}

void sg_dma_stop(sg_dma_t *dma)
{
    /* Clear ENABLE.  The IP finishes the current descriptor then halts. */
    _write(dma, SG_DMA_REG_CTRL, 0u);
}

/* ------------------------------------------------------------------ */
/*  Status queries                                                      */
/* ------------------------------------------------------------------ */

bool sg_dma_is_idle(const sg_dma_t *dma)
{
    return (_read(dma, SG_DMA_REG_STATUS) & SG_DMA_STATUS_IDLE) != 0u;
}

uint32_t sg_dma_bytes_done(const sg_dma_t *dma)
{
    return _read(dma, SG_DMA_REG_BYTES_DONE);
}

/* ------------------------------------------------------------------ */
/*  Completion harvesting                                               */
/*                                                                      */
/*  The IP writes the status word at descriptor+0x0C when it finishes  */
/*  a descriptor.  We scan forward from next_to_reap and invoke the    */
/*  callback for every descriptor with done=1.                          */
/*                                                                      */
/*  Important memory ordering note: on architectures with weakly-       */
/*  ordered memory (ARM, RISC-V) you must ensure the CPU observes the   */
/*  IP's write to desc->status before reading it.  Insert the          */
/*  appropriate data memory barrier before the status read if your      */
/*  platform requires it (e.g. __DMB() on ARM Cortex-M/A).             */
/* ------------------------------------------------------------------ */

uint32_t sg_dma_poll(sg_dma_t *dma)
{
    uint32_t reaped = 0u;

    while (dma->next_to_reap < dma->desc_count) {
        sg_dma_desc_t *desc = &dma->descs[dma->next_to_reap];

        /* Data memory barrier: ensure we see the IP's write to status
         * before we test it.  Expand this to a platform-specific barrier
         * (e.g. __DMB(); or mb();) if needed. */

        if (!sg_dma_desc_is_done(desc)) {
            /* Not yet complete — stop harvesting */
            break;
        }

        if (dma->completion_cb != NULL) {
            dma->completion_cb(desc, sg_dma_desc_error(desc),
                               dma->completion_user);
        }

        dma->next_to_reap++;
        reaped++;
    }

    return reaped;
}

/* ------------------------------------------------------------------ */
/*  IRQ handler                                                         */
/* ------------------------------------------------------------------ */

uint32_t sg_dma_irq_handler(sg_dma_t *dma)
{
    /* Clear the pending IRQ flag in hardware (W1C) before harvesting
     * so that any completion that arrives while we are in this handler
     * is not silently dropped: the IP will re-assert the IRQ line after
     * we return if another descriptor completes. */
    _write(dma, SG_DMA_REG_IRQ, SG_DMA_IRQ_PENDING);

    return sg_dma_poll(dma);
}

/* ------------------------------------------------------------------ */
/*  IRQ gate                                                            */
/* ------------------------------------------------------------------ */

void sg_dma_irq_enable(sg_dma_t *dma)
{
    _write(dma, SG_DMA_REG_IRQ_EN, SG_DMA_IRQ_EN_GLOBAL);
}

void sg_dma_irq_disable(sg_dma_t *dma)
{
    _write(dma, SG_DMA_REG_IRQ_EN, 0u);
}
