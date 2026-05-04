/**
 * sg_dma_example.c — usage examples for both DMA channel directions
 *
 * This file is illustrative only; it is not part of the driver itself.
 */

#include "sg_dma.h"
#include <stdio.h>

/* ------------------------------------------------------------------ */
/*  Example 1: MemoryToStream, IRQ-driven, 3-element scatter gather    */
/*                                                                      */
/*  Scenario: we want to read three separate memory buffers and send   */
/*  them out as a single stream.  We use an IRQ on the final           */
/*  descriptor to know when the transfer is done.                      */
/* ------------------------------------------------------------------ */

/* Descriptor storage — must be in memory visible to the DMA IP */
static sg_dma_desc_t m2s_descs[3];

/* Data buffers (normally populated with real data before submission) */
static uint8_t m2s_buf_a[256];
static uint8_t m2s_buf_b[128];
static uint8_t m2s_buf_c[64];

static volatile bool m2s_done = false;

static void m2s_completion(sg_dma_desc_t *desc, uint8_t error_code, void *user)
{
    (void)user;
    if (error_code != 0) {
        printf("[m2s] descriptor at %p failed with error 0x%02X\n",
               (void *)desc, error_code);
    } else {
        printf("[m2s] descriptor at %p complete\n", (void *)desc);
    }

    /* If this is the last descriptor in the chain the engine has stopped */
    if (desc->word2 & (SG_DMA_DESC_FLAGS_END_OF_CHAIN << SG_DMA_DESC_FLAGS_SHIFT)) {
        m2s_done = true;
    }
}

void example_memory_to_stream(uintptr_t ip_mmio_base)
{
    sg_dma_m2s_t ch;

    sg_dma_m2s_init(&ch,
                    ip_mmio_base,
                    NULL, NULL,          /* use default MMIO accessors */
                    m2s_descs, 3,
                    m2s_completion, NULL);

    sg_dma_reset(&ch.base);

    /* Describe the three source buffers */
    const uint32_t addrs[3] = {
        (uint32_t)(uintptr_t)m2s_buf_a,
        (uint32_t)(uintptr_t)m2s_buf_b,
        (uint32_t)(uintptr_t)m2s_buf_c,
    };
    const uint16_t lengths[3] = { 256, 128, 64 };

    sg_dma_build_chain(m2s_descs, 3, addrs, lengths, /*irq_on_last=*/true);

    m2s_done = false;
    sg_dma_m2s_submit(&ch, (uint32_t)(uintptr_t)&m2s_descs[0]);

    /* Wait for the IRQ-driven callback to fire (in a real system this
     * would be a semaphore pend or event flag wait, not a spin) */
    while (!m2s_done) { /* yield / sleep */ }

    printf("[m2s] transfer complete, bytes moved: %u\n",
           sg_dma_bytes_done(&ch.base));
}

/* ------------------------------------------------------------------ */
/*  Example 2: StreamToMemory, polled, single descriptor               */
/*                                                                      */
/*  Scenario: receive up to 1 kB from an incoming stream into a        */
/*  single contiguous buffer, using polling rather than an IRQ.        */
/* ------------------------------------------------------------------ */

static sg_dma_desc_t s2m_descs[1];
static uint8_t       s2m_buf[1024];

void example_stream_to_memory(uintptr_t ip_mmio_base)
{
    sg_dma_s2m_t ch;

    /* No callback: we will use sg_dma_poll() instead */
    sg_dma_s2m_init(&ch,
                    ip_mmio_base,
                    NULL, NULL,
                    s2m_descs, 1,
                    NULL, NULL);

    sg_dma_reset(&ch.base);

    const uint32_t addrs[1]   = { (uint32_t)(uintptr_t)s2m_buf };
    const uint16_t lengths[1] = { sizeof(s2m_buf) };

    /* Single descriptor, no IRQ requested */
    sg_dma_build_chain(s2m_descs, 1, addrs, lengths, /*irq_on_last=*/false);

    sg_dma_s2m_submit(&ch, (uint32_t)(uintptr_t)&s2m_descs[0]);

    /* Poll until the IP writes the completion status word */
    while (sg_dma_poll(&ch.base) == 0) { /* busy-wait */ }

    uint8_t err = sg_dma_desc_error(&s2m_descs[0]);
    if (err != 0) {
        printf("[s2m] error 0x%02X\n", err);
    } else {
        printf("[s2m] received %u bytes\n", sg_dma_bytes_done(&ch.base));
    }
}

/* ------------------------------------------------------------------ */
/*  Example 3: Reusing a descriptor ring for continuous reception      */
/*                                                                      */
/*  Show how to reset next_to_reap and rebuild the chain for repeated  */
/*  submissions without re-initialising the driver instance.           */
/* ------------------------------------------------------------------ */

#define RING_DEPTH  4
#define BUF_SIZE    512

static sg_dma_desc_t ring_descs[RING_DEPTH];
static uint8_t       ring_bufs[RING_DEPTH][BUF_SIZE];

static void ring_completion(sg_dma_desc_t *desc, uint8_t error_code, void *user)
{
    uint32_t idx = (uint32_t)(desc - ring_descs);
    (void)user;
    printf("[ring] buffer %u complete, error=0x%02X, bytes=%u\n",
           idx, error_code, BUF_SIZE);
}

void example_continuous_rx(uintptr_t ip_mmio_base)
{
    sg_dma_s2m_t ch;

    sg_dma_s2m_init(&ch,
                    ip_mmio_base,
                    NULL, NULL,
                    ring_descs, RING_DEPTH,
                    ring_completion, NULL);

    sg_dma_reset(&ch.base);

    uint32_t addrs[RING_DEPTH];
    uint16_t lengths[RING_DEPTH];
    uint32_t i;
    for (i = 0; i < RING_DEPTH; i++) {
        addrs[i]   = (uint32_t)(uintptr_t)ring_bufs[i];
        lengths[i] = BUF_SIZE;
    }

    /* Submit once, then re-arm in a loop */
    for (uint32_t round = 0; round < 8; round++) {
        /* Rebuild the chain: this clears all status words to 0 */
        sg_dma_build_chain(ring_descs, RING_DEPTH, addrs, lengths,
                           /*irq_on_last=*/true);

        /* Reset the reap index so poll starts from the beginning again */
        ch.base.next_to_reap = 0;

        sg_dma_s2m_submit(&ch, (uint32_t)(uintptr_t)&ring_descs[0]);

        /* Wait until all descriptors in this round are reaped */
        while (ch.base.next_to_reap < RING_DEPTH) {
            sg_dma_irq_handler(&ch.base);  /* or sg_dma_poll in a polled flow */
        }

        printf("[ring] round %u done\n", round);
    }
}
