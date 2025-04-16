#include <zephyr/kernel.h>
#include <zephyr/devicetree.h>
#include <zephyr/drivers/usb/udc.h>
#include <zephyr/drivers/clock_control.h>

#include <zephyr/usb/usbd.h>
#include <zephyr/usb/bos.h>
#include <zephyr/logging/log.h>
#include <zephyr/sys/barrier.h>

#include <zephyr/logging/log.h>
#include <zephyr/logging/log_ctrl.h>
#include <zephyr/shell/shell.h>
#include <zephyr/drivers/i2c.h>

#include "spi.h"
#include "stdlib.h"

#define MAX_WRITE_BYTES 16
#define MAX_READ_BYTES 16

#define SPI_REG_ADDRESS_BASE (0xe0000000 + 0x1F000)

static int cmd_read_write(const struct shell *sh, size_t argc, char **argv) {
    if (argc < 2) {
        shell_fprintf(sh, SHELL_ERROR, "Usage: %s <num_reads> <byte1> <byte2> ...\n", argv[0]);
        return -EINVAL;
    }

    uint8_t read_bytes[MAX_READ_BYTES];
    // Parse the number of reads
    int num_reads = atoi(argv[1]);
    if (num_reads > MAX_READ_BYTES) {
        shell_fprintf(sh, SHELL_WARNING, "Too many read bytes, truncating to %d\n", MAX_READ_BYTES);
	num_reads = MAX_READ_BYTES;
    }

    // Parse the bytes to write
    uint8_t write_bytes[MAX_WRITE_BYTES] = {0};
    size_t num_writes = argc - 2;
    if (num_writes > MAX_WRITE_BYTES) {
        shell_fprintf(sh, SHELL_WARNING, "Too many bytes, truncating to %d\n", MAX_WRITE_BYTES);
        num_writes = MAX_WRITE_BYTES;
    }

    for (size_t i = 0; i < num_writes; i++) {
      write_bytes[i] = (uint8_t)strtol(argv[i + 2], 0, 16);
    }

    // Perform the read and write operations
    shell_fprintf(sh, SHELL_INFO, "Performing %d reads and writing %zu bytes\n", num_reads, num_writes);

    spi_read_write(SPI_REG_ADDRESS_BASE, write_bytes, num_writes, read_bytes, num_reads);

    if(num_reads > 0) {
      shell_fprintf(sh, SHELL_INFO, "Read bytes: ");
      for (size_t i = 0; i < num_reads; i++) {
        shell_fprintf(sh, SHELL_INFO, "%02X ", read_bytes[i]);
      }
      shell_fprintf(sh, SHELL_INFO, "\n");
    }
    
    return 0;
}

SHELL_STATIC_SUBCMD_SET_CREATE(spiFlash,
			       SHELL_CMD(read_write, NULL,
					 "read_write",
					 cmd_read_write),
			       SHELL_SUBCMD_SET_END);

SHELL_CMD_REGISTER(spiFlash, &spiFlash, "SPI flash commands", NULL);
