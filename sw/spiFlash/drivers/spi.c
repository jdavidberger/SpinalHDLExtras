#include "spi.h"

uint8_t spi_read(uint32_t reg){
    while(spi_cmdAvailability(reg) == 0);
    write_u32(SPI_CMD_READ, reg + SPI_DATA);
    while(spi_rspOccupancy(reg) == 0);
    return read_u32(reg + SPI_DATA);
}

void spi_write(uint32_t reg, uint8_t data){
    while(spi_cmdAvailability(reg) == 0);
    write_u32(data | SPI_CMD_WRITE, reg + SPI_DATA);
}

#include <zephyr/irq.h>

// This function can not be ran from SPI flash; need something like:
// zephyr_code_relocate(FILES drivers/spi.c LOCATION RAM)
void spi_read_write(uint32_t reg, const uint8_t* write, uint32_t write_size, uint8_t* read, uint32_t read_size) {
  uint32_t lock_key = irq_lock();
  
  spi_select(reg, 0);
  for(int i = 0;i < write_size;i++) {
    spi_write(reg, write[i]);
  }
  for(int i = 0;i < read_size;i++) {
    read[i] = spi_read(reg);
  }
  spi_deselect(reg, 0);

  int status = 1;
  while(status & 1) {
    spi_select(reg, 0);
    spi_write(reg, 5);
    status = spi_read(reg);
    spi_deselect(reg, 0);
  }
  
  irq_unlock(lock_key);
}
