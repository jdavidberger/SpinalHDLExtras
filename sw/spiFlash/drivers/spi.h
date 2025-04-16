#ifndef SPI_H_
#define SPI_H_

#include "stdlib.h"
#include "stdint.h"

#define SPI_DATA        0x00
#define SPI_BUFFER      0x04
#define SPI_CONFIG      0x08
#define SPI_INTERRUPT   0x0C
#define SPI_CLK_DIVIDER 0x20
#define SPI_SS_SETUP    0x24
#define SPI_SS_HOLD     0x28
#define SPI_SS_DISABLE  0x2C

typedef struct {
    uint32_t cpol;
    uint32_t cpha;
    uint32_t mode;
    uint32_t clkDivider;
    uint32_t ssSetup;
    uint32_t ssHold;
    uint32_t ssDisable;
} Spi_Config;

#define SPI_CMD_WRITE (1 << 8)
#define SPI_CMD_READ (1 << 9)
#define SPI_CMD_SS (1 << 11)

#define SPI_RSP_VALID (1 << 31)

#define SPI_STATUS_CMD_INT_ENABLE = (1 << 0)
#define SPI_STATUS_RSP_INT_ENABLE = (1 << 1)
#define SPI_STATUS_CMD_INT_FLAG = (1 << 8)
#define SPI_STATUS_RSP_INT_FLAG = (1 << 9)


#define SPI_MODE_CPOL (1 << 0)
#define SPI_MODE_CPHA (1 << 1)


static inline uint32_t read_u32(uint32_t address){
    return *((volatile uint32_t*) address);
}

static inline void write_u32(uint32_t data, uint32_t address){
    *((volatile uint32_t*) address) = data;
}

static inline uint16_t read_u16(uint32_t address){
    return *((volatile uint16_t*) address);
}

static inline void write_u16(uint16_t data, uint32_t address){
    *((volatile uint16_t*) address) = data;
}

static inline uint8_t read_u8(uint32_t address){
    return *((volatile uint8_t*) address);
}

static inline void write_u8(uint8_t data, uint32_t address){
    *((volatile uint8_t*) address) = data;
}

static inline void write_u32_ad(uint32_t address, uint32_t data){
    *((volatile uint32_t*) address) = data;
}

#define writeReg_u32(name, offset) \
static inline void name(uint32_t reg, uint32_t value){ \
    write_u32(value, reg + offset); \
} \

#define readReg_u32(name, offset) \
static inline u32 name(uint32_t reg){ \
    return read_u32(reg + offset); \
} \

static uint32_t spi_cmdAvailability(uint32_t reg){
    return read_u32(reg + SPI_BUFFER) & 0xFFFF;
}
static uint32_t spi_rspOccupancy(uint32_t reg){
    return read_u32(reg + SPI_BUFFER) >> 16;
}

void spi_write(uint32_t reg, uint8_t data);

static uint8_t spi_writeRead(uint32_t reg, uint8_t data){
    while(spi_cmdAvailability(reg) == 0);
    write_u32(data | SPI_CMD_READ | SPI_CMD_WRITE, reg + SPI_DATA);
    while(spi_rspOccupancy(reg) == 0);
    return read_u32(reg + SPI_DATA);
}


uint8_t spi_read(uint32_t reg);

static void spi_select(uint32_t reg, uint32_t slaveId){
    while(spi_cmdAvailability(reg) == 0);
    write_u32(slaveId | 0x80 | SPI_CMD_SS, reg + SPI_DATA);
}

static void spi_diselect(uint32_t reg, uint32_t slaveId){
    while(spi_cmdAvailability(reg) == 0);
    write_u32(slaveId | 0x00 | SPI_CMD_SS, reg + SPI_DATA);
}

static void spi_applyConfig(uint32_t reg, Spi_Config *config){
    write_u32((config->cpol << 0) | (config->cpha << 1) | (config->mode << 4), reg + SPI_CONFIG);
    write_u32(config->clkDivider, reg + SPI_CLK_DIVIDER);
    write_u32(config->ssSetup, reg + SPI_SS_SETUP);
    write_u32(config->ssHold, reg + SPI_SS_HOLD);
    write_u32(config->ssDisable, reg + SPI_SS_DISABLE);
}

void spi_read_write(uint32_t reg, const uint8_t* write, uint32_t write_size, uint8_t* read, uint32_t read_size);

#endif /* SPI_H_ */

