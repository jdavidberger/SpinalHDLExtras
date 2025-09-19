# SpinalHDLExtras

A collection of SpinalHDL components and SoC designs, featuring the SpineX RISC-V processor with advanced configuration and monitoring capabilities.

## SpineX Processor

SpineX is a configurable RISC-V SoC built with SpinalHDL, featuring VexRiscv CPU core and a comprehensive set of peripherals.

### Key Features

- **VexRiscv RISC-V CPU Core** with configurable cache and debug support
- **Advanced Configuration & Control Block** with system monitoring
- **8-Channel Clock Frequency Measurement** using dedicated measurement circuits
- **Comprehensive Reset Management** with 16 external reset outputs
- **Ethernet Support** (SpinexEth variant) with RGMII interface
- **Standard Peripherals**: UART, Timer, I2C, SPI Flash (XIP)
- **Flexible Bus Architecture** with APB3 and Wishbone support

## Processor Variants

### SpineX (Standard)
- Base RISC-V SoC with essential peripherals
- Configuration control block with frequency measurement
- External reset control capabilities

### SpinexEth (Ethernet-Enabled)
- All SpineX features plus Ethernet MAC
- RGMII interface for Gigabit Ethernet PHY
- MDIO interface for PHY management
- Lattice TriSpeed Ethernet MAC integration

## Memory Map

### APB3 Peripherals (Base: 0xE0000000)
- **0x0000-0x03FF**: SPI Flash Controller (XIP)
- **0x1800-0x1BFF**: UART Controller
- **0x2800-0x2BFF**: Timer
- **0x3000-0x33FF**: Configuration & Control Block

### Wishbone Peripherals (SpinexEth)
- **0xE0004000**: Ethernet MAC (4KB)
- **0xE0005000**: I2C Controller (32 bytes)

### Memory Regions
- **0x40000000**: On-chip RAM (configurable size)
- **0x20000000**: SPI Flash (XIP region, 16MB)

## Configuration & Control Block

The SpineX processor includes a comprehensive configuration and control peripheral that provides:

### Register Map (Base: 0xE0003000)

#### Identification Block (0x00-0x0F)
- **0x00**: Device ID - "SPNX" (0x53504E58)
- **0x04**: Version - [31:16] Major, [15:8] Minor, [7:0] Patch
- **0x08**: Git Hash - 32-bit commit hash (build-time)
- **0x0C**: Build Time - Unix timestamp (build-time)

#### Reset Control Block (0x10-0x1F)
- **0x10**: Reset Control - Main system resets with auto-clear
- **0x14**: Peripheral Reset - [31:16] External resets, [7:0] Internal peripherals
- **0x18**: Peripheral Reset Readback - Status readback
- **0x1C**: Reserved

#### Status Block (0x20-0x2F)
- **0x20**: System Status - PLL lock, system ready, reset state
- **0x24**: Status Input - General purpose status (32-bit)
- **0x28**: Frequency Control - [7:0] Flush bits for clock measurers
- **0x2C**: Reserved

#### Control Block (0x30-0x3F)
- **0x30**: System Control - General system control (32-bit)
- **0x34**: GPIO Control - General purpose control output (32-bit)
- **0x38-0x3C**: Reserved

#### Frequency Measurement Block (0x40-0x5F)
- **0x40-0x5C**: Clock 0-7 frequency readings (Hz, 32-bit each)

#### Debug Block (0xF0-0xFF)
- **0xFC**: Scratch Register - For testing (default: 0x12345678)

### Features

#### Clock Frequency Measurement
- **8 Independent Channels**: Measure frequency of external clocks
- **1ms Measurement Period**: Accurate frequency readings
- **ClockMeasure Components**: Uses proven SpinalHDL frequency measurement
- **Individual Flush Control**: Reset measurements per channel

#### Reset Management
- **System Resets**: CPU, system, peripheral reset control
- **16 External Resets**: Configurable reset outputs for external peripherals
- **Individual Peripheral Resets**: UART, Timer, I2C, Ethernet, SPI, GPIO, DMA, ADC
- **Auto-Clear Support**: Pulse reset functionality

#### Status Monitoring
- **PLL Lock Status**: Monitor clock system stability
- **System Ready**: General system status indication
- **Build Information**: Git hash and build timestamp for version tracking

## IO Ports

### Standard SpineX
```scala
val uart: Uart                    // UART interface
val i2c0_scl/sda: Analog(Bool)   // I2C interface
val spiflash_*: ...              // SPI Flash interface
val jtag: Jtag                   // JTAG debug interface
val wb: Wishbone                 // External Wishbone bus (optional)
val externalInterrupts: Bool[N]  // External interrupt inputs
val externalResets: Bits(16)     // External reset outputs
val clockInputs: Bits(8)         // Clock frequency measurement inputs
val pll_lock: Bool               // PLL lock status input
```

### SpinexEth (Additional)
```scala
val eth_rgmii_*: ...             // RGMII Ethernet PHY interface
val eth_mdc: Bool                // MDIO clock
val eth_mdio: Analog(Bool)       // MDIO data (bidirectional)
```

## Build Information

The processor automatically captures build information:
- **Git Commit Hash**: 32-bit hash of current commit
- **Build Timestamp**: Unix timestamp when hardware was compiled
- **Version Information**: Configurable major.minor.patch version

This information is accessible via the configuration control block for firmware version tracking and debugging.

## Usage

### Basic SpineX
```scala
val spinex = Spinex(SpinexConfig.default)
// Connect IO ports as needed
```

### SpineX with Ethernet
```scala
val spinexEth = SpinexEth(SpinexEthConfig.default)
// Additional Ethernet IO connections required
```

### Configuration
Both variants support extensive configuration through their respective config classes, allowing customization of:
- CPU plugins and cache configuration
- Peripheral selection and addressing
- Memory sizes and mapping
- Interrupt routing

## Development

Built with SpinalHDL, this project demonstrates advanced SoC design patterns including:
- Modular peripheral architecture
- Bus interconnect design
- Clock domain management
- Hardware/software co-design
- Build-time configuration capture

## License

See LICENSE file for details.