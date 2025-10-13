package spinalextras.lib.soc.peripherals

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinalextras.lib.misc.{ClockMeasure, RandomNumberGenerator}

case class SpinexConfigCtrlConfig(
  baseAddress: Int = 0x3000,
  gitHashValue: BigInt = BigInt("deadbeef", 16), // Default value, will be overridden at build time
  buildTimestamp: BigInt = 0, // Unix timestamp of build
  versionMajor: Int = 1,
  versionMinor: Int = 0,
  versionPatch: Int = 0
)

case class SpinexConfigCtrl(config: SpinexConfigCtrlConfig) extends Component {
  val io = new Bundle {
    val apb = slave(Apb3(Apb3Config(addressWidth = 12, dataWidth = 32)))
    
    // Reset control outputs
    val systemReset = out Bool()
    val cpuReset = out Bool() 
    val peripheralReset = out Bool()
    
    // Individual peripheral reset outputs (active high)
    val uartReset = out Bool()
    val timerReset = out Bool()
    val i2cReset = out Bool()
    val ethernetReset = out Bool()
    val spiReset = out Bool()
    val gpioReset = out Bool()
    val dmaReset = out Bool()
    val adcReset = out Bool()
    
    // External reset outputs (upper 16 bits of peripheral reset control)
    val externalResets = out Bits(16 bits)
    
    // Status inputs
    val pllLocked = in Bool() default(True)
    val systemReady = in Bool() default(True)

    // Clock frequency measurement inputs
    val clockInputs = in Bits(8 bits)

    // GPIO-style general purpose control
    val controlOut = out Bits(32 bits)
    val statusIn = in Bits(32 bits) default(0)
  }
  
  val baseAddress = config.baseAddress
  
  // APB3 slave factory for register mapping
  val busCtrl = Apb3SlaveFactory(io.apb)
  
  // === IDENTIFICATION BLOCK (0x00-0x0F) ===
  // Device identification
  val deviceId = B"32'h53504E58" // "SPNX" in ASCII
  busCtrl.read(deviceId, 0x00)
  
  // Version information
  busCtrl.read(B((config.versionMajor << 16) | (config.versionMinor << 8) | config.versionPatch, 32 bits), 0x04) // Version
  busCtrl.read(B(config.gitHashValue, 32 bits), 0x08) // Git hash
  busCtrl.read(B(config.buildTimestamp, 32 bits), 0x0C) // Build timestamp
  
  // === RESET CONTROL BLOCK (0x10-0x1F) ===
  val resetControl = busCtrl.createReadAndWrite(Bits(8 bits), 0x10, 0) init(0)
  val peripheralResetControl = busCtrl.createReadAndWrite(Bits(32 bits), 0x14, 0) init(0)
  busCtrl.read(peripheralResetControl, 0x18) // Read back peripheral reset status
  // 0x1C reserved for future reset controls
  
  // === STATUS BLOCK (0x20-0x2F) ===
  val systemStatus = B"8'h00" ## io.pllLocked.asBits ## io.systemReady.asBits ## B"2'h0" ## resetControl(3 downto 0)
  busCtrl.read(systemStatus, 0x20)
  busCtrl.read(io.statusIn, 0x24)
  
  // Create ClockMeasure instances for each clock input
  val clockMeasurers = for(i <- 0 until 8) yield {
    val measurer = ClockMeasure(io.clockInputs(i), cntTil = 80000) // 1ms measurement period for 80MHz system clock
    measurer.setName(s"clockMeasurer_$i")
    measurer
  }
  
  // Map frequency registers (0x40-0x5C for 8 clocks) and flush control
  val freqMeasurementControl = busCtrl.createReadAndWrite(Bits(32 bits), 0x28, 0) init(0)
  
  // Reset-persistent message register (survives system reset)
  val resetPersistentMessage = Reg(Bits(32 bits)) init(0)
  val messageBusReg = busCtrl.createReadAndWrite(Bits(32 bits), 0x2C, 0) init(0)
  messageBusReg := resetPersistentMessage
  when(busCtrl.isWriting(0x2C)) {
    resetPersistentMessage := messageBusReg
  }
  
  for(i <- 0 until 8) {
    busCtrl.read(clockMeasurers(i).io.output_cnt.payload.asBits, 0x40 + i * 4)
    clockMeasurers(i).io.flush := freqMeasurementControl(i) // Individual flush bits
  }
  
  // === CONTROL BLOCK (0x30-0x3F) ===
  val systemControl = busCtrl.createReadAndWrite(Bits(32 bits), 0x30, 0) init(0)
  val gpioControl = busCtrl.createReadAndWrite(Bits(32 bits), 0x34, 0) init(0)
  // 0x38, 0x3C reserved for future control registers
  
  // === RANDOM NUMBER GENERATOR (0x60-0x6F) ===
  val rng = new RandomNumberGenerator(withSeed = true)
  
  // 0x60: Random data output (read-only, new value each clock)
  busCtrl.read(rng.io.random.asBits, 0x60)
  
  // 0x64: LFSR seed lower 32 bits (read/write)
  val rngSeedLow = busCtrl.createReadAndWrite(UInt(32 bits), 0x64, 0) init(0xBABE1234L)
  
  // 0x68: LFSR seed upper 32 bits (read/write)
  val rngSeedHigh = busCtrl.createReadAndWrite(UInt(32 bits), 0x68, 0) init(0xACE1CAFEL)
  
  // Connect RNG seeding (write to either register triggers reseed)
  rng.io.seed.payload := rngSeedHigh @@ rngSeedLow
  rng.io.seed.valid := busCtrl.isWriting(0x64) || busCtrl.isWriting(0x68)
  rngSeedLow := rng.io.lfsrState(31 downto 0)
  rngSeedHigh := rng.io.lfsrState(63 downto 32)
  
  // 0x6C: RNG status (read-only)
  val rngStatus = B"31'h0" ## rng.io.random.valid.asBits
  busCtrl.read(rngStatus, 0x6C)
  
  // === DEBUG/TEST BLOCK (0xF0-0xFF) ===
  // Scratch register for testing
  val scratchReg = busCtrl.createReadAndWrite(Bits(32 bits), 0xFC, 0) init(B"32'h12345678")
  
  // Reset control logic
  io.systemReset := resetControl(0)
  io.cpuReset := resetControl(1) 
  io.peripheralReset := resetControl(2)
  
  // Individual peripheral reset outputs (mapped to peripheralResetControl bits)
  io.uartReset := peripheralResetControl(0)
  io.timerReset := peripheralResetControl(1)
  io.i2cReset := peripheralResetControl(2)
  io.ethernetReset := peripheralResetControl(3)
  io.spiReset := peripheralResetControl(4)
  io.gpioReset := peripheralResetControl(5)
  io.dmaReset := peripheralResetControl(6)
  io.adcReset := peripheralResetControl(7)
  
  // External reset outputs (upper 16 bits of peripheral reset control)
  io.externalResets := peripheralResetControl(31 downto 16)
  
  // General purpose control output
  io.controlOut := gpioControl

  // Clock measurers use ClockMeasure components with manual flush control

  // Auto-clear reset bits after 1 cycle (pulse reset)
  when(resetControl(4)) { // Auto-clear enable bit for main resets
    when(RegNext(resetControl(2 downto 0)) =/= 0) {
      resetControl(2 downto 0) := 0
    }
  }

  when(resetControl(5)) { // Auto-clear enable bit for peripheral resets
    when(RegNext(peripheralResetControl) =/= 0) {
      peripheralResetControl := 0
    }
  }
  
  noIoPrefix()
}

object SpinexConfigCtrl {
  
  // Helper to get git hash at compile time
  def getGitHash(): BigInt = {
    import scala.sys.process._
    try {
      val gitHash = "git rev-parse --short=8 HEAD".!!.trim
      BigInt(gitHash, 16)
    } catch {
      case _: Exception => BigInt("deadbeef", 16)
    }
  }
  
  // Helper to get build timestamp
  def getBuildTimestamp(): BigInt = {
    BigInt(System.currentTimeMillis() / 1000) // Unix timestamp
  }
  
  def withGitInfo(baseConfig: SpinexConfigCtrlConfig = SpinexConfigCtrlConfig()): SpinexConfigCtrlConfig = {
    baseConfig.copy(
      gitHashValue = getGitHash(),
      buildTimestamp = getBuildTimestamp()
    )
  }
}

// Register map documentation:
/*
=== IDENTIFICATION BLOCK (0x00-0x0F) ===
Address | Access | Name           | Description
--------|--------|----------------|------------------------------------------
0x00    | R      | DEVICE_ID      | Device ID: "SPNX" (0x53504E58)
0x04    | R      | VERSION        | Version: [31:16] Major, [15:8] Minor, [7:0] Patch
0x08    | R      | GIT_HASH       | Git commit hash (32-bit)
0x0C    | R      | BUILD_TIME     | Build timestamp (Unix time)

=== RESET CONTROL BLOCK (0x10-0x1F) ===
Address | Access | Name           | Description
--------|--------|----------------|------------------------------------------
0x10    | RW     | RESET_CTRL     | Reset control: [5] Periph auto-clear, [4] Main auto-clear, [2] Periph, [1] CPU, [0] System
0x14    | RW     | PERIPH_RST     | Peripheral reset: [31:16] External resets, [7:0] Internal peripherals
0x18    | R      | PERIPH_RST_RD  | Peripheral reset status readback (32-bit)
0x1C    | -      | (Reserved)     | Reserved for future reset controls

=== STATUS BLOCK (0x20-0x2F) ===
Address | Access | Name           | Description
--------|--------|----------------|------------------------------------------
0x20    | R      | SYS_STATUS     | System status: [4] PLL_LOCK, [3] SYS_RDY, [3:0] RESET_STATE
0x24    | R      | STATUS_IN      | General purpose status input (32-bit)
0x28    | RW     | FREQ_CTRL      | Frequency measurement control: [7:0] Flush bits for clk0-7
0x2C    | RW     | MSG_REG        | Reset-persistent message register (survives system reset)

=== CONTROL BLOCK (0x30-0x3F) ===
Address | Access | Name           | Description
--------|--------|----------------|------------------------------------------
0x30    | RW     | SYS_CTRL       | System control register (32-bit)
0x34    | RW     | GPIO_CTRL      | General purpose control output (32-bit)
0x38    | -      | (Reserved)     | Reserved for future control registers
0x3C    | -      | (Reserved)     | Reserved for future control registers

=== FREQUENCY MEASUREMENT BLOCK (0x40-0x5F) ===
Address | Access | Name           | Description
--------|--------|----------------|------------------------------------------
0x40    | R      | FREQ_CLK0      | Clock 0 frequency measurement (32-bit)
0x44    | R      | FREQ_CLK1      | Clock 1 frequency measurement (32-bit)
0x48    | R      | FREQ_CLK2      | Clock 2 frequency measurement (32-bit)
0x4C    | R      | FREQ_CLK3      | Clock 3 frequency measurement (32-bit)
0x50    | R      | FREQ_CLK4      | Clock 4 frequency measurement (32-bit)
0x54    | R      | FREQ_CLK5      | Clock 5 frequency measurement (32-bit)
0x58    | R      | FREQ_CLK6      | Clock 6 frequency measurement (32-bit)
0x5C    | R      | FREQ_CLK7      | Clock 7 frequency measurement (32-bit)

=== RANDOM NUMBER GENERATOR (0x60-0x6F) ===
Address | Access | Name           | Description
--------|--------|----------------|------------------------------------------
0x60    | R      | RNG_DATA       | Random number output (32-bit, new value each read)
0x64    | RW     | RNG_SEED_LOW   | LFSR seed lower 32 bits (write to reseed)
0x68    | RW     | RNG_SEED_HIGH  | LFSR seed upper 32 bits (write to reseed)
0x6C    | R      | RNG_STATUS     | RNG status: [0] Valid (always 1)

=== DEBUG/TEST BLOCK (0xF0-0xFF) ===
Address | Access | Name           | Description
--------|--------|----------------|------------------------------------------
0xFC    | RW     | SCRATCH        | Scratch register for testing (default: 0x12345678)

Internal peripheral reset bits [7:0]: [7] ADC, [6] DMA, [5] GPIO, [4] SPI, [3] ETH, [2] I2C, [1] Timer, [0] UART
External reset bits [31:16]: Available as externalResets[15:0] output port
Clock inputs: 8-bit input port for frequency measurement (clockInputs[7:0])
Frequency measurement: Uses ClockMeasure components with 1ms measurement period (80K system clock cycles)
Random Number Generator: 64-bit LFSR with non-linear mixing, period 2^64-1, statistically tested for uniformity
*/
