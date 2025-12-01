package spinalextras.lib.peripherals.i2c

import spinal.core._
import spinal.core.formal.{FormalConfig, FormalDut, SmtBmc, anyseq}
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.wishbone.{AddressGranularity, Wishbone, WishboneConfig}
import spinal.lib.com.i2c.{I2cSlave, I2cSlaveBus, I2cSlaveGenerics, I2cSlaveIo}
import spinal.lib.io.TriState
import spinalextras.lib.blackbox.opencores.i2c_master_top
import spinalextras.lib.formal.ComponentWithFormalProperties.DefaultProperties
import spinalextras.lib.formal.HasFormalProperties

object I2CMasterSim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave
      .addIncludeDir("hw/verilog/opencores_i2c/rtl/verilog")
      .addSimulatorFlag("--no-timing")
      .withConfig(SpinalConfig(defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC)))
      .compile(new I2CMasterTestbench)
      .doSim { dut =>

        dut.io.i2cio.config.samplingClockDivider #= 0x40
        dut.io.i2cio.config.timeout #= 0x80
        dut.io.i2cio.config.tsuData #= 40

        // Initialize simulation
        dut.clockDomain.forkStimulus(period = 10)
        dut.io.i2c_scl #= true
        dut.io.i2c_sda #= true
        dut.clockDomain.waitSampling()

        // Test I2C master initialization and register access
        println("Testing register access...")

        // Set prescaler to 100
        writeReg(dut, 0, 0x64, dut.clockDomain) // Low byte
        writeReg(dut, 1, 0x00, dut.clockDomain) // High byte

        // Enable core and interrupts
        writeReg(dut, 2, 0xC0, dut.clockDomain)

        // Verify register values
        assert(readReg(dut, 0, dut.clockDomain) == 0x64, "PRER low byte mismatch")
        assert(readReg(dut, 1, dut.clockDomain) == 0x00, "PRER high byte mismatch")
        assert(readReg(dut, 2, dut.clockDomain) == 0xC0, "CTR mismatch")

        println("Register access test passed")

        // Test I2C write operation
        println("Testing I2C write operation...")

        // Prepare to issue a START condition with write to address 0x42
        writeReg(dut, 3, 0x42 << 1, dut.clockDomain) // Device address with write bit (0)
        writeReg(dut, 4, 0x90, dut.clockDomain) // Set START and WRITE bits in CR

        // Write data byte 0xAB to the device
        writeReg(dut, 3, 0xAB, dut.clockDomain) // Data byte
        writeReg(dut, 4, 0x10, dut.clockDomain) // Set WRITE bit in CR

        // Send STOP condition
        writeReg(dut, 4, 0x40, dut.clockDomain) // Set STOP bit in CR

        // Wait for operation to complete
        waitForIrq(dut, dut.clockDomain)

        // Acknowledge the interrupt
        writeReg(dut, 4, 0x01, dut.clockDomain)

        println("I2C write test completed")

        // Test I2C read operation
        println("Testing I2C read operation...")

        // Send START condition with address 0x42 for reading
        writeReg(dut, 3, (0x42 << 1) | 1, dut.clockDomain) // Device address with read bit (1)
        writeReg(dut, 4, 0x90, dut.clockDomain) // Set START and WRITE bits in CR

        // Wait for operation to complete
        waitForIrq(dut, dut.clockDomain)

        // Acknowledge the interrupt
        writeReg(dut, 4, 0x01, dut.clockDomain)

        // Read data byte with ACK
        writeReg(dut, 4, 0x20, dut.clockDomain) // Set READ bit in CR

        // Wait for operation to complete
        waitForIrq(dut, dut.clockDomain)

        // Read the received data
        val receivedData = readReg(dut, 3, dut.clockDomain)
        println(s"Read data: 0x${receivedData.toHexString}")

        // Acknowledge the interrupt
        writeReg(dut, 4, 0x01, dut.clockDomain)

        // Read data byte with NACK (last byte)
        writeReg(dut, 4, 0x28, dut.clockDomain) // Set READ and ACK bits in CR

        // Wait for operation to complete
        waitForIrq(dut, dut.clockDomain)

        // Read the received data
        val lastData = readReg(dut, 3, dut.clockDomain)
        println(s"Last read data: 0x${lastData.toHexString}")

        // Acknowledge the interrupt
        writeReg(dut, 4, 0x01, dut.clockDomain)

        // Send STOP condition
        writeReg(dut, 4, 0x40, dut.clockDomain) // Set STOP bit in CR

        // Wait for operation to complete
        waitForIrq(dut, dut.clockDomain)

        // Acknowledge the interrupt
        writeReg(dut, 4, 0x01, dut.clockDomain)

        println("I2C read test completed")
        println("All tests completed")
      }
  }

  // Helper function to write to I2C master registers
  def writeReg(dut: I2CMasterTestbench, addr: Int, data: Int, clockDomain: ClockDomain): Unit = {
    dut.io.wb.CYC #= true
    dut.io.wb.STB #= true
    dut.io.wb.WE #= true
    dut.io.wb.ADR #= addr
    dut.io.wb.DAT_MOSI #= data

    clockDomain.waitSamplingWhere(dut.io.wb.ACK.toBoolean)

    dut.io.wb.CYC #= false
    dut.io.wb.STB #= false
    dut.io.wb.WE #= false

    // Wait a cycle for stability
    clockDomain.waitSampling()
  }

  // Helper function to read from I2C master registers
  def readReg(dut: I2CMasterTestbench, addr: Int, clockDomain: ClockDomain): Int = {
    dut.io.wb.CYC #= true
    dut.io.wb.STB #= true
    dut.io.wb.WE #= false
    dut.io.wb.ADR #= addr

    clockDomain.waitSamplingWhere(dut.io.wb.ACK.toBoolean)

    val data = dut.io.wb.DAT_MISO.toInt

    dut.io.wb.CYC #= false
    dut.io.wb.STB #= false

    // Wait a cycle for stability
    clockDomain.waitSampling()

    data
  }

  // Helper function to wait for interrupt
  def waitForIrq(dut: I2CMasterTestbench, clockDomain: ClockDomain, timeout: Int = 10000): Unit = {
    var timeoutCounter = 0
    while (!dut.io.interrupt.toBoolean && timeoutCounter < timeout) {
      clockDomain.waitSampling()
      timeoutCounter += 1
    }

    assert(timeoutCounter < timeout, "Timeout waiting for interrupt")
    println(s"Interrupt received after ${timeoutCounter} cycles")
  }

  def verifyI2cTwin(i2cMaster : I2cMaster): i2c_master_top = {
    val i2cMasterComp = new i2c_master_top()

    i2cMasterComp.io.arst_i := i2cMasterComp.io.wb_rst_i

    i2cMaster.getAllIo.filter(_.isInput).filter(_.dlcIsEmpty).foreach(anyseq)

    val map1 = i2cMaster.getAllIo.map(x => (x.name, x)).toMap
    val map2 = i2cMasterComp.getAllIo.map(x => (x.name, x)).toMap

    for(k <- map1.keys) {
      if(map2.contains(k)) {
        val v1 = map1(k)
        val v2 = map2(k)

        if(v1.isInput) {
          v2.assignFrom(v1)
        }
      }
    }

    assert(i2cMaster.io.wb.ACK === i2cMasterComp.io.wb.ACK)
    when(i2cMaster.io.wb.ACK) {
      assert(i2cMaster.io.wb.DAT_MISO === i2cMasterComp.io.wb.DAT_MISO)
    }
    assert(i2cMaster.io.scl.writeEnable === i2cMasterComp.io.scl_padoen_o)
    assert(i2cMaster.io.scl.write === i2cMasterComp.io.scl_pad_o)
    assert(i2cMaster.io.sda.writeEnable === i2cMasterComp.io.sda_padoen_o)
    assert(i2cMaster.io.sda.write === i2cMasterComp.io.sda_pad_o)
    assert(i2cMaster.io.interrupt === i2cMasterComp.io.wb_inta_o)

    i2cMasterComp.io.sda_pad_i := i2cMaster.io.sda.read
    i2cMasterComp.io.scl_pad_i := i2cMaster.io.scl.read

    i2cMasterComp
  }
}

class I2CMasterTestbench(opencores : Boolean = false) extends Component {
  val io = new Bundle {
    val wb = slave(Wishbone(
      WishboneConfig(
        addressWidth = 3,
        dataWidth = 8,
        addressGranularity = AddressGranularity.WORD
      )
    ))
    val interrupt = out Bool()

    val i2cio = I2cSlaveIo(I2cSlaveGenerics())

    val i2c_scl = in Bool()
    val i2c_sda = in Bool()
  }

  val i2cSlave = new I2cSlave(I2cSlaveGenerics())
  i2cSlave.io <> io.i2cio

  val i2cMaster = I2cMaster()

  // Connect Wishbone bus
  io.wb <> i2cMaster.io.wb
  io.interrupt := i2cMaster.io.interrupt

  DefaultProperties(i2cMaster).formalAssertProperties()
  //  i2cSlave.io.i2c.scl.write := i2cMaster.io.scl.write
  //  i2cSlave.io.i2c.sda.write := i2cMaster.io.sda.write
  //
  i2cMaster.io.scl.read := i2cSlave.io.i2c.scl.write
  i2cMaster.io.sda.read := i2cSlave.io.i2c.sda.write

  I2CMasterSim.verifyI2cTwin(i2cMaster)
}


case class MatchDut() extends Component {
  val i2cMaster = new I2cMaster()
  val i2cMasterComp = new i2c_master_top()

  i2cMasterComp.io.arst_i := i2cMasterComp.io.wb_rst_i

  i2cMaster.getAllIo.filter(_.isInput).filter(_.dlcIsEmpty).foreach(anyseq)

  val map1 = i2cMaster.getAllIo.map(x => (x.name, x)).toMap
  val map2 = i2cMasterComp.getAllIo.map(x => (x.name, x)).toMap

  for(k <- map1.keys) {
    if(map2.contains(k)) {
      val v1 = map1(k)
      val v2 = map2(k)

      if(v1.isInput) {
        v2.assignFrom(v1)
      }
    }
  }

  assert(i2cMaster.io.wb.ACK === i2cMasterComp.io.wb.ACK)
  when(i2cMaster.io.wb.ACK) {
    assert(i2cMaster.io.wb.DAT_MISO === i2cMasterComp.io.wb.DAT_MISO)
  }
  assert(i2cMaster.io.scl.writeEnable === !i2cMasterComp.io.scl_padoen_o)
  assert(i2cMaster.io.scl.write === i2cMasterComp.io.scl_pad_o)
  assert(i2cMaster.io.sda.writeEnable === !i2cMasterComp.io.sda_padoen_o)
  assert(i2cMaster.io.sda.write === i2cMasterComp.io.sda_pad_o)
  assert(i2cMaster.io.interrupt === i2cMasterComp.io.wb_inta_o)

  i2cMasterComp.io.sda_pad_i := i2cMaster.io.sda.read
  i2cMasterComp.io.scl_pad_i := i2cMaster.io.scl.read

  DefaultProperties(i2cMaster).formalConfigureForTest()
  //DefaultProperties(i2cMasterComp).formalConfigureForTest()

  cover(i2cMaster.io.interrupt)
  cover(i2cMaster.tip)
  cover(i2cMaster.done)

  val hadInterrupt = RegInit(False) setWhen(i2cMaster.io.interrupt)
  cover(hadInterrupt && !i2cMaster.io.interrupt)

  val hadTip = RegInit(False) setWhen(i2cMaster.tip)
  cover(hadTip && !i2cMaster.tip)

  assumeInitial(ClockDomain.current.isResetActive)

  HasFormalProperties.printFormalAssertsReport()
}

object MatchDut {
  def main(args: Array[String]): Unit = {
    val config = FormalConfig._spinalConfig.copy(defaultConfigForClockDomains = ClockDomainConfig(
      resetActiveLevel = HIGH,
      resetKind = SYNC,
    ),
      mergeAsyncProcess = true,
      removePruned = true,
      defaultClockDomainFrequency = FixedFrequency(100 MHz))

    def formalConfig = FormalConfig
      .withDebug
      .withConfig(config)
      .withEngies(Seq(SmtBmc(nopresat = false,
        //solver = SmtBmcSolver.Boolector,
        progress = true, //noincr = false,
        //track_assumes = true, minimize_assumes = true
      )))

    val bmcConfig = formalConfig.withBMC(15 ).withCover(20)
    bmcConfig.doVerify(new MatchDut())("MatchDut")
  }
}