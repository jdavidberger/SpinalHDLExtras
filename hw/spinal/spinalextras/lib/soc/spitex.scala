package spinalextras.lib.soc

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.misc.{AddressMapping, BusSlaveFactory, BusSlaveFactoryAddressWrapper, SizeMapping}
import spinal.lib.bus.simple._
import spinal.lib.bus.wishbone.{AddressGranularity, Wishbone, WishboneConfig}
import spinal.lib.com.jtag.{Jtag, JtagTapInstructionCtrl}
import spinal.lib.com.spi.ddr.SpiXdrMasterCtrl.XipBus
import spinal.lib.com.spi.ddr._
import spinal.lib.com.uart._
import spinal.lib.cpu.riscv.debug._
import spinalextras.lib.Config
import spinalextras.lib.blackbox.lattice.lifcl.{OSCD, OSCDConfig}
import spinalextras.lib.blackbox.opencores.i2c_master_top
import spinalextras.lib.bus.{MultiInterconnectByTag, PipelinedMemoryBusMultiBus}
import spinalextras.lib.io.TristateBuffer
import spinalextras.lib.misc.ClockSpecification
import vexriscv.demo.MuraxPipelinedMemoryBusRam
import vexriscv.ip.{InstructionCacheConfig, InstructionCacheMemBus}
import vexriscv.plugin.CsrAccess.WRITE_ONLY
import vexriscv.plugin._
import vexriscv.{VexRiscv, VexRiscvConfig, plugin}

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

case class SpitexConfig(coreFrequency : HertzNumber,
                       onChipRamSize      : BigInt,
                       onChipRamHexFile   : String,
                       pipelineDBus       : Boolean,
                       pipelineMainBus    : Boolean,
                       pipelineApbBridge  : Boolean,
                       gpioWidth          : Int,
                       uartCtrlConfig     : UartCtrlMemoryMappedConfig,
                       xipConfig          : SpiXdrMasterCtrl.MemoryMappingParameters,
                       hardwareBreakpointCount : Int,
                       withNativeJtag      : Boolean,
                       cpuPlugins         : ArrayBuffer[Plugin[VexRiscv]],
                        externalInterrupts : Int
                      ){
  require(pipelineApbBridge || pipelineMainBus, "At least pipelineMainBus or pipelineApbBridge should be enable to avoid wipe transactions")
  val genXip = xipConfig != null

}



object SpitexConfig{
  val resetVector = 0x20200000

  def default : SpitexConfig = default(true, false)
  def default(withXip : Boolean = true, bigEndian : Boolean = false) =  SpitexConfig(
    coreFrequency         = 80 MHz,
    onChipRamSize         = 0x00010000,
    onChipRamHexFile      = null,
    pipelineDBus          = true,
    pipelineMainBus       = false,
    pipelineApbBridge     = true,
    gpioWidth = 32,
    xipConfig = ifGen(withXip) (SpiXdrMasterCtrl.MemoryMappingParameters(
      SpiXdrMasterCtrl.Parameters(8, 12, SpiXdrParameter(
        dataWidth = 4,
        ioRate = 1,
        ssWidth = 1))
        .addFullDuplex(id = 0)
        .addHalfDuplex(id = 1, rate = 1, ddr = false, spiWidth = 4, lateSampling = false),
      cmdFifoDepth = 32,
      rspFifoDepth = 32,
      xipEnableInit = true,

      modInit = 0,
      xipInstructionModInit = 0,
      xipAddressModInit  = 0,
      xipDummyModInit  = 0,
      xipPayloadModInit  = 1,
      xipInstructionDataInit = 0x6B,
      xipDummyDataInit = 0xa5,

      //xipConfigWritable = false,

      xip = SpiXdrMasterCtrl.XipBusParameters(addressWidth = 24, lengthWidth = 5)
    )),
    hardwareBreakpointCount = if(withXip) 3 else 0,
    withNativeJtag = false,
    cpuPlugins = ArrayBuffer( //DebugPlugin added by the toplevel
      new IBusCachedPlugin(
        config = InstructionCacheConfig(
          cacheSize = 2048,
          bytePerLine = 32,
          wayCount = 1,
          addressWidth = 32,
          cpuDataWidth = 32,
          memDataWidth = 32,
          catchIllegalAccess = true,
          catchAccessFault = true,
          asyncTagMemory = false,
          twoCycleRam = false,
          twoCycleCache = true
        ),
        resetVector = resetVector,
        prediction = STATIC,
        compressedGen = false,
        relaxedPcCalculation = false,
        memoryTranslatorPortConfig = null
      ),
      new DBusSimplePlugin(
        catchAddressMisaligned = true,
        catchAccessFault = true,
        earlyInjection = false,
        bigEndian = bigEndian
      ),
      //new CsrPlugin(CsrPluginConfig.small(mtvecInit = if(withXip) 0xE0040020l else 0x80000020l)),
      new CsrPlugin(CsrPluginConfig.small(mtvecInit = null).copy(mtvecAccess = WRITE_ONLY, ecallGen = true, wfiGenAsNop = true, withPrivilegedDebug = true, xtvecModeGen = false)),
      new MulDivIterativePlugin(
        genMul = true,
        genDiv = true,
        mulUnrollFactor = 1,
        divUnrollFactor = 1
      ),
      new DecoderSimplePlugin(
        catchIllegalInstruction = true
      ),
      new RegFilePlugin(
        regFileReadyKind = plugin.SYNC,
        zeroBoot = false
      ),
      new IntAluPlugin,
      new SrcPlugin(
        separatedAddSub = false,
        executeInsertion = true
      ),
      new LightShifterPlugin,
      new HazardSimplePlugin(
        bypassExecute = true,
        bypassMemory = true,
        bypassWriteBack = true,
        bypassWriteBackBuffer = true,
        pessimisticUseSrc = false,
        pessimisticWriteRegFile = false,
        pessimisticAddressMatch = false
      ),
      new BranchPlugin(
        earlyBranch = false,
        catchAddressMisaligned = true
      ),
      new StaticMemoryTranslatorPlugin(ioRange = _.msb),
      new ExternalInterruptArrayPlugin(
        machineMaskCsrId = 0xBC0,
        machinePendingsCsrId = 0xFC0,
        supervisorMaskCsrId = 0x9C0,
        supervisorPendingsCsrId = 0xDC0
      ),
      new YamlPlugin("cpu0.yaml")
    ),
    uartCtrlConfig = UartCtrlMemoryMappedConfig(
      uartCtrlConfig = UartCtrlGenerics(
        dataWidthMax      = 8,
        clockDividerWidth = 20,
        preSamplingSize   = 1,
        samplingSize      = 3,
        postSamplingSize  = 1
      ),
      initConfig = UartCtrlInitConfig(
        baudrate = 115200,
        dataLength = 7,  //7 => 8 bits
        parity = UartParityType.NONE,
        stop = UartStopType.ONE
      ),
      busCanWriteClockDividerConfig = false,
      busCanWriteFrameConfig = false,
      txFifoDepth = 16,
      rxFifoDepth = 16
    ),
    externalInterrupts = 1
  )

  def fast = {
    val config = default

    //Replace HazardSimplePlugin to get datapath bypass
    config.cpuPlugins(config.cpuPlugins.indexWhere(_.isInstanceOf[HazardSimplePlugin])) = new HazardSimplePlugin(
      bypassExecute = true,
      bypassMemory = true,
      bypassWriteBack = true,
      bypassWriteBackBuffer = true
    )
    //    config.cpuPlugins(config.cpuPlugins.indexWhere(_.isInstanceOf[LightShifterPlugin])) = new FullBarrelShifterPlugin()

    config
  }
}


case class Spitex(config : SpitexConfig = SpitexConfig.default) extends Component{
  import config._

  val wbConfig = WishboneConfig(32, 32, useERR = true, useRTY = false, selWidth = 4, addressGranularity = AddressGranularity.BYTE).withBurstType
  val io = new Bundle {
    //Clocks / reset
//    val asyncReset = in Bool()
//    val mainClk = in Bool()

    //Main components IO
    val jtag = ifGen(!config.withNativeJtag) (slave(Jtag()))

    //Peripherals IO
    val uart = master(Uart())

    val i2c0_scl = inout(Analog(Bool()))
    val i2c0_sda = inout(Analog(Bool()))

    val spiflash_clk = genXip generate out(Bool())
    val spiflash_cs_n = genXip generate out(Bool())
    val spiflash_dq = genXip generate inout(Analog(Bits(xipConfig.ctrl.spi.dataWidth bits)))

    val wb = master(Wishbone(wbConfig))
    val externalInterrupts = Array.fill(config.externalInterrupts)(in(Bool()) default(False))
  }
  noIoPrefix()

  val jtagNative = withNativeJtag generate new ClockingArea(debugClockDomain){
    val jtagCtrl = JtagTapInstructionCtrl()
    val tap = jtagCtrl.fromXilinxBscane2(userId = 2)
  }

  val mainClockDomain = ClockDomain.current

  val resetCtrlClockDomain = mainClockDomain.copy(
    reset = null,
    config = ClockDomainConfig(
      resetKind = BOOT
    )
  )

  val resetCtrl = new ClockingArea(resetCtrlClockDomain) {
    val mainClkResetUnbuffered  = False

    //Implement an counter to keep the reset axiResetOrder high 64 cycles
    // Also this counter will automatically do a reset when the system boot.
    val systemClkResetCounter = Reg(UInt(6 bits)) init(0)
    when(systemClkResetCounter =/= U(systemClkResetCounter.range -> true)){
      systemClkResetCounter := systemClkResetCounter + 1
      mainClkResetUnbuffered := True
    }

    when(BufferCC(mainClockDomain.readResetWire)){
      systemClkResetCounter := 0
    }

    //Create all reset used later in the design
    val mainClkReset = RegNext(mainClkResetUnbuffered)
    val systemReset  = RegNext(mainClkResetUnbuffered)
  }


  val systemClockDomain = mainClockDomain.copy(
    reset = resetCtrl.systemReset,
  )

  val debugClockDomain = mainClockDomain.copy(
    reset = resetCtrl.mainClkReset,
  )

  var interconnect: MultiInterconnectByTag = null

  import spinalextras.lib.bus.bus._

  def add_master(bus: IBusSimpleBus): Unit = {
    interconnect.addMaster(bus, "iBus")
  }

  def add_master(bus: InstructionCacheMemBus): Unit = {
    interconnect.addMaster(bus, "iBus")
  }

  def add_master(bus: DBusSimpleBus): Unit = {
    interconnect.addMaster(bus, "dBus")
  }

  def add_slave(bus: PipelinedMemoryBus, name : String, mapping : AddressMapping, tags : String*): Unit = {
    interconnect.addSlave(PipelinedMemoryBusMultiBus(bus), mapping = mapping, tags:_*)
  }

  def add_slave(bus: XipBus, name : String, mapping : AddressMapping, tags : String*): Unit = {
    interconnect.addSlave(bus, mapping = mapping, tags:_*)
  }
  def add_slave(bus: Wishbone, name : String, mapping : AddressMapping, tags : String*): Unit = {
    interconnect.addSlave(bus, mapping = mapping, tags:_*)
  }

  val system = new ClockingArea(systemClockDomain) {
    interconnect = new MultiInterconnectByTag()
    //    if(globalBus == null) {
//      globalBus = PipelineMemoryGlobalBus(PipelinedMemoryBusConfig(32, 32))
//    }

    val pipelinedMemoryBusConfig = PipelinedMemoryBusConfig(
      addressWidth = 32,
      dataWidth = 32
    )

    val bigEndianDBus = config.cpuPlugins.exists(_ match{ case plugin : DBusSimplePlugin => plugin.bigEndian case _ => false})

    //Instanciate the CPU
    val cpu = new VexRiscv(
      config = VexRiscvConfig(
        plugins = cpuPlugins += //new DebugPlugin(debugClockDomain, hardwareBreakpointCount)
          new EmbeddedRiscvJtag(
            p = DebugTransportModuleParameter(
              addressWidth = 7,
              version      = 1,
              idle         = 7
            ),
            debugCd = debugClockDomain,
            withTunneling = false,
            withTap = true
          )

      )
    )

    //Checkout plugins used to instanciate the CPU to connect them to the SoC
    val timerInterrupt = False
    val externalInterrupts = Reg(Bits(32 bits)) init(0)
    for(i <- 0 until config.externalInterrupts) {
      externalInterrupts(i) setWhen(io.externalInterrupts(i))
    }

    val usb32_irq_loc = 0
    val timer0_irq_loc = 1
    val uart_irq_loc = 2
    val framectrl = 4
    val i2s_rx = 6
    val i2s_tx = 7

    add_slave(io.wb, "wb0", SizeMapping(0xb0000000L, 0x0f000000L), "dBus")

    for(plugin <- cpu.plugins) plugin match{
      case plugin : IBusCachedPlugin =>
        add_master(plugin.iBus)
      case plugin : IBusSimplePlugin =>
        add_master(plugin.iBus)
      case plugin : DBusSimplePlugin => {
        add_master(plugin.dBus)
      }
      case plugin : CsrPlugin        => {
        plugin.timerInterrupt := timerInterrupt
      }
      case plugin : ExternalInterruptArrayPlugin => {
        plugin.externalInterruptArray := externalInterrupts
      }
      case plugin : DebugPlugin         => plugin.debugClockDomain{
        resetCtrl.systemReset setWhen(RegNext(plugin.io.resetOut))
        if (withNativeJtag) {
          jtagNative.jtagCtrl <> plugin.io.bus.fromJtagInstructionCtrl(ClockDomain(jtagNative.tap.TCK),0)
        } else {
          io.jtag <> plugin.io.bus.fromJtag()
        }
      }
      case plugin : EmbeddedRiscvJtag => {
        plugin.jtag <> io.jtag
      }
      case _ =>
    }



    //****** MainBus slaves ********
//    val mainBusMapping = ArrayBuffer[(PipelinedMemoryBus,SizeMapping)]()
    val ram = new MuraxPipelinedMemoryBusRam(
      onChipRamSize = onChipRamSize,
      onChipRamHexFile = onChipRamHexFile,
      pipelinedMemoryBusConfig = pipelinedMemoryBusConfig,
      bigEndian = bigEndianDBus
    )
    add_slave(ram.io.bus, "ram", SizeMapping(0x40000000l, onChipRamSize), "iBus", "dBus")
//    mainBusMapping += ram.io.bus -> (0x80000000l, onChipRamSize)

    val apbBridge = new PipelinedMemoryBusToApbBridge(
      apb3Config = Apb3Config(
        addressWidth = 20,
        dataWidth = 32
      ),
      pipelineBridge = pipelineApbBridge,
      pipelinedMemoryBusConfig = pipelinedMemoryBusConfig
    )
    add_slave(apbBridge.io.pipelinedMemoryBus, "apbBridge", SizeMapping(0xe0000000L, 1 MiB), "dBus")


    //******** APB peripherals *********
    val apbMapping = ArrayBuffer[(Apb3, SizeMapping)]()

    val uartCtrl = SpitexApb3UartCtrl(uartCtrlConfig)
    uartCtrl.io.uart <> io.uart
    externalInterrupts(uart_irq_loc) setWhen(uartCtrl.io.interrupt)
    apbMapping += uartCtrl.io.apb  -> (0x1800, 1 kB)

    val timer = new SpitexApb3Timer(0x2800)
    timerInterrupt setWhen(timer.io.interrupt)
    externalInterrupts(timer0_irq_loc) setWhen(timer.io.interrupt)
    apbMapping += timer.io.apb     -> (timer.baseAddress, 1 kB)

    val i2cCtrl = new i2c_master_top()
    i2cCtrl.attachi2c(io.i2c0_scl, io.i2c0_sda)
    add_slave(i2cCtrl.io.wb, "i2c", SizeMapping(0xee000000L, 64 Bytes), "dBus")

//
//    val i2cCtrl = new Apb3I2cCtrl(
//      I2cSlaveMemoryMappedGenerics(
//        ctrlGenerics = I2cSlaveGenerics(
//          samplingWindowSize = 3,
//          samplingClockDividerWidth = 10 bits,
//          timeoutWidth = 20 bits
//        ),
//        addressFilterCount = 4,
//        masterGenerics = I2cMasterMemoryMappedGenerics(
//          timerWidth = 12
//        )
//      )
//    )
//    apbMapping += i2cCtrl.io.apb     -> (0xa800, 1 kB)
//    i2cCtrl.io.i2c.scl.read := io.i2c0_scl
//    when(!i2cCtrl.io.i2c.scl.write) {
//      io.i2c0_scl := False
//    }
//
//    i2cCtrl.io.i2c.sda.read := io.i2c0_sda
//    when(!i2cCtrl.io.i2c.scl.write) {
//      io.i2c0_sda := False
//    }
//

    val xip = ifGen(genXip)(new Area{
      val ctrl = Apb3SpiXdrMasterCtrl(xipConfig)

      apbMapping += ctrl.io.apb     -> (0x1F000, 1 kB)

      add_slave(ctrl.io.xip, "xip", SizeMapping(0x20000000L, 0x01000000), "iBus", "dBus")

      val buffers = ctrl.io.spi.data.map(_ => TristateBuffer())

      for(i <- io.spiflash_dq.bitsRange) {
        val (phy, tristate, xdr) = (io.spiflash_dq(i), buffers(i), ctrl.io.spi.data(i))
        tristate.io.output_enable := xdr.writeEnable
        tristate.io.input := xdr.write(0)
        xdr.read(0) := RegNext(RegNext(tristate.io.output))
        tristate.io.phy <> phy
      }

      io.spiflash_clk := ctrl.io.spi.sclk.write(0)
      io.spiflash_cs_n := ctrl.io.spi.ss(0)

//      val bootloader = Apb3Rom("src/main/c/Spitex/xipBootloader/crt.bin")
//      apbMapping += bootloader.io.apb     -> (0x1E000, 4 kB)

    })



    //******** Memory mappings *********
    val apbDecoder = Apb3Decoder(
      master = apbBridge.io.apb,
      slaves = apbMapping.toSeq
    )

    interconnect.build()
  }

}


case class W25Q128JVxIM_quad(filename : String = "firmware.hex", offset : BigInt = 0x200000) extends Component {
  val io = new Bundle {
    val spiflash_cs_n = in Bool()
    val spiflash_clk = in Bool()
    val spiflash_dq = inout(Analog(Bits(4 bits)))
  }
  val flash = W25Q128JVxIM(filename, offset)

  flash.io.CSn := io.spiflash_cs_n
  flash.io.CLK := io.spiflash_clk

  flash.io.DIO <> io.spiflash_dq(0)
  flash.io.DO <> io.spiflash_dq(1)
  flash.io.WPn <> io.spiflash_dq(2)
  flash.io.HOLDn <> io.spiflash_dq(3)
}

case class W25Q128JVxIM(filename : String = "firmware.hex", offset : BigInt = 0x200000) extends BlackBox {
  addGeneric("FIRMWARE_FILENAME", filename)
  addGeneric("FIRMWARE_OFFSET", offset)

  val io = new Bundle {
    val CSn = in Bool()
    val CLK = in Bool()
    val DIO = inout(Analog(Bool()))
    val DO = inout(Analog(Bool()))
    val WPn = inout(Analog(Bool()))
    val HOLDn = inout(Analog(Bool()))
  }
  noIoPrefix()

}

object Spitex{
  def main(args: Array[String]) {
    Config.spinal.generateVerilog(Spitex())
  }
}

case class SpitexSim() extends Component {

  ClockDomain.push(ClockDomain.current.withBootReset())

  val oscd = new OSCD(OSCDConfig.create(ClockSpecification(80 MHz)))
  ClockDomain.push(oscd.hf_clk().get.withBootReset())

  val resetTimer = Timeout(10)
  val reset = RegInit(True) clearWhen resetTimer
  ClockDomain.push(oscd.hf_clk().get.copy(reset = reset))

  val som = new Spitex()
  val flash = new W25Q128JVxIM_quad("/home/justin/source/cr/zephyr-workspace3/build/zephyr/zephyr.hex")
  flash.io.spiflash_dq <> som.io.spiflash_dq
  flash.io.spiflash_cs_n <> som.io.spiflash_cs_n
  flash.io.spiflash_clk <> som.io.spiflash_clk

  som.io.jtag.tdi := False
  som.io.jtag.tms := False
  som.io.jtag.tck := False

  som.io.uart.rxd := True
  som.io.wb.DAT_MISO.clearAll()
  som.io.wb.ACK := False

}

object SpitexSim{
  def main(args: Array[String]) {
    Config.spinal.copy(device = Device("lattice", "lifcl")).generateVerilog(SpitexSim())
  }
}


object SpitexCfu{
  def main(args: Array[String]) {
    SpinalVerilog{
      val config = SpitexConfig.default
      config.cpuPlugins += new CfuPlugin(
        stageCount = 1,
        allowZeroLatency = true,
        encodings = List(
          CfuPluginEncoding (
            instruction = M"-------------------------0001011",
            functionId = List(14 downto 12),
            input2Kind = CfuPlugin.Input2Kind.RS
          )
        ),
        busParameter = CfuBusParameter(
          CFU_VERSION = 0,
          CFU_INTERFACE_ID_W = 0,
          CFU_FUNCTION_ID_W = 3,
          CFU_REORDER_ID_W = 0,
          CFU_REQ_RESP_ID_W = 0,
          CFU_INPUTS = 2,
          CFU_INPUT_DATA_W = 32,
          CFU_OUTPUTS = 1,
          CFU_OUTPUT_DATA_W = 32,
          CFU_FLOW_REQ_READY_ALWAYS = false,
          CFU_FLOW_RESP_READY_ALWAYS = false,
          CFU_WITH_STATUS = true,
          CFU_RAW_INSN_W = 32,
          CFU_CFU_ID_W = 4,
          CFU_STATE_INDEX_NUM = 5
        )
      )

      val toplevel = Spitex(config)

      toplevel.rework {
        for (plugin <- toplevel.system.cpu.plugins) plugin match {
          case plugin: CfuPlugin => plugin.bus.toIo().setName("miaou")
          case _ =>
        }
      }

      toplevel
    }
  }
}


case class SpitexApb3UartCtrl(config : UartCtrlMemoryMappedConfig) extends Component{
  val io = new Bundle{
    val apb =  slave(Apb3(Apb3UartCtrl.getApb3Config))
    val uart = master(Uart(ctsGen = config.uartCtrlConfig.ctsGen, rtsGen = config.uartCtrlConfig.rtsGen))
    val interrupt = out Bool()
  }

  val uartCtrl = new UartCtrl(config.uartCtrlConfig)
  io.uart <> uartCtrl.io.uart

  val busCtrl = Apb3SlaveFactory(io.apb)
  val bridge = driveFrom(busCtrl,config)
  io.interrupt := bridge.interruptCtrl.interrupt

  def driveFrom(busCtrl : BusSlaveFactory,config : UartCtrlMemoryMappedConfig,baseAddress : Int = 0) = new Area {
    val io = uartCtrl.io
    val g = config.uartCtrlConfig

    require(busCtrl.busDataWidth == 16 || busCtrl.busDataWidth == 32)
    val busCtrlWrapped = new BusSlaveFactoryAddressWrapper(busCtrl,baseAddress)
    //Manage config
    val uartConfigReg = Reg(io.config)
    uartConfigReg.clockDivider init(0)
    if(config.initConfig != null)config.initConfig.initReg(uartConfigReg)

    require(!config.busCanWriteClockDividerConfig)
    require(!config.busCanWriteFrameConfig)
    uartConfigReg.clockDivider.allowUnsetRegToAvoidLatch
    uartConfigReg.frame.allowUnsetRegToAvoidLatch

    io.config := uartConfigReg

    def sat255 (that : UInt) = if(widthOf(that) > 8) that.min(255).resize(8 bits) else that
    //manage TX
    val write = new Area {
      val streamUnbuffered = busCtrlWrapped.createAndDriveFlow(Bits(g.dataWidthMax bits), address = 0).toStream
      val (stream, fifoOccupancy) = streamUnbuffered.queueWithOccupancy(config.txFifoDepth)
      io.write << stream

      //busCtrlWrapped.read(sat255(config.txFifoDepth - fifoOccupancy),address = 4,bitOffset = 16)
      //streamUnbuffered.ready.allowPruning()
      //busCtrlWrapped.read(stream.valid, address = 4, 15)

      val txfull = !streamUnbuffered.ready
      val txempty = !stream.valid
      busCtrlWrapped.read(txfull, address = 4) // TXFULL
      busCtrlWrapped.read(txempty, address = 0x18) // TXEMPTY
    }

    //manage RX
    val read = new Area {
      val fifo = StreamFifo(io.read.payload, config.rxFifoDepth)
      fifo.io.push << io.read
      val stream = fifo.io.pop
      val fifoOccupancy = fifo.io.occupancy

      val streamBreaked = stream.throwWhen(io.readBreak)
      busCtrlWrapped.readStreamNonBlocking(streamBreaked, address = 0, validBitOffset = 16, payloadBitOffset = 0)

      val rxempty = !fifo.io.pop.valid
      val rxfull = !fifo.io.push.ready
      busCtrlWrapped.read(rxempty, address = 8) // RXEMPTY
      busCtrlWrapped.read(rxfull, address = 0x1c) // RXFULL

      def genCTS(freeThreshold : Int) = RegNext(fifoOccupancy <= config.rxFifoDepth - freeThreshold) init(False)  // freeThreshold => how many remaining space should be in the fifo before allowing transfer
    }

    val ev_enable_addr = 0x14
    val ev_pending_addr = 0x10
    //manage interrupts
    val interruptCtrl = new Area {
      val writeIntEnable = busCtrlWrapped.createReadAndWrite(Bool(), address = ev_enable_addr, 0) init(False)
      val readIntEnable  = busCtrlWrapped.createReadAndWrite(Bool(), address = ev_enable_addr, 1) init(False)
      val writeInt  = writeIntEnable & !write.stream.valid
      val readInt   = readIntEnable  &  read.streamBreaked.valid
      val interrupt = readInt || writeInt
      busCtrlWrapped.read(writeInt, address = ev_pending_addr, 0)
      busCtrlWrapped.read(readInt , address = ev_pending_addr, 1)
    }

//    val misc = new Area{
//      val readError = busCtrlWrapped.createReadAndClearOnSet(Bool(), 0x20, 0) init(False) setWhen(io.readError)
//      val readOverflowError = busCtrlWrapped.createReadAndClearOnSet(Bool(), 0x20, 1) init(False) setWhen(io.read.isStall)
//      busCtrlWrapped.read(io.readBreak, 0x20, 8)
//      val breakDetected = RegInit(False) setWhen(io.readBreak.rise())
//      busCtrlWrapped.read(breakDetected, 0x20, 9)
//      busCtrlWrapped.clearOnSet(breakDetected, 0x20, 9)
//      val doBreak = RegInit(False)
//      busCtrlWrapped.setOnSet(doBreak, 0x20, 10)
//      busCtrlWrapped.clearOnSet(doBreak, 0x20, 11)
//      io.writeBreak := doBreak
//    }
    io.writeBreak := False

  }

  busCtrl.printDataModel()
}


class SpitexApb3Timer(val baseAddress : BigInt) extends Component{
  val io = new Bundle {
    val apb = slave(Apb3(
      addressWidth = 8,
      dataWidth = 32
    ))
    val interrupt = out Bool()
  }

  val busCtrl = Apb3SlaveFactory(io.apb)
  val busCtrlWrapped = busCtrl // new BusSlaveFactoryAddressWrapper(busCtrl, baseAddress)

  val load_value = busCtrlWrapped.createReadAndWrite(UInt(32 bits), address = 0) init(0)
  val reload_value = busCtrlWrapped.createReadAndWrite(UInt(32 bits), address = 4) init(0)
  val en = busCtrlWrapped.createWriteOnly(Bool(), address = 8) init(False)

  val update_value = busCtrlWrapped.createAndDriveFlow(Bool(), address = 12)
  val update = busCtrlWrapped.createReadOnly(UInt(32 bits), address = 16) init(0)

  val ev_pending = busCtrlWrapped.createReadAndClearOnSet(Bool(), address = 24) init(False)
  val ev_enable = busCtrlWrapped.createReadAndWrite(Bool(), address = 28) init(False)

  val counter_value = Reg(UInt(32 bits)) init(0)
  val counter_is_zero = RegNext(counter_value === 1, init = False)

  when(update_value.valid) {
    update := counter_value
  }

  when(en) {
    counter_value := counter_value - 1
    when(counter_is_zero) {
      counter_value := reload_value
    }
  } otherwise {
    counter_value := load_value
  }

  when(counter_is_zero.rise()) {
    ev_pending := True
  }

  val uptime_latch = Bool()
  uptime_latch := False
  busCtrlWrapped.onWrite(address = 32) {
    uptime_latch := True
  }
  val uptime = CounterFreeRun(64 bits)
  val uptime_cycles = RegNextWhen(uptime.value, uptime_latch) init(0)
  busCtrlWrapped.createReadMultiWord(uptime_cycles, address = 36, "Uptime cycles") := uptime_cycles

  io.interrupt := ev_pending & ev_enable

  busCtrl.printDataModel()
}
