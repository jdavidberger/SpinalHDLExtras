package spinalextras.lib.soc.spinex

import spinal.core.{HertzNumber, IntToBuilder, ifGen}
import spinal.lib.com.spi.ddr.{SpiXdrMasterCtrl, SpiXdrParameter}
import spinal.lib.com.uart.{UartCtrlGenerics, UartCtrlInitConfig, UartCtrlMemoryMappedConfig, UartParityType, UartStopType}
import vexriscv.ip.InstructionCacheConfig
import vexriscv.{VexRiscv, plugin}
import vexriscv.plugin.CsrAccess.WRITE_ONLY
import vexriscv.plugin.{BranchPlugin, CsrPlugin, CsrPluginConfig, DBusSimplePlugin, DecoderSimplePlugin, ExternalInterruptArrayPlugin, HazardSimplePlugin, IBusCachedPlugin, IBusSimplePlugin, IntAluPlugin, LightShifterPlugin, MulDivIterativePlugin, Plugin, RegFilePlugin, STATIC, SrcPlugin, StaticMemoryTranslatorPlugin, YamlPlugin}

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

case class SpinexConfig(coreFrequency : HertzNumber,
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
                        externalInterrupts : Int,
                        withWishboneBus : Boolean
                       ){
  require(pipelineApbBridge || pipelineMainBus, "At least pipelineMainBus or pipelineApbBridge should be enable to avoid wipe transactions")
  val genXip = xipConfig != null

}



object SpinexConfig{
  val resetVector = 0x20200000

  def default : SpinexConfig = default(true, false)
  def default(withXip : Boolean = true, bigEndian : Boolean = false) =  SpinexConfig(
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
          twoCycleRam = true,
          twoCycleCache = true
        ),
        resetVector = resetVector,
        prediction = STATIC,
        compressedGen = false,
        relaxedPcCalculation = true,
        memoryTranslatorPortConfig = null
      ),
//      new IBusSimplePlugin(
//        resetVector = resetVector, cmdForkOnSecondStage = false, cmdForkPersistence = false,
//        catchAccessFault = true,
//        prediction = STATIC,
//        compressedGen = false,
//      ),
      new DBusSimplePlugin(
        catchAddressMisaligned = true,
        catchAccessFault = true,
        earlyInjection = true,
        bigEndian = bigEndian
      ),
      //new CsrPlugin(CsrPluginConfig.small(mtvecInit = if(withXip) 0xE0040020l else 0x80000020l)),
      new CsrPlugin(CsrPluginConfig.small(mtvecInit = null).copy(mtvecAccess = WRITE_ONLY,
        ecallGen = true, wfiGenAsNop = true, withPrivilegedDebug = true, xtvecModeGen = false, debugTriggers = 8)),
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
        zeroBoot = true
      ),
      new IntAluPlugin,
      new SrcPlugin(
        separatedAddSub = false,
        executeInsertion = false,
        decodeAddSub = true,
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
    externalInterrupts = 8,
    withWishboneBus = true
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
