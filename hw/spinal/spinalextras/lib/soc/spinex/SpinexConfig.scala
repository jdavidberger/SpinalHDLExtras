package spinalextras.lib.soc.spinex

import com.fasterxml.jackson.annotation.JsonPropertyDescription
import spinal.core.{B, BooleanPimped, ClockDomain, HertzNumber, IntToBuilder, TimeNumber, ifGen, log2Up}
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config, Apb3SlaveFactory}
import spinal.lib.bus.misc.{AddressMapping, BusSlaveFactoryNonStopWrite, BusSlaveFactoryOnReadAtAddress, BusSlaveFactoryOnWriteAtAddress, BusSlaveFactoryRead, BusSlaveFactoryWrite, SizeMapping}
import spinal.lib.com.spi.ddr.{SpiXdrMasterCtrl, SpiXdrParameter}
import spinal.lib.com.uart.{UartCtrlGenerics, UartCtrlInitConfig, UartCtrlMemoryMappedConfig, UartParityType, UartStopType}
import spinalextras.lib.soc.{DeviceTree, DeviceTreeProvider}
import spinalextras.lib.soc.peripherals.{UartCtrlPlugin, XipFlashPlugin}
import spinalextras.lib.soc.spinex.plugins.{I2CPlugin, IdentificationPlugin, JTagPlugin, OpenCoresI2CPlugin, TimerPlugin, Uart16550CtrlPlugin}
import vexriscv.ip.fpu.FpuParameter
import vexriscv.ip.{DataCacheConfig, InstructionCacheConfig}
import vexriscv.{VexRiscv, plugin}
import vexriscv.plugin.CsrAccess.WRITE_ONLY
import vexriscv.plugin.{BranchPlugin, CsrPlugin, CsrPluginConfig, DBusCachedPlugin, DBusSimplePlugin, DecoderSimplePlugin, ExternalInterruptArrayPlugin, FpuPlugin, FullBarrelShifterPlugin, HazardSimplePlugin, IBusCachedPlugin, IBusSimplePlugin, IntAluPlugin, LightShifterPlugin, MulDivIterativePlugin, MulPlugin, Plugin, RegFilePlugin, STATIC, SrcPlugin, StaticMemoryTranslatorPlugin, YamlPlugin}

import java.io.File
import java.nio.file.Files
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

trait SpinexPlugin {
  def apply(som : Spinex) : Unit
}

abstract class SpinexRegisterFilePlugin(path : String, mapping : SizeMapping) extends DeviceTreeProvider(mapping.base) with SpinexPlugin {
  override def compatible : Seq[String] = Seq(s"spinex,${entryName}")

  override def baseEntryPath: Seq[String] = Seq("/") ++ path.split('/').filter(_.nonEmpty)

  override def entryName: String = path.split('/').last

  override def regs : Seq[(String, SizeMapping)] = Seq(("base" -> SizeMapping(0, mapping.size)))
}

abstract class SpinexRegisterFileApbPlugin(name : String, mapping : SizeMapping) extends SpinexRegisterFilePlugin(name, SizeMapping(mapping.base + 0xe0000000L, mapping.size)) {
  val apbConfig = Apb3Config(log2Up(mapping.size), 32)
  lazy val apb : Apb3 = new Apb3(apbConfig).setName(s"${name}Bus", weak = true)

  var hasBusCtrl : Boolean = false

  lazy val busCtrl = {
    hasBusCtrl = true
    Apb3SlaveFactory(apb)
  }

  def busCtrlToRegs(busCtrl : Apb3SlaveFactory) = {
    busCtrl.elements.filter(_.mapping != null).map {
      case r: BusSlaveFactoryRead => {
        r.documentation -> SizeMapping(r.mapping.lowerBound, (r.that.getBitsWidth + 7) / 8)
      }
      case r: BusSlaveFactoryOnReadAtAddress => {
        r.documentation -> SizeMapping(r.mapping.lowerBound, 0x04)
      }
      case w: BusSlaveFactoryWrite => {
        w.documentation -> SizeMapping(w.mapping.lowerBound, (w.that.getBitsWidth + 7) / 8)
      }
      case w: BusSlaveFactoryOnWriteAtAddress => {
        w.documentation -> SizeMapping(w.mapping.lowerBound, 0x4)
      }
      case w: BusSlaveFactoryNonStopWrite => {
        w.documentation -> SizeMapping(w.mapping.lowerBound, 0x4)
      }
    }.filter(x => x != null && x._1 != null)
      .map(x => x._2.base -> x).toMap.values.toSeq.sortBy(_._2.base)
  }

  override def regs : Seq[(String, SizeMapping)] = {
    if(hasBusCtrl) {
      busCtrlToRegs(busCtrl)
    } else {
      Seq(("base" -> SizeMapping(0, mapping.size)))
    }
  }

  override def apply(som: Spinex): Unit = {
    som.system.apbMapping += apb -> mapping
  }

}

case class SpinexConfig(onChipRamSize      : BigInt,
                        onChipRamHexFile   : String,
                        pipelineDBus       : Boolean,
                        pipelineMainBus    : Boolean,
                        pipelineApbBridge  : Boolean,
                        gpioWidth          : Int,
                        uartCtrlConfig     : UartCtrlMemoryMappedConfig,
                        hardwareBreakpointCount : Int,
                        withJtag      : Boolean,
                        cpuPlugins         : ArrayBuffer[Plugin[VexRiscv]],
                        externalInterrupts : Int,
                        plugins : Seq[SpinexPlugin] = SpinexConfig.defaultPlugins
                       ){
  require(pipelineApbBridge || pipelineMainBus, "At least pipelineMainBus or pipelineApbBridge should be enable to avoid wipe transactions")

  def withPlugins(extraPlugins: SpinexPlugin*): SpinexConfig = {
    this.copy(plugins = extraPlugins ++ this.plugins)
  }

  def appendPlugins(extraPlugins: SpinexPlugin*): SpinexConfig = {
    this.copy(plugins = this.plugins ++ extraPlugins)
  }

}
case class SpinexMulDivOptions(@JsonPropertyDescription("The unroll factor of the multiplication operation. Multiplication will take ~ 32 / unroll_factor cycles. Higher values impose greater timing restrictions. If set to none, a DSP resource is used instead which takes ~1 cycle and fewer gates.")
                               mulUnrollFactor : Option[BigInt] = Some(1),
                               @JsonPropertyDescription("The unroll factor of the division operation. Division will take ~ 32 / unroll_factor cycles. Higher values here impose greater timing restrictions.")
                               divUnrollFactor : Int = 1
                              ) {

}

object SpinexConfig{
  val resetVector = 0x20200000

  def minimal : SpinexConfig = default(
    withJtag = true,
    xipConfig = None,//Some(XipFlashPlugin.defaultConfig),
    withI2C = false,
    withUart = true,
    ram_mapping = SizeMapping(0x40000000L, 0x0004000 Bytes),
    rom_mapping = SizeMapping(0x20200000L, 0x00010000)
  )

  def default : SpinexConfig = default()
  def default(bigEndian : Boolean = false, withJtag : Boolean = true,
              xipConfig          : Option[SpiXdrMasterCtrl.MemoryMappingParameters] = Some(XipFlashPlugin.defaultConfig),
              flashClockDomain   : ClockDomain = null,
              withUart : Boolean = true,
              withI2C : Boolean = true,
              ram_mapping : SizeMapping = SizeMapping(0x40000000L, 0x00010000 Bytes),
              rom_mapping : SizeMapping = SizeMapping(0x20000000L, 0x00010000),
              mulDivOptions: Option[SpinexMulDivOptions] = Some(SpinexMulDivOptions(
                Some(1),
                1
              )),
              hwFpu : Option[FpuParameter] = None,
              ram_name : String = "",
              // Takes roughly 200 gates; but is much faster performance. 55 -> 73 c/s
              withFullBarrel : Boolean = false,
              icacheConfig : Option[InstructionCacheConfig] = Some(InstructionCacheConfig(
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
              )),
              dcacheConfig : Option[DataCacheConfig] = None
             ) =  SpinexConfig(
    onChipRamSize         = 0x00010000,
    onChipRamHexFile      = null,
    pipelineDBus          = true,
    pipelineMainBus       = false,
    pipelineApbBridge     = true,
    gpioWidth = 32,
    hardwareBreakpointCount = 3,
    withJtag = withJtag,
    cpuPlugins = ArrayBuffer( //DebugPlugin added by the toplevel
      icacheConfig.map(cfg =>
        new IBusCachedPlugin(config = cfg,
          resetVector = resetVector,
          prediction = STATIC,
          compressedGen = false,
          relaxedPcCalculation = false,
          memoryTranslatorPortConfig = null)
      ).getOrElse(
        new IBusSimplePlugin(
          resetVector = resetVector,
          cmdForkOnSecondStage = false,
          cmdForkPersistence = false,
          catchAccessFault = true,
          prediction = STATIC,
          compressedGen = false,
        )
      ),
      dcacheConfig.map(cfg =>
        new DBusCachedPlugin(
          config = cfg,
        )
      ).getOrElse(new DBusSimplePlugin(
        catchAddressMisaligned = true,
        catchAccessFault = true,
        earlyInjection = false,
        bigEndian = bigEndian
      )),
      new CsrPlugin(CsrPluginConfig.small(mtvecInit = null).copy(mtvecAccess = WRITE_ONLY,
        ecallGen = true, wfiGenAsNop = true, withPrivilegedDebug = withJtag, xtvecModeGen = false, debugTriggers = 8)),
      hwFpu.map(x => new FpuPlugin(p = x)).orNull,
      mulDivOptions.map(_.mulUnrollFactor == 0 generate new MulPlugin(
        inputBuffer = true,
        outputBuffer = false
      )).orNull,
      mulDivOptions.map(x => new MulDivIterativePlugin(
        genMul = x.mulUnrollFactor != 0,
        genDiv = true,
        mulUnrollFactor = x.mulUnrollFactor.map(_.toInt).getOrElse(0),
        divUnrollFactor = x.divUnrollFactor
      )).orNull,
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
        // false, false here gives ~78mhz @ 3217 gates
        // false, true here gives ~85mhz @ 3275
        // true, false here gives ~80mhz @ 3272
        executeInsertion = false,
        decodeAddSub = false,
      ),
      if(withFullBarrel) new FullBarrelShifterPlugin else new LightShifterPlugin,
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
    ).filter(_ != null),

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
    plugins = plugins(withJtag = withJtag, xipConfig, flashClockDomain, withUart = withUart, withI2C = withI2C, ram_mapping = ram_mapping, rom_mapping = rom_mapping, ram_name = ram_name)
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

  def plugins(withJtag : Boolean = true,
              xipConfig          : Option[SpiXdrMasterCtrl.MemoryMappingParameters] = Some(XipFlashPlugin.defaultConfig),
              flashClockDomain   : ClockDomain = null,
              withUart : Boolean = true,
              withI2C : Boolean = true,
              ram_mapping : SizeMapping = SizeMapping(0x40000000l, 0x00010000 Bytes),
              rom_mapping : SizeMapping = SizeMapping(0x20000000L, 0x00010000),
              ram_name : String = "",
             ) = {
    val plugins : ArrayBuffer[SpinexPlugin] = mutable.ArrayBuffer(
      IdentificationPlugin(registerLocation = 0x3000),
      //RandomPlugin(registerLocation = 0x3060),
      TimerPlugin(),
      if (withUart) UartCtrlPlugin() else null,

      if (withI2C) OpenCoresI2CPlugin() else null,
      SystemRam(if (ram_name.isEmpty) "/soc/sram0" else ram_name, mapping = ram_mapping),
      PrintAPBMapping()
    )

    if(withJtag) {
      plugins.append(new JTagPlugin())
    }

    val withXip = xipConfig.isDefined
    if(withXip)
      plugins.append(new XipFlashPlugin(config = xipConfig.get, clockDomain = flashClockDomain))
    else {
      plugins.append(SystemRam("spinex_rom", rom_mapping))
    }

    plugins.filterNot(_ == null).toSeq
  }

  def defaultPlugins = plugins()


}
