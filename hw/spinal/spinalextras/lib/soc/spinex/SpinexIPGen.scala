package spinalextras.lib.soc.spinex

import com.fasterxml.jackson.annotation._
import com.fasterxml.jackson.databind.module.SimpleModule
import com.fasterxml.jackson.databind.node.ObjectNode
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import spinal.core._
import spinal.lib.bus.misc.{MaskMapping, SizeMapping}
import spinalextras.lib.ipgen.{IPGenerator, IPGeneratorOptions, IPGenerator_}
import spinalextras.lib.misc.ClockSpecification
import spinalextras.lib.soc.peripherals.XipFlashPlugin
import spinalextras.lib.soc.spinex.plugins.{BusType, BusTypeDeserializer, PeripheralBus}
import vexriscv.ip.fpu.FpuParameter
import vexriscv.ip.{DataCacheConfig, InstructionCacheConfig}

import scala.language.postfixOps
import scala.reflect.runtime.universe.typeOf

//object BusType extends Enumeration {
//  type BusType = Value
//  val PipelinedMemoryBus, AXI, Wishbone = Value
//}

case class SlavePeripheral (
                             @JsonPropertyDescription("The name of the peripheral determines the external signal names for the bus access")
                             name: String,
                             @JsonPropertyDescription("The mapping region for the peripheral. An error occurs if it overlaps with an existing peripheral")
                             address: MaskMapping,
                             @JsonPropertyDescription("The bus type that should be exposed at the top level. Any of these options maps to the internal data bus on the CPU")
                             busType : BusType
                          ) {

}

case class SpinexPerformanceOptions(
                                   useIcache : Boolean = true,
                                   useDcache : Boolean = false,
                                    @JsonPropertyDescription("If set to none, HW mul/div will not be supported and will require SW support.")
                                    hwMulDiv : Option[SpinexMulDivOptions] = Some(
                                      SpinexMulDivOptions()
                                    ),
                                    hwFpu : Option[FpuParameter] = None,
                                    @JsonPropertyDescription("Whether or not to implement a faster shift operation. The full barrel option costs ~200 gates for a ~20 coremark/sec gain.")
                                    withFullBarrel : Boolean = false,
                                   ) {

}

case class SpinexSpecification(frequency: HertzNumber = 75 MHz,
                               @JsonPropertyDescription("Clock speed to run the SPI flash at. Defaults to the CPU clock. Higher values are possible for faster instruction access at the cost of an additional clock generated and additional gates used for a cross clock FIFO")
                               val spiflashClock: Option[ClockSpecification] = None,
                               onChipRamSize: BigInt = 0x10000,
                               pipelineDBus: Boolean = true,
                               pipelineMainBus: Boolean = false,
                               pipelineApbBridge: Boolean = true,
                               withJtag: Boolean = true,
                               withUart: Boolean = true,
                               withI2C: Boolean = true,
                               withXip: Boolean = true,
                               hardwareBreakpointCount: Int = 3,
                               externalInterrupts: Int = 8,
                               performanceOptions: SpinexPerformanceOptions = SpinexPerformanceOptions(),
                               peripherals: Seq[SlavePeripheral] = Seq(),
                              ) {

  /**
   * Translates the serializable specification into the actual SpinexConfig
   * used by the hardware generator.
   */
  def toSpinexConfig(spiFlashCD : ClockDomain = null): SpinexConfig = {
    val xipCfg = if(withXip) Some(XipFlashPlugin.defaultConfig) else None

    val flash_cd = spiflashClock.map(cd => {
      if(spiFlashCD != null)
        spiFlashCD else
        ClockDomain.external("spiflash", ClockDomainConfig(), frequency = cd.toClockFrequency())
    }).getOrElse(ClockDomain.current)

    // Base generation off the SpinexConfig.default constructor
    SpinexConfig.default(
      flashClockDomain = flash_cd,
      withJtag = withJtag,
      xipConfig = xipCfg,
      withUart = withUart,
      withI2C = withI2C,
      mulDivOptions = performanceOptions.hwMulDiv,
      dcacheConfig = if (performanceOptions.useDcache) Some(DataCacheConfig(
        cacheSize = 2048,
        bytePerLine = 32,
        wayCount = 1,
        addressWidth = 32,
        cpuDataWidth = 32,
        memDataWidth = 32,
        catchAccessError = true,
        catchIllegal = true,
        catchUnaligned = true
      )) else None,
      icacheConfig = if (performanceOptions.useIcache) Some(InstructionCacheConfig(
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
      )) else None,
      withFullBarrel = performanceOptions.withFullBarrel,
      hwFpu = performanceOptions.hwFpu
    ).copy(
      onChipRamSize = onChipRamSize,
      pipelineDBus = pipelineDBus,
      pipelineMainBus = pipelineMainBus,
      pipelineApbBridge = pipelineApbBridge,
      hardwareBreakpointCount = hardwareBreakpointCount,
      externalInterrupts = externalInterrupts,
    ).withPlugins(peripherals.map(
      cfg => PeripheralBus(cfg.name, SizeMapping(cfg.address.base, cfg.address.mask), cfg.busType)
    ):_*)
  }
}

/**
 * IP Generator for the Spinex SoC.
 * Integrates into the IPGenerator framework to allow CLI-based generation from YAML/JSON specs.
 */
object GenerateSpinex extends IPGenerator_[SpinexSpecification] {
  override def Schema(): JsonNode = {
    BusType.patchEnumFields(json_mapper, super.Schema())
  }

  override def customMappings(module: SimpleModule): Unit = {
    super.customMappings(module)
    module.addDeserializer(classOf[BusType], BusTypeDeserializer())
  }

  override def defaultClockDomainFrequency(cfg: SpinexSpecification): IClockDomainFrequency = {
    FixedFrequency(cfg.frequency)
  }

  override def processConfig(options: IPGeneratorOptions, config: SpinexSpecification): Unit = {
    processRtl(
      options,
      config,
      () => Spinex(config.toSpinexConfig())
    )
  }

  override def DefaultConfig: Option[SpinexSpecification] = Some(SpinexSpecification())

  override def ConfigExample: SpinexSpecification = SpinexSpecification(
    frequency = 80 MHz,
    withJtag = true,
    withUart = true,
    withI2C = false, // Disabled for this specific example
    withXip = true,
    onChipRamSize = 0x20000 // 128KB RAM
  )

  override def Labels: Seq[String] = Seq("SoC", "RISC-V", "VexRiscv")

  override def Name: String = "Spinex SoC"

  override def Description: String =
    """
      |A highly configurable RISC-V SoC based on VexRiscv.
      |Includes optional peripherals like UART, I2C, JTAG, and SPI XIP Flash.
      |""".stripMargin

  IPGenerator.KnownGenerators.update(Name, () => GenerateSpinex)
  def main(args: Array[String]): Unit = {
    GenerateSpinex.cli_main(args)
  }
}
