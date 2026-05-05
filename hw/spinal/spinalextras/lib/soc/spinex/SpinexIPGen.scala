package spinalextras.lib.soc.spinex

import com.fasterxml.jackson.annotation._
import com.fasterxml.jackson.databind.module.SimpleModule
import com.fasterxml.jackson.databind.node.ObjectNode
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import spinal.core._
import spinal.lib.bus.misc.{MaskMapping, SizeMapping}
import spinalextras.lib.ipgen.{IPGenerator, IPGeneratorOptions, IPGenerator_}
import spinalextras.lib.soc.peripherals.XipFlashPlugin
import spinalextras.lib.soc.spinex.plugins.{BusType, BusTypeDeserializer, PeripheralBus}

import scala.language.postfixOps
import scala.reflect.runtime.universe.typeOf

//object BusType extends Enumeration {
//  type BusType = Value
//  val PipelinedMemoryBus, AXI, Wishbone = Value
//}

case class SlavePeripheral (
                             name: String,
                             address: MaskMapping,
                             busType : BusType
                          ) {

}

case class SpinexSpecification(frequency: HertzNumber = 75 MHz,
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
                               peripherals: Seq[SlavePeripheral] = Seq(),
                              ) {

  /**
   * Translates the serializable specification into the actual SpinexConfig
   * used by the hardware generator.
   */
  def toSpinexConfig(): SpinexConfig = {
    val xipCfg = if(withXip) Some(XipFlashPlugin.defaultConfig) else None

    // Base generation off the SpinexConfig.default constructor
    SpinexConfig.default(
      withJtag = withJtag,
      xipConfig = xipCfg,
      withUart = withUart,
      withI2C = withI2C
    ).copy(
      onChipRamSize = onChipRamSize,
      pipelineDBus = pipelineDBus,
      pipelineMainBus = pipelineMainBus,
      pipelineApbBridge = pipelineApbBridge,
      hardwareBreakpointCount = hardwareBreakpointCount,
      externalInterrupts = externalInterrupts
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
