package spinalextras.lib.soc.spinex

import com.fasterxml.jackson.annotation.JsonPropertyDescription
import spinal.core._
import spinalextras.lib.ipgen.{IPGenerator, IPGeneratorOptions, IPGenerator_}
import spinalextras.lib.misc.ClockSpecification
import spinalextras.lib.soc.peripherals.XipFlashPlugin

import scala.language.postfixOps

case class SpinexSpecification(
                                name: String = "Spinex",
                                frequency: HertzNumber = 75 MHz,
                                onChipRamSize: BigInt = 0x10000,
                                pipelineDBus: Boolean = true,
                                pipelineMainBus: Boolean = false,
                                pipelineApbBridge: Boolean = true,
                                withJtag: Boolean = true,
                                withUart: Boolean = true,
                                withI2C: Boolean = true,
                                withXip: Boolean = true,
                                hardwareBreakpointCount: Int = 3,
                                externalInterrupts: Int = 8
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
    )
  }
}

/**
 * IP Generator for the Spinex SoC.
 * Integrates into the IPGenerator framework to allow CLI-based generation from YAML/JSON specs.
 */
object GenerateSpinex extends IPGenerator_[SpinexSpecification] {

  override def defaultClockDomainFrequency(cfg: SpinexSpecification): IClockDomainFrequency = {
    FixedFrequency(cfg.frequency)
  }

  override def processConfig(options: IPGeneratorOptions, config: SpinexSpecification): Unit = {
    processRtl(
      options,
      config,
      () => Spinex(config.toSpinexConfig()).setDefinitionName(config.name).noIoPrefix()
    )
  }

  override def DefaultConfig: Option[SpinexSpecification] = Some(SpinexSpecification())

  override def ConfigExample: SpinexSpecification = SpinexSpecification(
    name = "SpinexSoC_Example",
    frequency = ClockSpecification(80 MHz),
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
