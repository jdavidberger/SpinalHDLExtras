package spinalextras.lib

import spinal.core._
import spinal.core.sim._

object Config {
  def spinalConfig = new SpinalConfig(
    defaultClockDomainFrequency = FixedFrequency(80 MHz),
    targetDirectory = "hw/gen",
    defaultConfigForClockDomains = ClockDomainConfig(
      resetActiveLevel = HIGH,
      resetKind = SYNC
    ),
    onlyStdLogicVectorAtTopLevelIo = false,
    oneFilePerComponent = false,

    // Warning -- Sometimes iverilog gives bad results when this is true
    mergeAsyncProcess = false,
    //asyncResetCombSensitivity = true,
    //removePruned = true,
    headerWithDate = false,
    privateNamespace = false,
    inlineRom = true,
    romReuse = true,
    device = Device(vendor = "lattice", family = "lifcl"),
    //nameWhenByFile = true
  )//.addStandardMemBlackboxing(policy = blackboxAll)

  def spinal = spinalConfig

  def sim = SimConfig.withConfig(spinal.includeSimulation).withIVerilog
    .withFstWave
    //.allOptimisation
    .workspacePath("simulations/")
}