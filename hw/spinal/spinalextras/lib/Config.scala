package spinalextras.lib

import spinal.core._
import spinal.core.sim._

object Config {
  def spinalConfig = new SpinalConfig(
    defaultClockDomainFrequency = FixedFrequency(75 MHz),
    targetDirectory = "hw/gen",
    defaultConfigForClockDomains = ClockDomainConfig(
      resetActiveLevel = HIGH,
      resetKind = SYNC
    ),
    onlyStdLogicVectorAtTopLevelIo = false,
    oneFilePerComponent = false,
    mergeAsyncProcess = true,
    //asyncResetCombSensitivity = true,
    //removePruned = true,
    headerWithDate = false,
    privateNamespace = false,
    inlineRom = true,
    romReuse = true,
    //mode = SystemVerilog,
    device = Device.LATTICE,

    //nameWhenByFile = true
  )//.addStandardMemBlackboxing(policy = blackboxAll)

  def spinal = spinalConfig

  def sim = SimConfig.withConfig(spinal.includeSimulation).withIVerilog//.withGhdl
    .withFstWave
    .allOptimisation
    .workspacePath("simulations/")
}