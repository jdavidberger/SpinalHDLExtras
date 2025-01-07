package spinalextras.lib.testing

import spinal.core.formal._
import spinal.core._

import java.io.IOException

trait FormalTestSuite {
  val config = FormalConfig._spinalConfig.copy(defaultConfigForClockDomains = ClockDomainConfig(
    resetActiveLevel = HIGH,
    resetKind = SYNC,
  ),
    mergeAsyncProcess = true,
    defaultClockDomainFrequency = FixedFrequency(100 MHz))

  val formalConfig = FormalConfig
    .withDebug
    .withConfig(config)

  def generateRtl(): Seq[(String, () => Component)]

  def generateRtlBMC(): Seq[(String, () => Component)] = Seq()

  def generateRtlCover(): Seq[(String, () => Component)] = generateRtl()

  def generateRtlProve() = generateRtl()

  def defaultDepth() = 100

  def BMCConfig(): SpinalFormalConfig = formalConfig.withBMC(defaultDepth())

  def CoverConfig(): SpinalFormalConfig = formalConfig.withCover(defaultDepth())

  def ProveConfig(): SpinalFormalConfig = formalConfig.withProve(defaultDepth())

  def renameDefinition(c: Component, suffix: String) = {
    c.setDefinitionName(c.getClass.getSimpleName + "_" + suffix)
  }

  def formalTests(): Seq[(String, () => Any)] = {
    (generateRtlBMC().map(lst => (s"${lst._1}_bmc", () => BMCConfig().doVerify(renameDefinition(lst._2(), s"${lst._1}_bmc")))) ++
      generateRtlProve().map(lst => (s"${lst._1}_prove", () => ProveConfig().doVerify(renameDefinition(lst._2(), f"${lst._1}prove")))) ++
      generateRtlCover().map(lst => (s"${lst._1}_cover", () => CoverConfig().doVerify(renameDefinition(lst._2(), s"${lst._1}_cover")))))
      .map(t => {
        (t._1, () => {
          try {
            t._2()
          } catch {
            case e: IOException => {
              println(s"Could not find sby ${e}")
            }
          }
        })
      })
  }
}
