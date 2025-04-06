package spinalextras.lib.testing

import spinal.core.formal._
import spinal.core._
import spinal.lib.{Counter, CounterFreeRun}
import spinalextras.lib.formal.{ComponentWithFormalProperties, HasFormalProperties}

import java.io.IOException
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

object ReflectionUtils {
  def constructorArgs(obj: Any): Map[String, Any] = {
    val mirror = cm.reflect(obj)
    val instanceSymbol = mirror.symbol
    val instanceType = instanceSymbol.toType

    // Extract constructor parameters
    val constructorParams = instanceType.decls.collect {
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }.flatMap(_.paramLists.flatten)

    val instanceMirror = mirror

    // Get field values using reflection
    constructorParams
      .map(name => {
        val field = obj.getClass.getDeclaredFields.filter(_.getName.endsWith(f"${name.name}")).head
        val v = field.get(obj)
        name.toString -> v // instanceMirror.reflectMethod(member.asMethod)()
      })
      .toMap
  }
}

case class GeneralFormalDut(f : () => ComponentWithFormalProperties) extends Component {
  val dut = FormalDut(f())
  assumeInitial(ClockDomain.current.isResetActive)

  dut.anyseq_inputs()
  HasFormalProperties.printFormalAssertsReport()

  val cycles = CounterFreeRun(5)
  cover(cycles.value > 3)

}


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
    .withEngies(Seq(SmtBmc(nopresat = false,
      //solver = SmtBmcSolver.Boolector,
      progress = false, //noincr = false,
      //track_assumes = true, minimize_assumes = true
    )))

  def generateRtl(): Seq[(String, () => Component)]

  def generateRtlBMC(): Seq[(String, () => Component)] = Seq()

  def generateRtlCover(): Seq[(String, () => Component)] = generateRtl()

  def generateRtlProve(): Seq[(String, () => Component)]  = generateRtl()

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
