package spinalextras.lib.testing

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.formal._
import spinal.core._
import spinal.lib.{Counter, CounterFreeRun}
import spinalextras.lib.formal.ComponentWithFormalProperties.DefaultProperties
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperty, HasFormalProperties}

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

case class GeneralFormalDut(f : () => ComponentWithFormalProperties, depth : Int = -1) extends Component {
  val top = f()
  val dut = FormalDut(top)

  dut.formalAssumeChildrenPastDepth(depth)

  setDefinitionName("GeneralFormalDut" + top.getClass.getSimpleName)

  assumeInitial(ClockDomain.current.isResetActive)

  dut.anyseq_inputs()
  dut.covers().foreach(x => cover(x.condition)(x.loc))

  HasFormalProperties.printFormalAssertsReport()
}

class DefaultFormalDut(f : () => Component, depth : Int = -1) extends Component {
  val top = f()
  val dut = FormalDut(top)
  setName("DefaultFormalDut" + top.getClass.getSimpleName)

  assumeInitial(ClockDomain.current.isResetActive)

  val defaultProperties = DefaultProperties(dut)
  defaultProperties.formalConfigureForTest()
  defaultProperties.formalAssumeChildrenPastDepth(depth)
  dut.getAllIo.filter(_.isInput).filter(_.dlcIsEmpty).foreach(anyseq)

  HasFormalProperties.printFormalAssertsReport()

  val cycles = CounterFreeRun(5)
  cover(cycles.value > 3)
}

object GeneralFormalDut {
  def apply(f : () => ComponentWithFormalProperties) = new GeneralFormalDut(f)
  def apply(f : () => Component) = new DefaultFormalDut(f)
}


trait FormalTestSuite {
  val config = FormalConfig._spinalConfig.copy(defaultConfigForClockDomains = ClockDomainConfig(
    resetActiveLevel = HIGH,
    resetKind = SYNC,
  ),
    mergeAsyncProcess = true,
    removePruned = false,
    defaultClockDomainFrequency = FixedFrequency(100 MHz))

  def formalConfig = FormalConfig
    .withDebug
    .withConfig(config)
    .withEngies(Seq(SmtBmc(nopresat = false,
      //solver = SmtBmcSolver.Boolector,
      progress = true, //noincr = false,
      //track_assumes = true, minimize_assumes = true
    )))

  def generateRtl(): Seq[(String, () => Component)]

  def generateRtlBMC(): Seq[(String, () => Component)] = Seq()

  def generateRtlCover(): Seq[(String, () => Component)] = generateRtl()

  def generateRtlProve(): Seq[(String, () => Component)]  = generateRtl()

  def defaultDepth() = 20

  def BMCConfig(): SpinalFormalConfig = formalConfig.withBMC(defaultDepth())

  def CoverConfig(): SpinalFormalConfig = formalConfig.withCover(defaultDepth())

  def ProveConfig(): SpinalFormalConfig = formalConfig.withProve(defaultDepth())

  def renameDefinition(c: Component, suffix: String) = {
    if(c.definitionName == null) {
      c.setDefinitionName(c.getClass.getSimpleName + "_" + suffix)
    }
    c
  }

  def formalTests(): Seq[(String, () => Any)] = {
    ( generateRtlProve().map(lst => (s"${lst._1}_prove", () => ProveConfig().doVerify(renameDefinition(lst._2(), f"${lst._1}prove")))) ++
      generateRtlBMC().map(lst => (s"${lst._1}_bmc", () => BMCConfig().doVerify(renameDefinition(lst._2(), s"${lst._1}_bmc")))) ++
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

abstract class FormalAnyTestSuite extends AnyFunSuite with FormalTestSuite{
  formalTests().foreach(t => test(t._1) { t._2() })
}