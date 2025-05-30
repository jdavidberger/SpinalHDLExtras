package spinalextras.lib.formal

import spinal.core.internals.{AssertStatement, AssertStatementKind}
import spinal.core.{Bool, Component, DslScopeStack, ScopeProperty, SpinalTag, True, cover, in, when}
import spinalextras.lib.formal.ComponentWithFormalProperties.DefaultProperties

import scala.collection.mutable

class HasFormalAssertsTag(val formalAsserts : HasFormalProperties) extends SpinalTag

/**
 * This trait is used to define some component-like thing (area, component, etc) as having formal properties that it
 * abides by. Use overrides on `formalInputProperties` and `formalProperties` when implementing this trait to fill in
 * this logic.
 *
 * formalInputProperties defines what constitutes valid input to the component-like thing. It can be empty for certain
 * cases.
 *
 * `formalProperties` are a set of properties that are expected to hold when every property in `formalInputProperties`
 * is true.
 *
 * When setting up for formal testing, you also need to invoke `formalSetMinimumAssertionKind`, `formalConfigureForTest`,
 * `formalAssertProperties` or `formalAssumeProperties` to determine how these properties are used. The two typical
 * cases are:
 *
 * - When being tested directly, `formalConfigureForTest` assumes all the input properties are true and asserts that the
 * body properties are true.
 * - When nested in a component under test, assert all the input properties are true and assume that the body properties
 * are true. This allows the solver to not waste time trying to verify the properties in the component not under direct
 * test.
 *
 * If you are adding this to a Component, you likely want to use `ComponentWithFormalAsserts`
 */
trait HasFormalProperties { self =>
  /**
   * The typical configuration when testing on a block directly. Assume it's inputs and assert it's body properties.
   * Typically, the inputs are also all driven by `any_seq` or `any_const`.
   */
  def formalConfigureForTest() = {
    formalSetMinimumAssertionKind(AssertStatementKind.ASSERT, AssertStatementKind.ASSUME)
  }

  val parentComponent = Component.current

  def covers(): Seq[FormalProperty] = Seq()
  def formalCovers(): Unit = {
    covers().foreach(p => cover(p.condition)(loc=p.loc))
  }
  /**
   * Set the assertion kinds used for both body properties and input properties. Notably, this function will latch any
   * assertion level that was set to ASSUME even if called again with ASSESRTION. This follows system verilog logic
   * which will also effectively ignore asserts if it was given an assume with the same predicate prior.
   *
   * @param assertionKind Main properties assertion kind
   * @param inputAssertionKind Input properties assertion kind
   */
  def formalSetMinimumAssertionKind(assertionKind : AssertStatementKind = AssertStatementKind.ASSERT,
                                     inputAssertionKind : AssertStatementKind = AssertStatementKind.ASSERT): this.type = {
    if (shouldApplyAssertionKind(CurrentAssertionKind, assertionKind)) {
      CurrentAssertionKind = assertionKind
    }
    if (shouldApplyAssertionKind(CurrentInputsAssertionKind, inputAssertionKind)) {
      CurrentInputsAssertionKind = inputAssertionKind
    }
    this
  }

  def formalAssertProperties(): this.type = formalSetMinimumAssertionKind(AssertStatementKind.ASSERT)

  def formalAssumeProperties(): this.type = formalSetMinimumAssertionKind(AssertStatementKind.ASSUME)

  /**
   * @return Returns the list of properties that must be true if the input to the given component-like class are to
   *         be considered valid, and thus signify that the `formalProperties` themselves are all true as well.
   *
   *         For complicated properties, consider using the helper class `FormalProperties`
   */
  protected def formalInputProperties() : Seq[FormalProperty] = Seq()

  /**
   * @return The formal properties which should all be true if the formalInputProperties are true too. These are the main
   *         assertions we are concerned with defining and verifying in formal testing
   *
   *         For complicated properties, consider using the helper class `FormalProperties`
   */
  protected def formalProperties() : Seq[FormalProperty]

  if (!this.isInstanceOf[Component]) {
    println(s"Adding ${this} to component ${Component.current}@${Component.current.getInstanceCounter}")
    Component.current.addTag(new HasFormalAssertsTag(this))
  }

  private def shouldApplyAssertionKind(current: Option[AssertStatementKind], newKind: AssertStatementKind): Boolean = {
    (current, newKind) match {
      case (None, _) => true
      case (_, AssertStatementKind.ASSERT) => false
      case (Some(AssertStatementKind.ASSUME), _) => false
      case (Some(AssertStatementKind.ASSERT), AssertStatementKind.ASSUME) => true
    }
  }

  private var _CurrentAssertionKind : Option[AssertStatementKind] = None
  def CurrentAssertionKind = _CurrentAssertionKind
  def CurrentAssertionKind_=(kind: AssertStatementKind): Unit ={
    val actualKind = if(HasFormalProperties.alwaysAssert) AssertStatementKind.ASSERT else kind
    _CurrentAssertionKind = Some(actualKind)
  }

  private var _CurrentInputsAssertionKind : Option[AssertStatementKind] = None
  def CurrentInputsAssertionKind = _CurrentInputsAssertionKind
  def CurrentInputsAssertionKind_=(kind: AssertStatementKind): Unit ={
    _CurrentInputsAssertionKind = Some(kind)
  }

  final lazy val formalInputStateIsValid: Bool = {
    formalInputPropertiesEval.map(_.condition).fold(True)((x,y) => x && y).setWeakName(s"formalInputStateIsValid")
  }

  final lazy val formalStateIsValid: Bool = {
    formalInputStateIsValid && formalProperties().map(_.condition).fold(True)((x, y) => x && y)
  }

  def formalChildren(): Seq[HasFormalProperties] = Seq()
  def formalDescendants(includeSelf : Boolean = false): Seq[HasFormalProperties] = {
    (if(includeSelf) Seq(self) else Seq()) ++
      formalChildren().flatMap(c => c.formalDescendants(true))
  }
  /**
   * Gather all the formal properties and turn them into assert statements. These statements can be upgraded to assumptions
   * later if required.
   */

  // We use private lazy vals here to only call formalProperties / formalInputProperties once.
  private lazy val formalPropertiesEval = formalProperties()
  private lazy val formalInputPropertiesEval = formalInputProperties()

  def formalAssumeChildrensInputs(): Unit = {
    parentComponent.withAutoPull()

    when(formalInputStateIsValid) {
      formalChildren().foreach(child => {
        child.formalInputProperties().foreach(prop => spinal.core.assume(prop.condition))
      })
    }
  }

  Component.toplevel.addPrePopTask(() => {
    // For assumptions, it is a hard requirement that we predicate the assumption on the inputs being valid. It is also
    // useful with assertions -- the fewer assertions that fire off at once when testing the better since that makes it
    // easier to figure out where the flaw in the logic is.
    CurrentAssertionKind.foreach(kind => {
      when(formalInputStateIsValid) {
        formalPropertiesEval.foreach(_(kind))
      }
    })

    if(CurrentAssertionKind.contains(AssertStatementKind.ASSUME)) {
      formalAssumeChildrensInputs()
    }

    CurrentInputsAssertionKind.foreach(kind => {
        formalInputPropertiesEval.foreach(_(kind))
    })
  })
}

object HasFormalProperties {
  /***
   * Occasionaly it can be useful to swap every assumption except the input assumptions for assertions. This is mainly
   * to verify that there are no faulty assumptions being made.
   */
  private def alwaysAssert: Boolean = sys.env.contains("SPINAL_FORMAL_NEVER_ASSUME")

  private def allFormalTraits(c: Component = Component.toplevel): Seq[HasFormalProperties] = {
    if (c == null) {
      return Seq()
    }

    def apply(c: Component, walkSet: mutable.HashSet[Component]): Seq[HasFormalProperties] = {
      if (!walkSet.contains(c)) {

        walkSet += c

        (c match {
          case asserts: HasFormalProperties => Seq(asserts)
          case _ => Seq()
        }) ++
          c.getTagsOf[HasFormalAssertsTag]().map(_.formalAsserts) ++
          c.children.flatMap(apply(_, walkSet))
      } else Seq()
    }

    val walkSet = new mutable.HashSet[Component]()
    apply(c, walkSet)
  }

  def printFormalAssertsReport(): Unit = {
    def toString(assertStatementKind: Option[AssertStatementKind]): String = {
      assertStatementKind match {
        case Some(AssertStatementKind.ASSUME) => "ASSUME"
        case Some(AssertStatementKind.ASSERT) => "ASSERT"
        case _ => "NONE"
      }
    }

    def formalAssertDesc(c : HasFormalProperties): String = {
      s"Inputs: ${toString(c.CurrentInputsAssertionKind)} (${c.formalInputProperties().size}) Body: ${toString(c.CurrentAssertionKind)} Properties: ${c.formalProperties().size}"
    }

    def stringify(msg : Seq[Any]): String = {
      msg.foldLeft("")(_+ " " + _)
    }

    def printBody(c : Any, prefix : String): Unit = {
      c match {
        case c: HasFormalProperties => {
          println(s"\t$prefix Inputs: ${toString(c.CurrentInputsAssertionKind)} ")
          c.formalInputProperties().foreach(c => println(s"\t\t$prefix - ${stringify(c.msg)} (${c.loc.file}.scala:${c.loc.line})"))
          println(s"\t$prefix Body: ${toString(c.CurrentAssertionKind)} ")
          c.formalProperties().foreach(c => println(s"\t\t$prefix - ${stringify(c.msg)} (${c.loc.file}.scala:${c.loc.line})"))
        }
        case _ => {}
      }
    }

    def printTree(c : Component, tabs : Int = 0): Unit = {
      val desc =
        c match {
          case c: HasFormalProperties => formalAssertDesc(c)
          case _ => ""
        }

      val prefix = "\t" * tabs

      println(s"$prefix ${c.getClass.getSimpleName} ${c.name} ${desc}")
      printBody(c, prefix)

      c.getTagsOf[HasFormalAssertsTag]().map(_.formalAsserts).foreach(c => {
        println(s"\t$prefix ${c.getClass.getSimpleName} $c ${formalAssertDesc(c)}")
        printBody(c, f"\t$prefix")
      })

      if(c.children.nonEmpty) {
        println(f"\t$prefix Children:")
        c.children.foreach(printTree(_, tabs = tabs + 2))
      }
    }

    Component.toplevel.addPrePopTask(() => {
      printTree(Component.toplevel)
    })
  }

}

