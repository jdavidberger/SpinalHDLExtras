package spinalextras.lib.formal

import spinal.core.internals.{AssertStatement, AssertStatementKind}
import spinal.core.{Bool, Data, ImplicitArea, Nameable, ScopeProperty, True, assert}
import spinal.idslplugin.Location

import scala.collection.mutable
import scala.language.implicitConversions

/**
 * A formal property is a thing that is expected to be true. msg and loc are captured so that error messages can be
 * more easily understood, and correspond to the same arguments in `assert`
 *
 * @param cond The condition which is expected to be true
 * @param msg  The diagnostic information to be emitted when the formal property fails
 * @param loc  The location to reference for the property. Typically emitted / captured in the case of assertion failure.
 */
class FormalProperty(cond: Bool, val msg: Seq[Any])(implicit val loc: Location) {
  // Create and associate an assert with this property. Note that the kind -- ASSERT or ASSUME -- can be modified later.
  // This prevents us from creating a bunch of asserts unintentionally.
  //lazy val assertStatement = assert(condition, msg)(loc = loc)

  def apply(assertStatementKind: AssertStatementKind) = {
    condition.component.withAutoPull()
    assertStatementKind match {
      case AssertStatementKind.ASSUME => spinal.core.assume(condition)(loc = loc)
      case AssertStatementKind.ASSERT => spinal.core.assert(condition, msg)(loc = loc)
    }
  }

  // If the FormalProperty has a scope, it's important we capture and incorporate it into the various tests. This allows
  // the correct behavior when a formal property is created in a `when` scope.
  private val _context = ScopeProperty.capture()
  val condition = {
    val c = Bool().allowOverride()
    val oldContext = ScopeProperty.captureNoClone()
    _context.restoreCloned()
    c := cond
    oldContext.restore()
    def sanitize(seq: Seq[Any]) = {
      seq.map(_.toString).foldLeft("")(_+_)
        .replaceAll("[^A-Za-z0-9]", "_")
    }
    c.setWeakName(sanitize(msg))
    c
  }
}

object FormalProperty {
  /**
   * Enables the syntax
   *
   * {{{
   * override def formalIsStateValid() = (customCondition, "Custom condition message")
   * }}}
   */
  implicit def toFormalProperty(cond: (Bool, Any))(implicit loc: Location): FormalProperty = new FormalProperty(cond._1, Seq(cond._2))(loc)

  def apply(cond: Bool, msg: String)(implicit loc: Location) = toFormalProperty(cond, msg)
  /**
   * Enables the syntax
   *
   * {{{
   * override def formalIsStateValid() = Seq(customCondition1, customCondition2)
   * }}}
   */
  implicit def toFormalProperty(cond: Bool)(implicit loc: Location): FormalProperty = new FormalProperty(cond, Seq())(loc)

  /**
   * Enables the syntax
   *
   * {{{
   * override def formalIsStateValid() = customCondition
   * }}}
   */
  implicit def toFormalProperties(cond: Bool)(implicit loc: Location): Seq[FormalProperty] = Seq(new FormalProperty(cond, Seq())(loc))

  implicit def toFormalProperties(area: ImplicitArea[Seq[FormalProperty]]): Seq[FormalProperty] = area.implicitValue
}


/***
 * For complicated components, there might be a lot of internal logic to derive the properties, as opposed to just a
 * few booleans. This helper class makes a more readable form for declaring these properties.
 *
 * {{{
 *   override def formalIsStateValid() = new FormalProperties {
 *       addFormalProperty(condition1)
 *       addFormalProperty(condition2)
 *       when(condition3) {
 *           addFormalProperty(condition4)
 *       }
 *   }
 * }}}
 */
class FormalProperties(val self : Nameable = null, val postfix : String = "formalProperties") extends ImplicitArea[Seq[FormalProperty]] {

  val prefix = if(self != null) {
    if (postfix == null) {
      setCompositeName(self, weak = true)
    } else {
      setCompositeName(self, postfix, weak = true)
    }
    Seq(self.toString())
  } else Seq()

  lazy val formalProperties = new mutable.ArrayBuffer[FormalProperty]()

  def addFormalProperty(cond: Bool, msg: String)(implicit loc: Location): Unit =
    formalProperties += new FormalProperty(cond, prefix ++ Seq(msg))(loc)

  def addFormalProperty(cond: Bool, msg: Seq[Any] = Seq())(implicit loc: Location): Unit =
    formalProperties += new FormalProperty(cond, prefix ++ msg)(loc)

  def addFormalProperties(properties: Seq[FormalProperty]): Unit =
    formalProperties ++= properties

  implicit override def implicitValue: Seq[FormalProperty] = formalProperties.toSeq
}
