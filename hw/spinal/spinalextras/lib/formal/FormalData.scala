package spinalextras.lib.formal

import spinal.core.{BaseType, Component, Data, MultiData, SpinalLog}
import spinal.lib.IMasterSlave

/**
 * This trait can be use by custom bundle classes to add logic on what is a valid state for that bundle.
 */
trait FormalData { self =>
  type Self <: FormalData

  /**
   * @return Whether or not the current state of the bundle is valid. Typically either asserted or assumed by a
   *         component which has this bundle as an input or an output.
   *
   *         For complicated properties, consider using the helper class `FormalProperties`
   */
  def formalIsStateValid(): Seq[FormalProperty]

  /**
   * When a bundle has a formal contract which can not be tied directly to it's current state, it is sometimes necessary
   * to assert that two objects have a functionally equivalent internal state.
   *
   * As a concrete example, a pipelined bus will typically have an assertion that it can't receive more data than was
   * requested and so there is an outstanding replies counter which must be part of the busses formal contract. This
   * function would be called when you wanted to ensure that another bus has the exact smae oustanding replies counter.
   *
   * This is typically required for inductive methods which often must be told this, even if the components share
   * all of the same signals.
   *
   * @param that The object to assert equivalence with.
   */
  def formalAssertEquivalence(that: Self): Unit = {}

  def underlyingData: Data = self.asInstanceOf[Data]

  def checkForEquivalance(): this.type = {
    def findTypeParent(e: Any) : Option[Self] = {
      e match {
        case self: Self =>
          Some(self)
        case b: BaseType => findTypeParent(b.parent)
        case b: MultiData => findTypeParent(b.parent)
        case _ => None
      }
    }

    def isSlaveInterface(d: Data) : Boolean = {
      d match {
        case null => false
        case ms: IMasterSlave => {
          (ms.isMasterInterface, ms.isSlaveInterface) match {
            case (true, _) => false
            case (_, true) => true
            case _ => isSlaveInterface(d.parent)
          }
        }
        case _ => isSlaveInterface(d.parent)
      }
    }

    def flattenElements(elements : Seq[Data]): Seq[BaseType] = {
      elements.flatMap(x => {
        x match {
          case b: BaseType => {
            val isInput = b.isInput ^ (b.component == x.component) ^ isSlaveInterface(b)
            println(s"=== ${b} raw input ${b.isInput}  input ${isInput} slave: ${isSlaveInterface(b)} component parent ${(b.component == x.component)})")
            if (isInput)
              Seq(b)
            else
              Seq()
          }
          case m: MultiData => flattenElements(m.elements.map(_._2))
        }
      })
    }

    def getAssignmentBundle(e : Any): Option[Self] = {
      e match {
        case b: BaseType => {
          if(b.dlcHasOnlyOne && b.dlcHead.source.isInstanceOf[BaseType]) {
            findTypeParent(b.dlcHead.source.asInstanceOf[BaseType]).orElse(getAssignmentBundle(b.dlcHead.source))
          } else {
            None
          }
        }
        case b: MultiData => {
          val flattenedSource = flattenElements(b.elements.map(_._2))
          val assignmentSources = flattenedSource.map(x => getAssignmentBundle(x))
          val singleSource = assignmentSources.forall(assignmentSources.head == _)
          if(singleSource) {
            assignmentSources.headOption.flatten
          } else {
            None
          }
        }
        case _ => None
      }
    }

    val singleSource = getAssignmentBundle(this)
    if(singleSource.nonEmpty) {
      println(s"Associating ${singleSource.get} with ${this}")
      formalAssertEquivalence(singleSource.get)
    }

    this
  }

  Component.toplevel.addPrePopTask( () => {
    checkForEquivalance()
  })
}
