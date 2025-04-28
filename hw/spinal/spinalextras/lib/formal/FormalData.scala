package spinalextras.lib.formal

import spinal.core.{BaseType, Bundle, Component, Data, MultiData, SpinalLog, UInt, allowFloating, cloneOf}
import spinal.lib.IMasterSlave

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.{ClassTag, classTag}

object FormalData {
  val input_sets = mutable.HashMap[String, (Set[String], Set[String])]()
  def get_inputs(data: Data) : (Set[String], Set[String]) = {
    val typeString = data match {
      case b : Bundle => Some(b.getTypeString)
      case _ => None
    }

    if(typeString.isDefined && input_sets.contains(typeString.get)) {
      return input_sets(typeString.get)
    }

    data match {
      case ms : IMasterSlave => {

        val cloned = cloneOf(ms)
        cloned.setAsMaster()
        val ins, outs = mutable.Set[String]()
        for(kv <- cloned.asInstanceOf[MultiData].elements) {
          val should_flip = kv._2 match {
            case ms: IMasterSlave => ms.isSlaveInterface
            case _ => false
          }

          var (c_ins, c_outs) = get_inputs(kv._2)
          c_ins = c_ins.map(kv._1 + "." + _).map(_.stripSuffix("."))
          c_outs = c_outs.map(kv._1 + "." + _).map(_.stripSuffix("."))
          if(should_flip){
            outs ++= c_ins
            ins ++= c_outs
          } else {
            ins ++= c_ins
            outs ++= c_outs
          }
        }
        cloned.setWeakName("clonedExemplar")
        cloned.addTag(allowFloating)

        if(typeString.isDefined) {
          input_sets += ((typeString.get, (ins.toSet, outs.toSet)))
        }

        (ins.toSet, outs.toSet)
      }
      case b : Bundle => {
        if(b.isInput)
          (b.elements.map(_._1).toSet, Set())
        else
          (Set(), b.elements.map(_._1).toSet)
      }
      case d : Data => {
        if (d.isInput) {
          (Set(""), Set.empty[String])
        } else {
          (Set.empty[String], Set(""))
        }
      }
    }
  }

}
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

  def underlyingData: Data = self.asInstanceOf[Data]
}

trait FormalDataWithEquivalnce[T <: FormalData] extends FormalData {
  def selfClassTag : ClassTag[T]

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
  def formalAssertEquivalence(that: T): Unit

  def checkForEquivalance(): this.type = {
    def findTypeParent(e: Data) : Option[T] = {
      if (e == null) return None
      val f = fillins.findFillin(e)
      //println(s"Checking ${e} / ${f}")
      f match {
        case self: T if selfClassTag.runtimeClass.isInstance(f)  => {
          //println(s"Found ${e}")
          Some(self)
        }
        case b: BaseType => findTypeParent(b.parent)
        case b: MultiData => findTypeParent(b.parent)
        case b : FormalData => findTypeParent(b.underlyingData.parent)
        case _ => {
          None
        }
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

    def flattenElementsToPaths(elements : Seq[(String, Data)]): Seq[(String, BaseType)] = {
      elements.flatMap(x => {
        x._2 match {
          case b: BaseType => {
            Seq((x._1, b))
          }
          case m: MultiData => flattenElementsToPaths(m.elements).map(y => (x._1 + "." + y._1, y._2))
        }
      })
    }

    def flattenElements(elements : Seq[Data]): Seq[BaseType] = {
      elements.flatMap(x => {
        x match {
          case b: BaseType => {
            val isInput = b.isInput ^ (b.component == x.component) ^ isSlaveInterface(b)
//            if (isInput)
//              println(s"=== ${b} raw input ${b.isInput}  input ${isInput} slave: ${isSlaveInterface(b)} component parent ${(b.component == x.component)})")
            if (isInput)
              Seq(b)
            else
              Seq()
          }
          case m: MultiData => flattenElements(m.elements.map(_._2))
        }
      })
    }


    def getAssignmentBundle(e : Any): Option[T] = {
      e match {
        case b: BaseType => {
          //println(f"---- ${b} ${b.dlcHead} ---- ${b.dlcLast}")
          if(b.dlcHasOnlyOne && b.dlcHead.source.isInstanceOf[BaseType]) {
            findTypeParent(b.dlcHead.source.asInstanceOf[BaseType]).orElse(getAssignmentBundle(b.dlcHead.source))
          } else {
            None
          }
        }
        case b: MultiData => {
          val flattenedPaths = flattenElementsToPaths(b.elements)
          val in_outs = FormalData.get_inputs(b)
          val flattenedPathIns = flattenedPaths.filter(x => in_outs._1.contains(x._1)).map(_._2)
          //val flattenedSource = flattenElements(b.elements.map(_._2))
          val assignmentSources = flattenedPathIns.map(x => getAssignmentBundle(x))
          //println(assignmentSources.map(_.map(_.underlyingData)))
          val singleSource = assignmentSources.forall(assignmentSources.head.map(_.underlyingData) == _.map(_.underlyingData))
          if(singleSource) {
            assignmentSources.headOption.flatten
          } else {
            None
          }
        }
        case _ => None
      }
    }

    //println(s"Looking for single source for ${this.underlyingData}")
    val singleSource = getAssignmentBundle(this.underlyingData)
    if(singleSource.nonEmpty) {
      println(s"Associating ${singleSource.get.underlyingData} with ${this.underlyingData}")
      formalAssertEquivalence(singleSource.get)
    }

    this
  }

  Component.toplevel.addPrePopTask( () => {
    checkForEquivalance()
  })
}
