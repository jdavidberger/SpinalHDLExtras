package spinalextras.lib.formal

import spinal.core.formal.anyseq
import spinal.core.internals.AssertStatementKind
import spinal.core.{Area, Component, Data, MultiData, True}
import spinal.lib.{IMasterSlave, StreamFifo}
import spinalextras.lib.formal.fillins._

import scala.collection.mutable

object ComponentWithFormalProperties {

  type HandlerFunction = PartialFunction[Any, HasFormalProperties]

  private var handlers = mutable.ArrayBuffer[HandlerFunction]()

  def AddHandler(f: HandlerFunction): this.type = {
    handlers.append(f)
    this
  }

  /**
   * The default implementation of formalInputProperties for components checks their IO bundle for bundles with the
   * formal trait and uses those as the input properties.
   *
   * @return The list of properties that are true when the input to this component are valid
   */
  protected def formalInputProperties(c : Component) = new FormalProperties {
    val flattenIOs = flattenIO(c)

    val iosWithFillins = flattenIOs.map(fillins.findFillin)
    iosWithFillins.foreach({
      case io: FormalMasterSlave =>
        if (io.asIMasterSlave.isMasterInterface) addFormalProperties(io.formalIsConsumerValid())
        else if (io.asIMasterSlave.isSlaveInterface) addFormalProperties(io.formalIsProducerValid())
        else if (io.underlyingData.isInput) addFormalProperties(io.formalIsStateValid())
      case io: FormalData => if (io.underlyingData.isInput) addFormalProperties(io.formalIsStateValid())
      case io: Any => {}
    })
  }

  /**
   * Iterate through all the output IOs and assert/assum each is valid if it is driven by this component.
   */
  protected def formalCheckOutputs(c : Component) = new FormalProperties {
    val flattenIOs = flattenIO(c)

    flattenIOs.map(fillins.findFillin).foreach({
      case io: FormalMasterSlave =>
        if (io.asIMasterSlave.isMasterInterface) addFormalProperties(io.formalIsProducerValid())
        else addFormalProperties(io.formalIsConsumerValid())
      case io: FormalData => if (io.underlyingData.isOutput) addFormalProperties(io.formalIsStateValid())
      case _ => True
    })
  }

  // Recursively flatten out IO until we find the bundle / multidate with a direction specifier.
  private def flattenIO(c : Component): Seq[Data] = {
    def hasDirectionnSpecifier(io: Data) = io match {
      case io: IMasterSlave => (io.isSlaveInterface || io.isMasterInterface)
      case _ => !io.isDirectionLess
    }

    def recursiveWalk(io: Data): Seq[Data] = io match {
      case io: MultiData => if (hasDirectionnSpecifier(io)) Seq(io) else io.elements.map(_._2).toSeq.flatMap(recursiveWalk)
      case _ => Seq(io)
    }

    c.getGroupedIO(true).flatMap(recursiveWalk)
  }

  def ComponentAreas(c : Component): Set[Area] = {
    val areas = new mutable.HashSet[Area]()
    c.dslBody.walkStatements {
      case d: Data => d.refOwner match {
        case a: Area => {
          areas += a
        }
        case _ => {
          println(d.refOwner)
        }
      }
      case _ => {}
    }

    areas.toSet
  }

  class DefaultProperties(c : Component) extends HasFormalProperties {
    override protected def formalInputProperties(): Seq[FormalProperty] = ComponentWithFormalProperties.formalInputProperties(c)

    import c._
    lazy val mappedChildren = children.filter(!_.isInstanceOf[HasFormalProperties]).map(ComponentWithFormalProperties.DefaultProperties.apply) ++
      findInternalFormalProperties(c)

    override def formalChildren(): Seq[HasFormalProperties] = {
      getTagsOf[HasFormalAssertsTag]().map(_.formalAsserts).toList ++
        children.filter(_.isInstanceOf[HasFormalProperties]).map(_.asInstanceOf[HasFormalProperties]) ++
        mappedChildren
    }

    override protected def formalProperties(): Seq[FormalProperty] = {


      // By default we assume all children of this component are valid so we are only testing the logic around the
      // given component itself. Override to change this behavior. In particular when the component uses library components
      // which are tested directly, this tends to save a lot of time
      formalChildren().foreach(_.formalSetMinimumAssertionKind(assertionKind = AssertStatementKind.ASSERT))

      ComponentWithFormalProperties.formalCheckOutputs(c)
    }
  }

  object DefaultProperties {
    def apply(c : Component) = {
      val restore = Component.push(c)
      val defaultProperties = new DefaultProperties(c)
      handlers.foreach(_.lift(c))
      restore.restore()
      defaultProperties
    }
  }

  ComponentWithFormalProperties.AddHandler { case fifo: StreamFifo[Data] => new StreamFifoFormalProperties[Data](fifo) }
}

/**
 * This is a convience class that incorporates a lot of reasonable behaviors when adding properties to a Component.
 *
 * Mainly, it uses the attached IO bundle to populate the input and main properties of the component. This is typically
 * the bare minimum requires for a component -- it validates that the component in question will not violate the
 * contracts of it's IO, but does nothing else.
 */
class ComponentWithFormalProperties extends Component with HasFormalProperties {
  override def asFormalDut() = {
    formalConfigureForTest()
    super.asFormalDut()
  }

  lazy val mappedChildren = children.filter(!_.isInstanceOf[HasFormalProperties]).map(ComponentWithFormalProperties.DefaultProperties.apply) ++
    findInternalFormalProperties(this)

  override def formalChildren(): Seq[HasFormalProperties] = {
    getTagsOf[HasFormalAssertsTag]().map(_.formalAsserts).toList ++
      children.filter(_.isInstanceOf[HasFormalProperties]).map(_.asInstanceOf[HasFormalProperties]) ++
      mappedChildren
  }

  /**
   * Use anyseq on any input signals to this component which do not have existing assignments.
   */
  def anyseq_inputs(): Unit = {
    getAllIo.filter(_.isInput).filter(_.dlcIsEmpty).foreach(anyseq)
  }

  override protected def formalInputProperties(): Seq[FormalProperty] = ComponentWithFormalProperties.formalInputProperties(this)

  def formalComponentProperties() : Seq[FormalProperty] = Seq.empty

  lazy val contained_elements = new mutable.ArrayBuffer[Data]()
  override def valCallbackRec(ref: Any, name: String): Unit = {
    if(ref.isInstanceOf[Data]) {
      contained_elements.append(ref.asInstanceOf[Data])
    }
    super.valCallbackRec(ref, name)
  }

  override protected def formalProperties() = {
    formalChildren().foreach(_.formalSetMinimumAssertionKind(assertionKind = AssertStatementKind.ASSERT))

    ComponentWithFormalProperties.formalCheckOutputs(this) ++ formalComponentProperties() ++ contained_elements.flatMap(FormalData.formalIsStateValid)
  }

}
