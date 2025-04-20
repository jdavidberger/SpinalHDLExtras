package spinalextras.lib.formal

import spinal.core.formal.anyseq
import spinal.core.internals.AssertStatementKind
import spinal.core.{Component, Data, MultiData, True}
import spinal.lib.IMasterSlave

object ComponentWithFormalProperties {
  /**
   * The default implementation of formalInputProperties for components checks their IO bundle for bundles with the
   * formal trait and uses those as the input properties.
   *
   * @return The list of properties that are true when the input to this component are valid
   */
  protected def formalInputProperties(c : Component) = new FormalProperties {
    val flattenIOs = flattenIO(c)

    flattenIOs.map(fillins.findFillin).foreach({
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

  case class DefaultProperties(c : Component) extends HasFormalProperties {
    override protected def formalInputProperties(): Seq[FormalProperty] = ComponentWithFormalProperties.formalInputProperties(c)

    override protected def formalProperties(): Seq[FormalProperty] = {
      ComponentWithFormalProperties.formalCheckOutputs(c)
    }
  }
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


  private lazy val wrappedComponents = {
    children.filter(!_.isInstanceOf[HasFormalProperties]).foreach(ComponentWithFormalProperties.DefaultProperties)
  }

  override def formalChildren(): Seq[HasFormalProperties] = {
    getTagsOf[HasFormalAssertsTag]().map(_.formalAsserts).toList ++
      children.filter(_.isInstanceOf[HasFormalProperties]).map(_.asInstanceOf[HasFormalProperties])
  }

  /**
   * Use anyseq on any input signals to this component which do not have existing assignments.
   */
  def anyseq_inputs(): Unit = {
    getAllIo.filter(_.isInput).filter(_.dlcIsEmpty).foreach(anyseq)
  }

  override protected def formalInputProperties(): Seq[FormalProperty] = ComponentWithFormalProperties.formalInputProperties(this)

  override protected def formalProperties() = {
    // By default we assume all children of this component are valid so we are only testing the logic around the
    // given component itself. Override to change this behavior. In particular when the component uses library components
    // which are tested directly, this tends to save a lot of time
    formalChildren().foreach(_.formalSetMinimumAssertionKind(assertionKind = AssertStatementKind.ASSUME))

    ComponentWithFormalProperties.formalCheckOutputs(this)
  }

}
