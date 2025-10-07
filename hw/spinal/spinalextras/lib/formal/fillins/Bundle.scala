package spinalextras.lib.formal.fillins

import spinal.core.{Bundle, Data}
import spinal.lib.IMasterSlave
import spinalextras.lib.formal.{FormalData, FormalMasterSlave, FormalProperty}

class BundleExt(data : Bundle) extends FormalData {

  /**
   * @return Whether or not the current state of the bundle is valid. Typically either asserted or assumed by a
   *         component which has this bundle as an input or an output.
   *
   *         For complicated properties, consider using the helper class `FormalProperties`
   */
  override def formalIsStateValid(): Seq[FormalProperty] = {
    val formals = data.elements.map(x => findFillin(x._2)).filter(_.isInstanceOf[FormalData]).map(_.asInstanceOf[FormalData])
    formals.flatMap(_.formalIsStateValid())
  }

  override def underlyingData: Data = data
}


class IMasterSlaveExt(data : Bundle with IMasterSlave) extends FormalMasterSlave {
  private lazy val elementsWithProperties = data.elements.map(x => findFillin(x._2))

  override def underlyingData: Data = data

  /**
   * @return True if and only if the driving signals are valid
   */
  override def formalIsProducerValid(): Seq[FormalProperty] = {
    elementsWithProperties.flatMap {
      case ims: FormalMasterSlave => ims.formalIsProducerValid()
      case formalData: FormalData => formalData.formalIsStateValid()
      case _ => Seq()
    }
  }

  override def formalIsConsumerValid(): Seq[FormalProperty] = {
    elementsWithProperties.flatMap {
      case ims: FormalMasterSlave => ims.formalIsConsumerValid()
      case _ => Seq()
    }
  }

  override def asIMasterSlave = data
}