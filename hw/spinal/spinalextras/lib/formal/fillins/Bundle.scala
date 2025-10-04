package spinalextras.lib.formal.fillins

import spinal.core.{Bundle, Data}
import spinalextras.lib.formal.{FormalData, FormalProperty}

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