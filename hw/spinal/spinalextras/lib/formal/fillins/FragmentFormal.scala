package spinalextras.lib.formal

import spinal.core._
import spinal.lib._

package object FragmentFormal {
  implicit class FragmentExt[T <: Data](fragment: Fragment[T]) extends FormalData {

    override type Self = this.type

    /**
     * @return Whether or not the current state of the bundle is valid. Typically either asserted or assumed by a
     *         component which has this bundle as an input or an output.
     *
     *         For complicated properties, consider using the helper class `FormalProperties`
     */
    override def formalIsStateValid(): Seq[FormalProperty] = FormalData.formalIsStateValid(fragment.fragment)
  }
}

