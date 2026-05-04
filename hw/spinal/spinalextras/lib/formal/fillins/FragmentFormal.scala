package spinalextras.lib.formal

import spinal.core._
import spinal.lib._
import spinalextras.lib.formal.fillins.{EquivalenceRegistry, HasDefinedEquivalence}

package object FragmentFormal {
  implicit class FragmentExt[T <: Data](val fragment: Fragment[T]) extends FormalData with HasDefinedEquivalence {
    override def underlyingData: Data = fragment

    override type Self = this.type

    /**
     * @return Whether or not the current state of the bundle is valid. Typically either asserted or assumed by a
     *         component which has this bundle as an input or an output.
     *
     *         For complicated properties, consider using the helper class `FormalProperties`
     */
    override def formalIsStateValid(): Seq[FormalProperty] = FormalData.formalIsStateValid(fragment.fragment)

    override def IsEquivalent(b: FragmentExt.this.type): Bool = {
      fragment.last === b.fragment.last && EquivalenceRegistry.Check(fragment.fragment, b.fragment.fragment)
    }
  }
}

