package spinalextras.lib.blackbox.lattice.lifcl

import spinal.core._

import scala.language.postfixOps

case class BUF() extends BlackBox {
  val io = new Bundle {
    val A = in Bool()
    val Z = out Bool()
  }
  noIoPrefix()
}

object BUF {
  def apply(A: Bool): Bool = {
    val buf = BUF().addAttribute("keep_hierarchy", "TRUE")
    buf.setPartialName(s"${A.name}_buffer")
    buf.io.A := A
    CombInit(buf.io.Z).addAttribute("syn_keep", 1).addAttribute("nomerge", "")
  }
  def apply(A: Bool, stages : Int): Bool = {
    if(stages == 0)
      return A

    var z = A;
    for(i <- 0 until stages) {
      z = BUF(z)
      z.setPartialName(s"${A.name}_buf${i}")
    }
    z
  }

  def apply[T <: Data](A: T, stages : Int = 1): T = {
    if(stages == 0)
      return A

    val z = cloneOf(A)

    val b = Bits(A.getBitsWidth bits)
    for(i <- 0 until A.getBitsWidth) {
      val in = A.asBits(i)
      in.setPartialName(s"${A.name}_${i}", true)
      b(i) := BUF(in, stages)
    }
    z.assignFromBits(b)
    z.setPartialName(s"${A.name}_buf", true)

    z
  }
}