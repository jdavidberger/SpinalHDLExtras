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

case class MULTI_BUF[T <: Data](dataType : HardType[T], stages : Int) extends Component {
  setDefinitionName(s"MULTI_BUF_${dataType.getBitsWidth}w_${stages}s")

  addAttribute("syn_keep")
  addAttribute("dont_touch")

  val io = new Bundle {
    val A = in (dataType())
    val Z = out(dataType())
  }

  var c = BUF.keep_wire(io.A.asBits)

  for(i <- 0 until stages) {
    val n = Bits(io.A.getBitsWidth bits)
    for(b <- 0 until c.getBitsWidth) {
      n(b) := BUF(c(b))
    }
    c = BUF.keep_wire(n)
  }

  io.Z.assignFromBits(c)
}

object BUF {
  def keep_wire[T <: Data](B : T): T = {
    CombInit(B).addAttribute("nomerge", 1).addAttribute("keep", 1).addAttribute("syn_keep", 1)
  }
  def apply(A: Bool): Bool = {
    val buf = BUF()
    buf.setPartialName(s"${A.name}_buffer")
    buf.addAttribute("keep", 1)
    buf.io.A := BUF.keep_wire(A)
    BUF.keep_wire(buf.io.Z)
  }
  def apply(A: Bool, stages : Int): Bool = {
    if(stages == 0)
      return A

    var z = A;
    for(i <- 0 until stages) {
      z = BUF(z)
    }
    z
  }

  def apply[T <: Data](A: T, stages : Int = 1): T = {
    val dut = MULTI_BUF(A, stages)
    dut.io.A := keep_wire(A)
    keep_wire(dut.io.Z)
  }
}