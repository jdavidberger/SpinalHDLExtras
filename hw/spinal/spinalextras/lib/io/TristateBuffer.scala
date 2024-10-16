package spinalextras.lib.io

import spinal.core._
import spinal.lib._
import spinalextras.lib.impl.ImplementationSpecificFactory
import spinalextras.lib.io.lattice.LatticeTristateBuffer

import scala.language.postfixOps

abstract class TristateBuffer() extends Component {
  val io = new Bundle {
    val input, output_enable = in Bool()
    val output = out Bool()
    val phy = inout(Analog(Bool()))
  }
}

class GenericTristate() extends TristateBuffer {
  io.output := io.phy
  when(io.output_enable) {
    io.phy := io.input
  }
}

object TristateBuffer {
  def factory = new ImplementationSpecificFactory[TristateBuffer, Unit] {
    AddHandler { case _ => { _ => new GenericTristate()}}
    AddHandler { case Device("lattice", family, name, resetKind) => { _ => new LatticeTristateBuffer()}}
  }

  def apply() = factory(())
}

case class TristateBufferArray[T <: BitVector](payloadType : HardType[T]) extends Component {
  val bitsWidth = payloadType.getBitsWidth
  setDefinitionName(s"Tristate_w${bitsWidth}")
  val io = new Bundle {
    val output_enable = in Bool()
    val input = in (payloadType)
    val output = out (payloadType)
    val phy = inout(Analog(Bits(bitsWidth bits)))
  }

  for(i <- 0 until bitsWidth) {
    val bb = TristateBuffer()
    bb.setName(s"TristateBufferArray_${i}")
    bb.io.phy <> io.phy(i)
    bb.io.output_enable := io.output_enable
    bb.io.output <> io.output(i)
    bb.io.input <> io.input(i)
  }

}