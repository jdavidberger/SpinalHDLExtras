package spinalextras.lib.io

import spinal.core._
import spinal.lib._
import spinal.lib.io.TriState
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
    AddHandler { case Device("lattice", family, name, resetKind) => { _ => new LatticeTristateBuffer()}}
    AddHandler { case _ => { _ => new GenericTristate()}}
  }

  def apply() = factory(())
}

case class TristateBuffers[T <: BitVector](payloadType : HardType[T]) extends Component {
  val bitsWidth = payloadType.getBitsWidth
  setDefinitionName(s"Tristate_w${bitsWidth}")
  val io = new Bundle {
    val output_enable = in Bool()
    val output_enable_suppress = in(Bits(bitsWidth bits))

    val input = in (payloadType)
    val output = out (payloadType)
    val phy = inout(Analog(Bits(bitsWidth bits)))
  }

  for(i <- 0 until bitsWidth) {
    val bb = TristateBuffer()
    bb.setName(s"TristateBufferArray_${i}")
    bb.io.phy <> io.phy(i)
    bb.io.output_enable := io.output_enable && ~io.output_enable_suppress(i)
    bb.io.output <> io.output(i)
    bb.io.input <> io.input(i)
  }

}