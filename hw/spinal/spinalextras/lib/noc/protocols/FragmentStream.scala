package spinalextras.lib.noc.protocols

import spinal.core._
import spinal.lib._
import spinalextras.lib.noc.{Flit, Header, NocConfig}

import scala.language.postfixOps

class FragmentStreamInput[T <: Data](payloadType : HardType[T], cfg : NocConfig) extends Component {
  val io = new Bundle {
    val input = slave(Stream(Fragment(payloadType)))

    val dest = in(UInt(cfg.topology.addressSize bits))
    val vcid = in(UInt(cfg.virtualChannelBits bits)) default(0)

    val output = master(Stream(Fragment(Flit(cfg))))
  }

  val header = Header(cfg)
  header.dest := io.dest
  header.application.setAll()

  io.input.toFragmentBits(payloadType.getBitsWidth).insertHeader(header.asBits.resized).map(x => {
    val flit = Fragment(Flit(cfg))
    flit.datum := x.fragment
    flit.vc := io.vcid
    flit.last := x.last
    flit
  }) <> io.output
}