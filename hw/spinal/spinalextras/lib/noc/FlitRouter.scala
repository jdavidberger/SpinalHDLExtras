package spinalextras.lib.noc

import spinal.core._
import spinal.lib._
import spinalextras.lib.logging._
import spinalextras.lib.misc.Optional

import scala.language.postfixOps

class FlitRouter(val cfg: NocConfig, address: Int, inputPort: Topology.canonical_port) extends Component {
  val outputNodeIndices = cfg.topology.nodePortIndicesForCanonicalPorts(address, inputPort)
  val connectivityOut = outputNodeIndices.size

  val io = new Bundle {
    val input = slave(Stream(Fragment(Bits(cfg.dataWidth bits))))
    val output = Vec(master(Stream(Fragment(Bits(cfg.dataWidth bits)))), connectivityOut)

    val activity = out(Bool())
  }

  // Already expressed in the connectivityOut-sized, inputPort-excluded
  // numbering -- resolveDestPort guarantees this never points back through
  // inputPort, so no self-redirect or compaction is needed here.
  val outputNode = RegInit(Optional.Empty(UInt(log2Up(connectivityOut) bits)))
  val input = io.input

  io.activity := False

  when(outputNode.has_value) {
    when(input.lastFire) {
      //report(Seq("Finish Address: ", address, " ", cfg.topology.addressName(address), " vcid ", idx))
      outputNode.clear()
    }
  } elsewhen (input.valid) {
    val hdr = Header(cfg)
    hdr.assignFromBits(input.payload.fragment)

    outputNode.set_value(cfg.topology.resolveDestPort(hdr.dest, address, inputPort))
    io.activity := True
    //report(Seq("Start Address: ", address, " ", cfg.topology.addressName(address), " dst ", hdr.dest, " app ", hdr.application, " vcid ", idx, " output ", outputNode))
  }

  when(outputNode.has_value) {
    if (outputNode.value.maxValue >= connectivityOut) {
      assert(outputNode.value < connectivityOut)
    }
  }

  StreamDemux(input.continueWhen(outputNode.has_value), outputNode.value, connectivityOut) <> io.output
}

object FlitRouter {
  def apply(node: RouterNode, inputPort: Topology.canonical_port, vcid: Int, input: Stream[Fragment[Bits]]): Vec[Stream[Fragment[Bits]]] = {
    val router = new FlitRouter(node.cfg, node.address, inputPort = inputPort)
    router.setName(s"flit_router_${node.address}_p${node.cfg.topology.portName(inputPort)}_v${vcid}")
    router.io.input <> input
    router.io.output
  }
}