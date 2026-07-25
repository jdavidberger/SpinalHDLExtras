package spinalextras.lib.noc

import spinal.core._
import spinal.lib._
import spinalextras.lib.logging._
import spinalextras.lib.misc.Optional

import scala.language.postfixOps

class FlitRouter(cfg: NocConfig, address: Int, inputPort: Int, vcid: Int, connectivityOut: Int) extends Component {
  val io = new Bundle {
    val input = slave(Stream(Fragment(Bits(cfg.dataWidth bits))))
    // One stream per possible destination port, excluding inputPort itself --
    // a port never routes back through itself, so that slot doesn't exist.
    val output = Vec(master(Stream(Fragment(Bits(cfg.dataWidth bits)))), connectivityOut - 1)

    val activity = out(Bool())
  }

  val outputNode = RegInit(Optional.Empty(UInt(log2Up(connectivityOut) bits)))
  val input = io.input

  val nocOutput = Flow(TupleBundle(Header(cfg), UInt(log2Up(cfg.virtualChannels) bits)))
  nocOutput.setName(s"noc_output_${address}").setIdle()

  io.activity := False

  GlobalLogger(
    Set("noc-router", "router-mode", s"router-mode-${address}"),
    FlowLogger.flows(nocOutput)
  )

  when(outputNode.has_value) {
    when(input.lastFire) {
      //report(Seq("Finish Address: ", address, " ", cfg.topology.addressName(address), " vcid ", idx))
      outputNode.clear()
    }
  } elsewhen (input.valid) {
    val hdr = Header(cfg)
    hdr.assignFromBits(input.payload.fragment)
    val resolvedOutputNode = cfg.topology.resolveDestPort(hdr.dest, address)

    // It would not make sense for resolvedOutputNode to be the current input node, so just divert these to LOCAL. This
    // is largely an optimization so that wires are optimized out which would be loopbacks.
    outputNode.set_value(Mux(resolvedOutputNode === inputPort, 0, resolvedOutputNode))
    io.activity := True
    //report(Seq("Start Address: ", address, " ", cfg.topology.addressName(address), " dst ", hdr.dest, " app ", hdr.application, " vcid ", idx, " output ", outputNode))
    nocOutput.valid := True
    nocOutput._1 := hdr
    nocOutput._2 := vcid
  }

  when(outputNode.has_value) {
    if (outputNode.value.maxValue >= connectivityOut) {
      assert(outputNode.value < connectivityOut)
    }
  }

  // Compact the resolved destination port into [0, connectivityOut - 2] by
  // dropping inputPort's slot from the numbering, then demux the flit stream
  // directly into the per-destination output vector.
  val compactedOutputNode = Mux(outputNode.value > inputPort, outputNode.value - 1, outputNode.value)
    .resize(log2Up(connectivityOut - 1) bits)

  StreamDemux(input.continueWhen(outputNode.has_value), compactedOutputNode, connectivityOut - 1) <> io.output
}

object FlitRouter {
  def apply(node: RouterNode, inputPort: Int, vcid: Int, input: Stream[Fragment[Bits]]): Vec[Stream[Fragment[Bits]]] = {
    val router = new FlitRouter(node.cfg, node.address, inputPort = inputPort, vcid = vcid, connectivityOut = node.connectivityOut)
    router.setName(s"flit_router_p${inputPort}_v${vcid}")
    node.routerActivity(vcid)(inputPort) := router.io.activity
    router.io.input <> input
    router.io.output
  }
}