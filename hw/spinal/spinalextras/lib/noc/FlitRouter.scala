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

  // Already expressed in the connectivityOut - 1-sized, inputPort-excluded
  // numbering -- resolveDestPort guarantees this never points back through
  // inputPort, so no self-redirect or compaction is needed here.
  val outputNode = RegInit(Optional.Empty(UInt(log2Up(connectivityOut - 1) bits)))
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

    outputNode.set_value(cfg.topology.resolveDestPort(hdr.dest, address, inputPort))
    io.activity := True
    //report(Seq("Start Address: ", address, " ", cfg.topology.addressName(address), " dst ", hdr.dest, " app ", hdr.application, " vcid ", idx, " output ", outputNode))
    nocOutput.valid := True
    nocOutput._1 := hdr
    nocOutput._2 := vcid
  }

  when(outputNode.has_value) {
    if (outputNode.value.maxValue >= connectivityOut - 1) {
      assert(outputNode.value < connectivityOut - 1)
    }
  }

  StreamDemux(input.continueWhen(outputNode.has_value), outputNode.value, connectivityOut - 1) <> io.output
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