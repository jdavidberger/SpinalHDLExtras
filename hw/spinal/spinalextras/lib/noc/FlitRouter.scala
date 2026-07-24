package spinalextras.lib.noc

import spinal.core._
import spinal.lib._
import spinalextras.lib.logging._
import spinalextras.lib.misc.Optional

import scala.language.postfixOps

class FlitRouter(cfg: NocConfig, address: Int, inputPort: Int, vcid: Int, connectivityOut: Int) extends Component {
  val io = new Bundle {
    val input = slave(Stream(Fragment(Bits(cfg.dataWidth bits))))
    val output = master(Stream(Fragment(RoutedFlit(cfg, connectivityOut))))

    val activity = out(Bool())
  }

  val vc = RegInit(Optional.Empty(UInt(log2Up(connectivityOut) bits)))
  val input = io.input

  val nocOutput = Flow(TupleBundle(Header(cfg), UInt(log2Up(cfg.virtualChannels) bits)))
  nocOutput.setName(s"noc_output_${address}").setIdle()

  io.activity := False

  GlobalLogger(
    Set("noc-router", "router-mode", s"router-mode-${address}"),
    FlowLogger.flows(nocOutput)
  )

  when(vc.has_value) {
    when(input.lastFire) {
      //report(Seq("Finish Address: ", address, " ", cfg.topology.addressName(address), " vcid ", idx))
      vc.clear()
    }
  } elsewhen (input.valid) {
    val hdr = Header(cfg)
    hdr.assignFromBits(input.payload.fragment)
    val outputNode = cfg.topology.resolveDestPort(hdr.dest, address)
    vc.set_value(Mux(outputNode === inputPort, 0, outputNode))
    io.activity := True
    //report(Seq("Start Address: ", address, " ", cfg.topology.addressName(address), " dst ", hdr.dest, " app ", hdr.application, " vcid ", idx, " output ", outputNode))
    nocOutput.valid := True
    nocOutput._1 := hdr
    nocOutput._2 := vcid
  }

  when(vc.has_value) {
    if (vc.value.maxValue >= connectivityOut) {
      assert(vc.value < connectivityOut)
    }
  }

  input.continueWhen(vc.has_value).map(flit => {
    val routedFlit = Fragment(new RoutedFlit(cfg, connectivityOut))
    routedFlit.last := flit.last
    routedFlit.flit.datum := flit.fragment
    routedFlit.flit.vc := vcid
    routedFlit.routedNode := vc.value
    routedFlit
  }) <> io.output
}

object FlitRouter {
  def apply(node: RouterNode, inputPort: Int, vcid: Int, input: Stream[Fragment[Bits]]): Stream[Fragment[RoutedFlit]] = {
    val router = new FlitRouter(node.cfg, node.address, inputPort = inputPort, vcid = vcid, connectivityOut = node.connectivityOut)
    router.setName(s"flit_router_p${inputPort}_v${vcid}")
    node.routerActivity(vcid)(inputPort) := router.io.activity
    router.io.input <> input
    router.io.output
  }
}