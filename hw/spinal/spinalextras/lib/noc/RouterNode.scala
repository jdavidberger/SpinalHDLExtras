package spinalextras.lib.noc

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib.StreamArbiter.{LowerFirst, TransactionLock}
import spinal.lib._
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties, FormalProperty}
import spinalextras.lib.logging.{FlowLogger, GlobalLogger}
import spinalextras.lib.misc.Optional
import spinalextras.lib.misc.StreamTools.CreateFragment
import spinalextras.lib.noc.topology.{Mesh, Ring, Tree}
import spinalextras.lib.noc.virtualchannels.{Dynamic, VirtualIdAllocator}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

case class RoutedFlit(cfg : NocConfig, connectivityOut : Int) extends Bundle {
  val flit = Flit(cfg)
  val routedNode = UInt(log2Up(connectivityOut) bits)
}

class RouterNode(cfg: NocConfig, address: Int) extends ComponentWithFormalProperties {
  var connectivityIn : Int = cfg.topology.nodePortIndicesForCanonicalPorts(address).size
  var connectivityOut : Int = connectivityIn

  val io = new Bundle {
    val inputs = Array.fill(connectivityIn)(slave(Stream(Fragment(Flit(cfg)))))
    val outputs = Vec(master(Stream(Fragment(Flit(cfg)))), connectivityOut)
  }

  def inputs(canonicalPort : Int): Stream[Fragment[Flit]] = {
    io.inputs(cfg.topology.resolveCanonicalInputPort(address, canonicalPort))
  }

  def outputs(canonicalPort : Int): Stream[Fragment[Flit]] = {
    io.outputs(cfg.topology.resolveCanonicalOutputPort(address, canonicalPort))
  }

  val inputPorts = io.inputs.map(InputPort(_))
  val outputPorts = io.outputs.map(OutputPort(_))

  for (port <- 0 until connectivityIn; vc <- 0 until cfg.virtualChannels) {
    val isStart = RegInit(True)
    val portFlowFire = inputPorts(port).io.outputs(vc).toFlowFire
    isStart := portFlowFire.lastFire

    GlobalLogger(
      Set("noc-router", "router-input", s"router-input-${address}", s"router-input-${port}-${vc}"),
      FlowLogger.flows(portFlowFire.takeWhen(isStart).map(x => {
        TupleBundle(x.fragment, U(port, log2Up(connectivityIn) bits), U(vc, cfg.virtualChannelBits bits))
      }).setName(s"noc_input_p${port}_vc${vc}"))
    )
  }

  val nocOutput = Flow(TupleBundle(Header(cfg), UInt(log2Up(cfg.virtualChannels) bits)))
  nocOutput.setName(s"noc_output_${address}").setIdle()

  GlobalLogger(
    Set("noc-router", "router-mode", s"router-mode-${address}"),
    FlowLogger.flows(nocOutput)
  )

  // Allocate one downstream (destNode, destVc) slot per packet and route the
  // granted flits to the matching output port. This replaces the old
  // Static/Dynamic VcMap, which only arbitrated per-VC without any output-slot
  // locking.
  val allocator = new VirtualIdAllocator(
    cfg          = cfg,
    connectivityIn  = connectivityIn,
    connectivityOut = connectivityOut,
    dynamicAllocation     = cfg.virtualChannelMode == Dynamic,
    roundRobinArbitration = cfg.virtualChannelArbitrationPolicy == RoundRobin
  )

  for (inputPort <- 0 until connectivityIn; vcid <- 0 until cfg.virtualChannels) {
    val vc = RegInit(Optional.Empty(UInt(log2Up(connectivityOut) bits)))

    val vcStreams = inputPorts(inputPort).io.outputs(vcid)
      when(vc.has_value) {
        when(vcStreams.lastFire) {
          //report(Seq("Finish Address: ", address, " ", cfg.topology.addressName(address), " vcid ", idx))
          vc.clear()
        }
      } elsewhen (vcStreams.valid) {
        val hdr = Header(cfg)
        hdr.assignFromBits(vcStreams.payload.fragment)
        val outputNode = cfg.topology.resolveDestPort(hdr.dest, address)
        vc.set_value(outputNode)

        //report(Seq("Start Address: ", address, " ", cfg.topology.addressName(address), " dst ", hdr.dest, " app ", hdr.application, " vcid ", idx, " output ", outputNode))
        nocOutput.valid := True
        nocOutput._1 := hdr
        nocOutput._2 := vcid
      }

      when(vc.has_value) {
        if(vc.value.maxValue >= connectivityOut) {
          assert(vc.value < connectivityOut)
        }
      }

      vcStreams.continueWhen(vc.has_value).map(flit => {
        val routedFlit = Fragment(new RoutedFlit(cfg, connectivityOut))
        routedFlit.last := flit.last
        routedFlit.flit.datum := flit.fragment
        routedFlit.flit.vc := vcid
        routedFlit.routedNode := vc.value
        routedFlit
      }) <> allocator.io.routedFlits(inputPort)(vcid)
  }

  for (o <- 0 until connectivityOut; v <- 0 until cfg.virtualChannels) {
    allocator.io.allocatedFlits(o)(v) <> outputPorts(o).io.inputs(v)
  }

  override def formalComponentProperties(): Seq[FormalProperty] = new FormalProperties(this) {

  }
}


class NocRouterFormalTester extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 2

  formalTests().foreach(t => test(t._1) {
    t._2()
  })

  override def generateRtl() = {
    for((name, cfg) <- NocConfig.testConfigurations()) yield
      (name, () => GeneralFormalDut(() => new RouterNode(cfg, 0)))
  }
}


class OutputPort(cfg : NocConfig) extends Component {
  val io = new Bundle {
    val inputs = Vec(slave(Stream(Fragment(Flit(cfg)))), cfg.virtualChannels)
    val output = master(Stream(Fragment(Flit(cfg))))
  }

  StreamArbiterFactory().lowerFirst.transactionLock.on(io.inputs) <> io.output
}

object OutputPort {
  def apply(i: Stream[Fragment[Flit]]) = {
    val port = new OutputPort(i.payload.fragment.cfg)
    port.io.output <> i
    port
  }
}

class InputPort(cfg : NocConfig) extends Component {
  val io = new Bundle {
    val input = slave(Stream(Fragment(Flit(cfg))))

    val outputs =
      Vec(master(Stream(Fragment(Bits(cfg.dataWidth bits)))), cfg.virtualChannels)
  }

  val fifos = Array.fill(cfg.virtualChannels)(StreamFifo(
    Fragment(Bits(cfg.dataWidth bits)),
    cfg.vcDepth
  ))

  StreamDemux(io.input, io.input.payload.vc, cfg.virtualChannels).zip(fifos).foreach(x => {
    x._1.map(x => CreateFragment(x.datum, x.last)) <> x._2.io.push
  })
  io.outputs.zip(fifos).foreach(x => x._1 <> x._2.io.pop)
}

object InputPort {
  def apply(i: Stream[Fragment[Flit]]) = {
    val port = new InputPort(i.payload.fragment.cfg)
    port.io.input <> i
    port
  }
}

class InputPortFormalTester extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 10

  formalTests().foreach(t => test(t._1) {
    t._2()
  })

  override def generateRtl() = {
    Seq(
      (s"Basic", () =>
        GeneralFormalDut(() => new InputPort(cfg = NocConfig(topology = new Mesh((4, 3))))))
    )
  }
}