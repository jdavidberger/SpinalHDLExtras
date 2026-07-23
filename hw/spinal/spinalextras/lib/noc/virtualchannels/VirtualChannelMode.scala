package spinalextras.lib.noc.virtualchannels

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib.StreamArbiter.{FragmentLock, LowerFirst}
import spinal.lib._
import spinalextras.lib.misc.Optional
import spinalextras.lib.noc.topology.Mesh
import spinalextras.lib.noc.{Flit, NocConfig}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

import scala.language.postfixOps

sealed trait VirtualChannelMode {
  def apply(cfg : NocConfig, ports : Int) : MapExtVcToVc
}
case object Static extends VirtualChannelMode {
  def apply(cfg : NocConfig, ports : Int) = new StaticVcMap(cfg, ports)
}
case object Dynamic extends VirtualChannelMode {
  def apply(cfg : NocConfig, ports : Int) = new DynamicVcMap(cfg, ports)
}


class MapExtVcToVc(cfg : NocConfig, ports : Int = 5) extends Component {
  val io = new Bundle {
    val inputs = Vec(
      Vec(
        slave(Stream(Fragment(Flit(cfg)))), cfg.virtualChannels
      ),
      ports
    )
    val output = Vec(master(Stream(Fragment(Flit(cfg)))), cfg.virtualChannels)
  }
}

class StaticVcMap(cfg : NocConfig, ports : Int = 5) extends MapExtVcToVc(cfg, ports) {
  (0 until cfg.virtualChannels).foreach(idx => {
    val arbiter = new StreamArbiter(io.inputs.map(_(idx)).head.payloadType, io.inputs.map(_(idx)).size, arbitrationPolicy = LowerFirst, lockPolicy = FragmentLock)
    arbiter.setName(s"static_vcid_${idx}_arbiter")
    (arbiter.io.inputs, io.inputs.map(_(idx))).zipped.foreach(_ << _)
    arbiter.io.output.combStage() <> io.output(idx)
  })
}

class DynamicVcMap(cfg : NocConfig, ports : Int = 5) extends MapExtVcToVc(cfg, ports) {
  io.inputs.map(_.map(_.setBlocked()))
  io.output.map(_.setIdle())

  // Mappings from the external ports VCID into our internal VCID
  val extVcToVc = Vec(Vec(RegInit(Optional.Empty(UInt(log2Up(cfg.virtualChannels) bits))), cfg.virtualChannels), ports)

  // Mapping of internal VCID back to the (port, external_vcid) tuple
  val vcToExtVc = Vec(RegInit(Optional.Empty(TupleBundle(
    UInt(log2Up(ports) bits),
    UInt(log2Up(cfg.virtualChannels) bits)))
  ), cfg.virtualChannels)

  // Show the next channel up for allocation
  var availableChannel = Optional.Empty(UInt(log2Up(cfg.virtualChannels) bits))
  vcToExtVc.zipWithIndex.map { case (vc, idx) => {
    when(!vc.has_value) {
      availableChannel.has_value := True
      availableChannel.value := idx
    }
  }}

  var wc = when(availableChannel.has_value === False){}
  for (port <- 0 until ports) {
    for (external_vc <- 0 until cfg.virtualChannels) {
      val vcStream = io.inputs(port)(external_vc)
      val extVc = extVcToVc(port)(external_vc)
      wc = wc elsewhen(vcStream.valid && !extVc.has_value) {
        extVc.set_value(availableChannel.value)
        vcToExtVc(availableChannel.value).has_value := True
        vcToExtVc(availableChannel.value).value._1 := U(port).resized
        vcToExtVc(availableChannel.value).value._2 := U(external_vc).resized
      }
    }
  }


  (0 until cfg.virtualChannels).foreach(internal_vcid => {
    val extVcPort = vcToExtVc(internal_vcid).value._1
    val extVcId = vcToExtVc(internal_vcid).value._2

    when(io.output(internal_vcid).lastFire) {
      vcToExtVc(internal_vcid).clear()
      extVcToVc(extVcPort)(extVcId).clear()
    }

    when(vcToExtVc(internal_vcid).has_value) {
      assert(extVcToVc(extVcPort)(extVcId).has_value)
      assert(extVcToVc(extVcPort)(extVcId).value === U(internal_vcid).resized)

      io.inputs(extVcPort)(extVcId).map(flit => {
        val oflit = cloneOf(flit)
        oflit.datum := flit.datum
        oflit.vc := U(internal_vcid).resized
        oflit.last := flit.last
        oflit
      }) <> io.output(internal_vcid)
    }
  })
}


class VCStaticMapFormalTester extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 10

  formalTests().foreach(t => test(t._1) {
    t._2()
  })

  override def generateRtl() = {
    Seq(
      (s"Basic", () =>
        GeneralFormalDut(() => new StaticVcMap(cfg = NocConfig(topology = new Mesh((3, 2))))))
    )
  }
}


class VCDynamicMapFormalTester extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 10

  formalTests().foreach(t => test(t._1) {
    t._2()
  })

  override def generateRtl() = {
    Seq(
      (s"Basic", () =>
        GeneralFormalDut(() => new DynamicVcMap(cfg = NocConfig(topology = new Mesh((3, 2))), ports = 5)))
    )
  }
}