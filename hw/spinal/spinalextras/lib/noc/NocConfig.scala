package spinalextras.lib.noc

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib.StreamArbiter.{FragmentLock, LowerFirst}
import spinal.lib._
import spinalextras.lib.logging.GlobalLogger
import spinalextras.lib.misc.Optional
import spinalextras.lib.noc.topology.Mesh
import spinalextras.lib.noc.virtualchannels._
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

import scala.language.postfixOps

sealed trait VirtualChannelArbitrationPolicy;
object RoundRobin extends VirtualChannelArbitrationPolicy
object LowestFirst extends VirtualChannelArbitrationPolicy

case class NocConfig(
                      topology           : Topology = new Mesh(),
                      dataWidth          : Int = 32,
                      virtualChannels    : Int = 2,
                      vcDepth            : Int = 2,
                      virtualChannelMode : VirtualChannelMode = Static,
                      virtualChannelArbitrationPolicy : VirtualChannelArbitrationPolicy = RoundRobin
                    ) {
  def headerApplicationBits = dataWidth - topology.addressSize
  def virtualChannelBits = log2Up(virtualChannels)
}

object NocConfig {
  def objectName(a : Any): String = {
    a.getClass.getSimpleName.replace("$", "")
  }
  def testConfigurations() = {
    for((name, topology) <- Topology.testConfigurations();
        virtualChannels <- Seq(1, 2, 3);
        virtualChannelMode <- Seq(Static, Dynamic);
        virtualChannelArbitrationPolicy <- Seq(RoundRobin, LowestFirst)
        ) yield
      f"${name}_vc${virtualChannels}_vcm${objectName(virtualChannelMode)}_vcp${objectName(virtualChannelArbitrationPolicy)}" ->
        NocConfig(topology = topology, virtualChannels = virtualChannels,
          virtualChannelMode = virtualChannelMode,
          virtualChannelArbitrationPolicy = virtualChannelArbitrationPolicy
        )
  }
}