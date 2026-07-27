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
                      virtualChannelArbitrationPolicy : VirtualChannelArbitrationPolicy = RoundRobin,
                    ) {
  def headerApplicationBits = dataWidth - topology.addressSize
  def virtualChannelBits = log2Up(virtualChannels)
  def datatype = Bits(dataWidth bits)

  def packHeader(dest: UInt, subheader: UInt): Bits = {
    val header = Header(this)
    header.dest := dest
    header.application := B(0, headerApplicationBits bits)
    header.application(topology.addressSize - 1 downto 0) := subheader.asBits
    header.asBits.resized
  }
}

object NocConfig {
  def objectName(a : Any): String = {
    a.getClass.getSimpleName.replace("$", "")
  }
  def testConfigurations() = {
    // NB: vcDepth = 1 was tried here too (tightest possible buffering, so
    // most likely to expose a cyclic channel dependency as a hard deadlock)
    // but it doesn't even elaborate on Ring/Torus: spinal.lib.StreamFifo's
    // depth-1 case bypasses to a purely combinational ready path, and
    // chained all the way around a physical cycle with no register
    // anywhere breaking it, that's a genuine RTL combinational loop --
    // independent of vcCount/mode/policy. That's a separate, real bug, not
    // something to fold into this suite silently, so vcDepth is left at the
    // default for now.
    (for((name, topology) <- Topology.testConfigurations();
        virtualChannels <- Seq(1, 2, 4);
        virtualChannelMode <- Seq(Static, Dynamic);
        virtualChannelArbitrationPolicy <- Seq(RoundRobin, LowestFirst)
        ) yield
      f"${name}_vc${virtualChannels}_vcm${objectName(virtualChannelMode)}_vcp${objectName(virtualChannelArbitrationPolicy)}" ->
        NocConfig(topology = topology,
          dataWidth = 16,
          virtualChannels = virtualChannels,
          virtualChannelMode = virtualChannelMode,
          virtualChannelArbitrationPolicy = virtualChannelArbitrationPolicy
        )
      ).filter(cfg => cfg._2.virtualChannels >= cfg._2.topology.minimumVirtualChannels)
  }
}