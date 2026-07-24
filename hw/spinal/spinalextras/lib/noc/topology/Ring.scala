package spinalextras.lib.noc.topology

import spinal.core._
import spinalextras.lib.noc.topology.Ring.{ClockWise, CounterClockWise, Local}
import spinalextras.lib.noc.virtualchannels.{Dynamic, GrantTable}
import spinalextras.lib.noc.{NoC, NocConfig, RouterNode, Topology}

object Ring {
  val Local = 0
  val ClockWise = 1
  val CounterClockWise = 2

  def apply(dest: UInt, curr: Int, size: Int): UInt = {
    val R = UInt(2 bits)
    val delta = (dest.intoSInt - curr)
    val direction = delta > 0
    val wrap = delta >= S(size / 2, delta.getBitsWidth bits)

    when(dest === curr) {
      R := 0
    } elsewhen (direction ^ wrap) {
      R := ClockWise
    } otherwise {
      R := CounterClockWise
    }
    R
  }
}
class Ring(size: Int = 0) extends Topology {
  def defaultConnectivityIn : Int = 3

  override def nodes: Int = size

  override def sizeFor(nodes: Int): Topology = new Ring(nodes)

  override def resolveDestPort(dest: UInt, curr: Int): UInt = {
    Ring(dest, curr, size)
  }

  override def nodePortIndicesForCanonicalPorts(address: Int): Seq[Int] = (0 until maxCanonicalPorts)
  override def resolveNeighborAddress(address: Int, canonicalPort: Int): (Int, Int) = {
    val dx = if (canonicalPort == CounterClockWise) -1 else canonicalPort
    ((address + dx + size) % size, canonicalPort match {
      case CounterClockWise => ClockWise
      case ClockWise => CounterClockWise
      case Local => Local
    })
  }

  // The ring has one physical cycle, so a purely adaptive ("any free lane")
  // Dynamic assignment can deadlock: concurrent packets can saturate every
  // lane around the loop with nothing able to advance. Fix: reserve the top
  // vc index as a sticky escape lane, entered only at the one designated
  // dateline edge (address == size-1 going ClockWise, or address == 0 going
  // CounterClockWise -- the same physical wraparound edge from each side).
  // Ordinary (non-dateline) hops keep the rest of the vcs as a fully
  // adaptive pool; a packet that has already escaped stays pinned to the
  // escape lane on every subsequent hop. Static is left to the default
  // (diagonal) behavior.
  override def allowedTransitionTable(cfg: NocConfig, port: (address_t, canonical_port),
                                       candidateCount: Int, vcCount: Int): Seq[Seq[Boolean]] = {
    cfg.virtualChannelMode match {
      case Dynamic if vcCount >= 2 =>
        val (address, canonicalPort) = port
        val escapeVc = vcCount - 1
        val isDateline = (address == size - 1 && canonicalPort == ClockWise) ||
                         (address == 0 && canonicalPort == CounterClockWise)

        Seq.tabulate(candidateCount) { c =>
          val inputPort = c / vcCount
          val sourceVc = c % vcCount
          // A candidate's incoming vc tag only means "already escaped" if it
          // was actually assigned that way by an upstream router's own
          // allowedTransitionTable. On the Local port it's whatever the
          // injecting source arbitrarily picked -- not evidence of a real
          // dateline crossing -- so it must not be honored as sticky here;
          // otherwise a packet could spawn straight into the escape lane and
          // later use the dateline edge as an ordinary continuing hop rather
          // than a one-time transition, reopening the same deadlock.
          val alreadyEscaped = sourceVc == escapeVc && inputPort != Local

          if (isDateline || alreadyEscaped)
            Seq.tabulate(vcCount)(_ == escapeVc)      // forced/sticky escape
          else
            Seq.tabulate(vcCount)(_ != escapeVc)      // adaptive pool, escape excluded
        }
      case _ => super.allowedTransitionTable(cfg, port, candidateCount, vcCount)
    }
  }
}