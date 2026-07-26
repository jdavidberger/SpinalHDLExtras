package spinalextras.lib.noc.topology

import spinal.core._
import spinalextras.lib.noc.Topology.canonical_port
import spinalextras.lib.noc.topology.Ring.{ClockWise, CounterClockWise, Local}
import spinalextras.lib.noc.virtualchannels.{Dynamic, Static}
import spinalextras.lib.noc.{NoC, NocConfig, RouterNode, Topology}

import scala.language.postfixOps

object Ring {
  val Local = 0
  val ClockWise = 1
  val CounterClockWise = 2

  def apply(dest: UInt, curr: Int, size: Int, setResult : canonical_port => Unit ): Unit = {
    val delta = (dest.intoSInt - curr)
    val direction = delta > 0
    val wrap = delta >= S(size / 2, delta.getBitsWidth bits)

    when(dest === curr) {
      setResult(0)
    } elsewhen (direction ^ wrap) {
      setResult(ClockWise)
    } otherwise {
      setResult(CounterClockWise)
    }
  }
  def apply(dest: UInt, curr: Int, size: Int): UInt = {
    val R = UInt(2 bits)
    apply(dest, curr, size, p => R := p)
    R
  }
}

sealed trait RingRouteing
object Closest extends RingRouteing
object ClockwiseAlways extends RingRouteing

class Ring(size: Int = 0, routeing : RingRouteing = Closest) extends Topology {
  def defaultConnectivityIn : Int = 3

  override def minimumVirtualChannels : Int = 2

  override def nodes: Int = size

  override def sizeFor(nodes: Int): Topology = new Ring(nodes)

  override def resolveCanonicalDestPort(dest : routeable_address_t, curr : address_t, setResult : canonical_port => Unit): Unit = {
    if (routeing == ClockwiseAlways) {
      setResult(Local)
      when(dest =/= curr) {
        setResult(ClockWise)
      }
    } else
      Ring(dest, curr, size, setResult)
  }
  override def portNamess: Seq[String] = Seq("Local", "ClockWise", "CounterClockWise")

  override def nodePortIndicesForCanonicalPorts(address: Int): Seq[Int] = (0 until maxCanonicalPorts)
  override def resolveNeighborAddress(address: Int, canonicalPort: Int): (Int, Int) = {
    val dx = if (canonicalPort == CounterClockWise) -1 else canonicalPort
    ((address + dx + size) % size, canonicalPort match {
      case CounterClockWise => ClockWise
      case ClockWise => CounterClockWise
      case Local => Local
    })
  }

  // The ring has one physical cycle, so keeping a packet's vc fixed for its
  // entire trip -- whether that's Static's pinned destVc or Dynamic's
  // unrestricted "any free lane" -- has no rule tying vc use to that cycle,
  // and concurrent packets can saturate every lane around the loop with
  // nothing able to advance. Both modes need a class transition at the one
  // designated dateline edge (address == size-1 going ClockWise, or
  // address == 0 going CounterClockWise -- the same physical wraparound
  // edge from each side); they differ only in whether non-dateline hops are
  // adaptive (Dynamic) or pinned (Static).
  override def allowedTransitionTable(cfg: NocConfig, port: (address_t, canonical_port),
                                       candidateCount: Int, vcCount: Int): Seq[Seq[Boolean]] = {
    val (address, canonicalPort) = port
    val isDateline = (address == size - 1 && canonicalPort == ClockWise) ||
                     (address == 0 && canonicalPort == CounterClockWise)
    val canonicalPorts = this.nodePortIndicesForCanonicalPorts(address, canonicalPort)

    val escapeVc = vcCount - 1
    cfg.virtualChannelMode match {
      case Dynamic if vcCount >= 2 =>

        // Per-candidate predicate over v, transposed below into GrantTable's
        // v-major allowed(v)(c) layout.
        val perCandidate = Seq.tabulate(candidateCount) { c =>
          val inputPort = canonicalPorts(c / vcCount)
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
            (v: Int) => v == escapeVc      // forced/sticky escape
          else
            (v: Int) => v != escapeVc      // adaptive pool, escape excluded
        }
        Seq.tabulate(vcCount, candidateCount) { (v, c) => perCandidate(c)(v) }
      // Static's destVc is otherwise pinned to sourceVc forever -- including
      // across the dateline -- which is exactly the same cyclic-dependency
      // deadlock as Dynamic without an escape lane, just duplicated per vc
      // class instead of network-wide. This is Dally's original
      // static/dimension-order dateline scheme (Dynamic's escape lane above
      // is Duato's adaptive generalization of the same idea): pin destVc =
      // sourceVc on ordinary hops, but force a one-time class bump at the
      // dateline instead of leaving it pinned. As with Dynamic, a Local
      // candidate's incoming vc tag is whatever the sender arbitrarily
      // picked, not a real class -- treat it as class 0 so it can't skip
      // the one-time bump or masquerade as already having crossed.
      case Static if vcCount >= 2 =>
        val perCandidate = Seq.tabulate(candidateCount) { c =>
          val inputPort = canonicalPorts(c / vcCount)
          val sourceVc = c % vcCount
          val effectiveClass = if (inputPort == Local && sourceVc == escapeVc) 0 else sourceVc

          val targetClass = if (isDateline) escapeVc else effectiveClass
          (v: Int) => v == targetClass
        }
        Seq.tabulate(vcCount, candidateCount) { (v, c) => perCandidate(c)(v) }
      case _ => super.allowedTransitionTable(cfg, port, candidateCount, vcCount)
    }
  }
}