package spinalextras.lib.noc.topology

import spinal.core.{False, IntToBuilder, SInt, UInt, log2Up, when}
import spinalextras.lib.noc.virtualchannels.Dynamic
import spinalextras.lib.noc.{NocConfig, Topology}

class Torus(gridSize: (Int, Int) = (0, 0)) extends Mesh(gridSize) {
  override def resolveCanonicalOutputPort(address : Int, port : Int): Int = nodePortIndicesForCanonicalPorts(address).indexOf(port)

  override def minimumVirtualChannels : Int = 2

  override def sizeFor(nodes: Int): Topology = {
    if (gridSize._1 * gridSize._2 <= nodes)
      this
    else
      new Torus(getBestGridSize(nodes))
  }

  override def createAddress(x: Int, y: Int): Int = {
    val _x = (x + gridSize._1) % gridSize._1
    val _y = (y + gridSize._2) % gridSize._2
    _x * gridSize._2 + _y
  }

  override def nodePortIndicesForCanonicalPorts(address: Int): Seq[Int] = (0 until maxCanonicalPorts)

  override def resolveDestPort(dest: routeable_address_t, curr: address_t): UInt = {
    val (dx, dy) = unpackRouteableAddress(dest)
    val (x, y) = addressToXY(curr)

    val dir_x = Ring(dx, x, gridSize._1)
    val dir_y = Ring(dy, y, gridSize._2)

    val N = UInt(log2Up(nodePortIndicesForCanonicalPorts(curr).size) bits)
    def setResult(canonicalPort : Int): Unit = {
      //spinal.core.report(Seq("Setting ", canonicalPort, " from ", dir_x, " ", dir_y, " ", x, " ", y, " ", dx, " ", dy))
      val output_port = resolveCanonicalOutputPort(curr, canonicalPort)
      assert(output_port >= 0)
      N := output_port
    }

    setResult(Mesh.LOCAL)

    when(dir_x === Ring.Local) {
      when(dir_y === Ring.ClockWise) {
        setResult(Mesh.SOUTH)
      } elsewhen(dir_y === Ring.CounterClockWise) {
        setResult(Mesh.NORTH)
      }
    } elsewhen(dir_x === Ring.ClockWise) {
      setResult(Mesh.EAST)
    } elsewhen(dir_x === Ring.CounterClockWise) {
      setResult(Mesh.WEST)
    }

    N
  }

  // Two physical cycles here (an X-ring and a Y-ring), so -- like Ring --
  // purely adaptive Dynamic assignment can deadlock. A single shared escape
  // class (the top vc index) covers *both* dimensions: dimension-order
  // routing (X fully completes before Y starts, see resolveDestPort) means a
  // packet needs at most one class bump total, ever -- whichever dateline
  // (X's or Y's) it happens to cross first -- and once escaped it never
  // needs to distinguish which dimension it escaped in, since staying in the
  // same (already-top) class at a later dateline is trivially monotonic.
  // Giving X and Y *separate* escape classes (escapeX = vcCount-1, escapeY =
  // vcCount-2) doesn't just waste a class: a packet needing both crossings
  // gets forced escapeX -> escapeY at the Y dateline, a real class
  // *decrease* that breaks the monotonic invariant, and it costs a second
  // dedicated lane for no benefit -- at vcCount == 2 that leaves zero pool
  // lanes at all (both taken by escapeX/escapeY), guaranteeing an instant
  // stall for every ordinary hop. One shared escape class needs only
  // vcCount >= 2, same as Ring.
  override def allowedTransitionTable(cfg: NocConfig, port: (address_t, canonical_port),
                                       candidateCount: Int, vcCount: Int): Seq[Seq[Boolean]] = {
    val (address, canonicalPort) = port
    val (x, y) = addressToXY(address)
    val escapeVc = vcCount - 1

    val isDateline = (x == gridSize._1 - 1 && canonicalPort == Mesh.EAST) ||
      (x == 0 && canonicalPort == Mesh.WEST) ||
      (y == gridSize._2 - 1 && canonicalPort == Mesh.SOUTH) ||
      (y == 0 && canonicalPort == Mesh.NORTH)

    val dynamic = cfg.virtualChannelMode == Dynamic

    Seq.tabulate(candidateCount) { c =>
      val inputPort = c / vcCount
      val sourceVc = c % vcCount
      // As with Ring: a candidate's incoming vc tag only means "already
      // escaped" if an upstream router's own allowedTransitionTable
      // actually put it there. On the Local port it's whatever the
      // injecting source arbitrarily picked, not evidence of a real
      // dateline crossing, so it must not be honored as sticky here.
      val isEscapeInputVc = sourceVc == escapeVc
      val alreadyEscaped = isEscapeInputVc && inputPort != Mesh.LOCAL
      val effectiveClass = if (inputPort == Mesh.LOCAL && isEscapeInputVc) 0 else sourceVc

      if (isDateline || alreadyEscaped) Seq.tabulate(vcCount)(_ == escapeVc)   // forced/sticky escape
      else if (dynamic) Seq.tabulate(vcCount)(_ != escapeVc)                  // adaptive pool
      else Seq.tabulate(vcCount)(_ == effectiveClass)
    }

  }
}
