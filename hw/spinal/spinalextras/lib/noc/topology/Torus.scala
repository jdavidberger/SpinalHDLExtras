package spinalextras.lib.noc.topology

import spinal.core.{False, IntToBuilder, SInt, UInt, log2Up, when}
import spinalextras.lib.noc.virtualchannels.Dynamic
import spinalextras.lib.noc.{NocConfig, Topology}

class Torus(gridSize: (Int, Int) = (0, 0)) extends Mesh(gridSize) {
  override def resolveCanonicalOutputPort(address : Int, port : Int): Int = nodePortIndicesForCanonicalPorts(address).indexOf(port)

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
  // purely adaptive Dynamic assignment can deadlock. Reserve the top two vc
  // indices as sticky escape lanes, one per dimension: escapeX is entered
  // only at the X-ring's dateline edge, escapeY only at the Y-ring's.
  // Dimension-order routing (X fully completes before Y starts, see
  // resolveDestPort) means a packet only ever needs one of the two, never
  // both, and never needs to shed one for the other. Ordinary hops keep the
  // remaining vcs as a fully adaptive pool, excluding both escape lanes.
  // Static (and vcCount < 2) fall through to the default (diagonal/allowAll)
  // behavior.
  override def allowedTransitionTable(cfg: NocConfig, port: (address_t, canonical_port),
                                       candidateCount: Int, vcCount: Int): Seq[Seq[Boolean]] = {
    cfg.virtualChannelMode match {
      case Dynamic if vcCount >= 2 =>
        val (address, canonicalPort) = port
        val (x, y) = addressToXY(address)
        val escapeX = vcCount - 1
        val escapeY = vcCount - 2

        val isXDateline = (x == gridSize._1 - 1 && canonicalPort == Mesh.EAST) ||
                          (x == 0 && canonicalPort == Mesh.WEST)
        val isYDateline = (y == gridSize._2 - 1 && canonicalPort == Mesh.SOUTH) ||
                          (y == 0 && canonicalPort == Mesh.NORTH)

        Seq.tabulate(candidateCount) { c =>
          val inputPort = c / vcCount
          val sourceVc = c % vcCount
          // As with Ring: a candidate's incoming vc tag only means "already
          // escaped" if an upstream router's own allowedTransitionTable
          // actually put it there. On the Local port it's whatever the
          // injecting source arbitrarily picked, not evidence of a real
          // dateline crossing, so it must not be honored as sticky here.
          val alreadyEscaped = (sourceVc == escapeX || sourceVc == escapeY) && inputPort != Mesh.LOCAL

          if (isXDateline) Seq.tabulate(vcCount)(_ == escapeX)               // forced escapeX
          else if (isYDateline) Seq.tabulate(vcCount)(_ == escapeY)          // forced escapeY
          else if (alreadyEscaped) Seq.tabulate(vcCount)(_ == sourceVc)      // sticky
          else Seq.tabulate(vcCount)(v => v != escapeX && v != escapeY)      // adaptive pool
        }
      case _ => super.allowedTransitionTable(cfg, port, candidateCount, vcCount)
    }
  }
}
