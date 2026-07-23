package spinalextras.lib.noc.topology

import spinal.core._
import spinalextras.lib.noc.topology.Ring.{ClockWise, CounterClockWise, Local}
import spinalextras.lib.noc.{NoC, RouterNode, Topology}

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
}