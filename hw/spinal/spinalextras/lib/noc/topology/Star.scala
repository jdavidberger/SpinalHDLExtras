package spinalextras.lib.noc.topology

import spinal.core._
import spinalextras.lib.noc.topology.Ring.{ClockWise, CounterClockWise, Local}
import spinalextras.lib.noc.{NoC, RouterNode, Topology}

object Star {
  def apply(nodes : Int) = new Tree(nodes, nodes)
}