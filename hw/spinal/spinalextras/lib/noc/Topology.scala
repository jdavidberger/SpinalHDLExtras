package spinalextras.lib.noc

import spinal.core._
import spinal.lib._
import spinalextras.lib.noc.topology._

trait Topology {
  type address_t = Int
  type routeable_address_constant_t = Int
  type routeable_address_t = UInt

  def nodes: Int
  def addressSize : Int = log2Up(nodes)
  def sizeFor(nodes : Int) : Topology
  def resolveDestPort(dest : routeable_address_t, curr : address_t): UInt

  def addressToRouteableAddress(address : address_t) : routeable_address_constant_t = address
  def routeableAddressToAddress(routeable_address : routeable_address_constant_t) : address_t = routeable_address

  def addressName(address : Int) : String = s"node_${address}"
  def defaultConnectivityIn : Int
  def defaultConnectivityOut : Int = defaultConnectivityIn
  def maxCanonicalPorts: Int = Math.max(defaultConnectivityIn, defaultConnectivityOut)

  // Sequence of canonical port numbers in their logical port index
  def nodePortIndicesForCanonicalPorts(address : Int): Seq[Int]

  def resolveCanonicalOutputPort(address : Int, port : Int): Int = nodePortIndicesForCanonicalPorts(address).indexOf(port)

  // Returns the neighbor address and the opposite port
  def resolveNeighborAddress(address : Int, canonicalPort : Int) : (Int, Int)

  def resolveCanonicalInputPort(address : Int, port : Int): Int = resolveCanonicalOutputPort(address, port)

  def createNode(cfg: NocConfig, address: Int): RouterNode = {
    new RouterNode(cfg, address = address)
  }


  def createNodes(noc : NoC) : Seq[RouterNode] = {
    val nodes = for (x <- 0 until this.nodes) yield {
      val node = createNode(noc.cfg, x)
      node.setName(s"node_${x}")
      node.io.inputs(0) <> noc.io.inputs(x)
      node.io.outputs(0) <> noc.io.outputs(x)
      node
    }

    var connections : Int = 0
    for (address <- 0 until this.nodes; canonicalPort <- noc.cfg.topology.nodePortIndicesForCanonicalPorts(address)) {
      if(canonicalPort > 0) {
        val (neighborAddress, neighborPort) = noc.cfg.topology.resolveNeighborAddress(address, canonicalPort)
        println(s"Connecting ${addressName(address)}:${canonicalPort} to ${neighborAddress} ${addressName(neighborAddress)}:${neighborPort}")
        nodes(address).inputs(canonicalPort) <> nodes(neighborAddress).outputs(neighborPort)
        connections += 1
      }
    }

    println(f"NoC has ${connections} edges for ${this.nodes} nodes")
    nodes
  }
}

object Topology {
  def testConfigurations() = {
    Seq(
      "Mesh(3x2)" -> new Mesh((3, 2)),
      "Mesh(4x4)" -> new Mesh((4, 4)),
      "Torus(3x2)" -> new Torus((3, 2)),
      "Tree(10,2)" -> new Tree(10, 2),
      "Ring(6)" -> new Ring(6),
      "Star(8)" -> Star(8),
    )
  }
}



