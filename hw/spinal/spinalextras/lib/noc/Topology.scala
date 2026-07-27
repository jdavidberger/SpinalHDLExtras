package spinalextras.lib.noc

import spinal.core._
import spinal.lib._
import spinalextras.lib.misc.arbitration.GrantTable
import spinalextras.lib.noc.topology._
import spinalextras.lib.noc.virtualchannels.{Dynamic, Static}

import scala.collection.mutable
import scala.language.postfixOps

trait Topology {
  // 'address' is essentially the index of the node in the nodes list
  type address_t = Int

  // 'routeable_address' is topology defined and not necessarilly the same as address. Most notably for mesh topologies
  // the routeable_address is encoded as XY.
  type routeable_address_constant_t = Int
  type routeable_address_t = UInt

  // Canonical ports span the range of all possible logic ports -- Local, North, East, West, South for mesh for instance.
  // 0 is always the local port
  type canonical_port = Int
  // Used to mark where a port_index is returned. A port index indexs into the port object on a given node or port. So
  // if a port can only see Local, East, South, a port index of 2 points to the south port.
  type port_index = Int

  // Topologies with different shapes have different requirements for minimum virtual channels to prevent deadlocks.
  def minimumVirtualChannels : Int = 1

  def portNamess : Seq[String]
  def portName(canonicalPort : canonical_port) = portNamess(canonicalPort)

  override def toString: String = f"${getClass.getSimpleName.replace("$", "")}"

  def nodes: Int
  def addressSize : Int = log2Up(nodes)
  def sizeFor(nodes : Int) : Topology

  // Per-topology routing decision, in the full (uncompacted) physical port
  // numbering -- may resolve to the port the flit arrived on (e.g. a
  // dimension-order routing corner case, or a self-addressed packet); that
  // gets excluded by the public resolveDestPort below.
  def resolveDestPort(dest : routeable_address_t, curr : address_t, inputPort : canonical_port) = {
    val nodeIndices = nodePortIndicesForCanonicalPorts(curr, inputPort)
    val destPortIndex = U(0, log2Up(nodeIndices.size) bits)
    destPortIndex.allowOverride()
    resolveCanonicalDestPort(dest, curr, canonical_port => {
      val idx = nodeIndices.indexOf(canonical_port)
      if(idx != -1) {
        destPortIndex := idx
      }
    })
    destPortIndex
  }

  def resolveCanonicalDestPort(dest : routeable_address_t, curr : address_t, set_result : canonical_port => Unit): Unit

  def addressToRouteableAddress(address : address_t) : routeable_address_constant_t = address
  def routeableAddressToAddress(routeable_address : routeable_address_constant_t) : address_t = routeable_address

  def addressName(address : Int) : String = s"node_${address}"

  def maxCanonicalPorts: Int = portNamess.size

  // Sequence of canonical port numbers in their logical port index
  def nodePortIndicesForCanonicalPorts(address : Int): Seq[canonical_port]

  def nodeHasInputPort(address : Int, inputPort : canonical_port) = nodePortIndicesForCanonicalPorts(address).indexOf(inputPort) != -1

  // The output port indices as seen from an input port
  def nodePortIndicesForCanonicalPorts(address : Int, inputPort : canonical_port): Seq[canonical_port] =
    if(nodeHasInputPort(address, inputPort))
      nodePortIndicesForCanonicalPorts(address).filter(canonicalPort => inputPort == 0 || canonicalPort != inputPort)
    else
      Seq()

  def nodeInputPortIndicesForCanonicalPorts(address : Int, outputPort : canonical_port) : Seq[canonical_port] = {
    val rtn = new mutable.ArrayBuffer[Int]()
    for(i <- 0 until maxCanonicalPorts) {
      if(resolveCanonicalOutputPort(address, outputPort, i) != -1)
        rtn.append(i)
    }
    rtn
  }

  def resolveCanonicalOutputPort(address : Int, port : Int): Int = nodePortIndicesForCanonicalPorts(address).indexOf(port)

  // Physical port for `port`, excluding inputPort's own slot -- -1 if
  // `port` isn't reachable from `address` at all, or if it *is* inputPort's
  // own canonical port (never a valid destination, by definition).
  def resolveCanonicalOutputPort(address : Int, port : Int, inputPort : canonical_port): port_index =
    nodePortIndicesForCanonicalPorts(address, inputPort).indexOf(port)

  // Returns the neighbor address and the opposite port
  def resolveNeighborAddress(address : Int, canonicalPort : canonical_port) : (Int, canonical_port)

  def resolveCanonicalInputPort(address : Int, port : canonical_port): port_index = resolveCanonicalOutputPort(address, port)

  def createNode(cfg: NocConfig, address: Int): RouterNode = {
    new RouterNode(cfg, address = address)
  }

  // Describes the transition table at any given interchange. More allowed transitions tends to lead to higher gate
  // usage but lower congestion. Cyclic topologies have complicated transition tables at their wrap around point.
  def allowedTransitionTable(cfg: NocConfig, port : (address_t, canonical_port),
                             candidateCount : Int, vcCount : Int
                            ): Seq[Seq[Boolean]] = {
    cfg.virtualChannelMode match {
      case Static => GrantTable.diagonal(candidateCount, vcCount)
      case Dynamic => GrantTable.allowAll(candidateCount, vcCount)
    }
  }

  def createNodes(noc : NoC) : Seq[RouterNode] = {
    val nodes = for (x <- 0 until this.nodes) yield {
      val node = createNode(noc.cfg, x)
      node.setName(s"${addressName(x)}")
      node.io.inputs(0) <> noc.io.inputs(x).map(d => {
        val f = Fragment(Flit(noc.cfg))
        f.fragment.datum := d.fragment
        f.vc.clearAll()
        f.last := d.last
        f
      })

      StreamArbiterFactory().lowerFirst.fragmentLock.on(
        StreamDemux(node.io.outputs(0), node.io.outputs(0).payload.vc, noc.cfg.virtualChannels)
      ).map(flit => {
        val p = Fragment(noc.cfg.datatype)
        p.last := flit.last
        p.fragment := flit.fragment.datum
        p
      }) <> noc.io.outputs(x)
      node
    }

    for (address <- 0 until this.nodes; canonicalPort <- noc.cfg.topology.nodePortIndicesForCanonicalPorts(address)) {
      if(canonicalPort > 0) {
        val (neighborAddress, neighborPort) = noc.cfg.topology.resolveNeighborAddress(address, canonicalPort)
        nodes(address).inputs(canonicalPort) <> nodes(neighborAddress).outputs(neighborPort)
      }
    }
    nodes
  }
}

object Topology {
  def testConfigurations() = {
    Seq(
      "Mesh_3x2" -> new Mesh((3, 2)),
      "Mesh_4x4" -> new Mesh((4, 4)),
      "Ring_3" -> new Ring(3),
      "Torus_3x2" -> new Torus((3, 2)),
      "Tree_4x2" -> new Tree(4, 2),
    )
  }

  type address_t = Int
  type routeable_address_constant_t = Int
  type routeable_address_t = UInt

  type canonical_port = Int
  type port_index = Int
}



