package spinalextras.lib.noc.topology

import spinal.core._
import spinal.lib._
import spinalextras.lib.noc.topology.Mesh.LOCAL
import spinalextras.lib.noc.{NoC, RouterNode, Topology}

import scala.collection.mutable

class Mesh(gridSize: (Int, Int) = (0, 0)) extends Topology {
  def defaultConnectivityIn : Int = 5

  override def createNodes(noc: NoC): Seq[RouterNode] = {
    val nodes = super.createNodes (noc)

    def getNode(x: Int, y: Int) = {
      val idx = createAddress(x, y)
      if (idx >= 0) nodes(idx) else null
    }

    for (x <- 0 until gridSize._1; y <- 0 until gridSize._2) {
      val Node = getNode(x, y)
      Node.setName(s"node_${x}_${y}")
    }

    nodes
  }

  override def nodes: Int = gridSize._1 * gridSize._2

  override def addressSize: Int = {
    log2Up(gridSize._1) + log2Up(gridSize._2)
  }

  def addressSizeTuple: (Int, Int) = {
    (log2Up(gridSize._1), log2Up(gridSize._2))
  }

  def unpackRouteableAddress(address: UInt): (UInt, UInt) = {
    val (x, y) = addressSizeTuple
    (address.resize(x bits), address >> x)
  }
  def unpackRouteableAddress(address: Int): (Int, Int) = {
    val (x, y) = addressSizeTuple
    (address, address >> x)
  }

  def createAddress(x: Int, y: Int): Int = {
    if (x > gridSize._1 || x < 0) -1
    else if (y > gridSize._2 || y < 0) -1
    else x * gridSize._2 + y
  }

  def addressToXY(address : Int): (Int, Int) = {
    (address / gridSize._2, address % gridSize._2)
  }


  override def addressName(address : Int) : String = {
    val (x,y) = addressToXY(address)
    s"node_${x}_${y}"
  }

  override def addressToRouteableAddress(address : address_t) : routeable_address_constant_t = {
    val (x, y) = addressToXY(address)
    x | y << log2Up(gridSize._1)
  }
  override def routeableAddressToAddress(routeable_address : routeable_address_constant_t) : address_t = {
    val (x,y) = unpackRouteableAddress(routeable_address)
    createAddress(x, y)
  }

  override def resolveDestPort(dest: routeable_address_t, curr: address_t): UInt = {
    val destAddress = unpackRouteableAddress(dest)
    val (x, y) = addressToXY(curr)

    val N = UInt(log2Up(nodePortIndicesForCanonicalPorts(curr).size) bits)
    def setResult(canonicalPort : Int): Unit = {
      val port = resolveCanonicalOutputPort(curr, canonicalPort)
      if(port != -1) {
        N := port
      }
    }

    setResult(Mesh.LOCAL)
    when(destAddress._1 < x) {
      setResult(Mesh.WEST)
    } elsewhen(destAddress._1 > x) {
      setResult(Mesh.EAST)
    } elsewhen(destAddress._2 < y) {
      setResult(Mesh.NORTH)
    } elsewhen(destAddress._2 > y) {
      setResult(Mesh.SOUTH)
    }

    N
  }

  def getBestGridSize(n: Int): (Int, Int) = {
    val sqrtN = Math.sqrt(n).toInt

    // Find the largest factor <= sqrt(n)
    val rows = (sqrtN to 1 by -1).find(n % _ == 0).getOrElse(1)
    val cols = n / rows

    (rows, cols)
  }

  def sizeFor(nodes: Int): Topology = {
    if (gridSize._1 * gridSize._2 <= nodes)
      this
    else
      new Mesh(getBestGridSize(nodes))
  }

  override def nodePortIndicesForCanonicalPorts(address: Int): Seq[Int] = {
    val (x, y) = addressToXY(address)
    val indices = new mutable.ArrayBuffer[Int]()
    indices.append(Mesh.LOCAL)
    if(x > 0) indices.append(Mesh.WEST)
    if((x + 1) < gridSize._1) indices.append(Mesh.EAST)
    if(y > 0) indices.append(Mesh.NORTH)
    if((y + 1) < gridSize._2) indices.append(Mesh.SOUTH)

    indices
  }

  override def resolveNeighborAddress(address: Int, canonicalPort: Int): (Int, Int) = {
    val (x, y) = addressToXY(address)
    val (dx, dy, opposite_port) = canonicalPort match {
      case Mesh.WEST => (-1, 0, Mesh.EAST)
      case Mesh.EAST => (1, 0, Mesh.WEST)
      case Mesh.NORTH => (0, -1, Mesh.SOUTH)
      case Mesh.SOUTH => (0, 1, Mesh.NORTH)
      case _ => (0, 0, Mesh.LOCAL)
    }

    (createAddress(x + dx, y + dy), opposite_port)
  }
}

object Mesh {
  val LOCAL = 0
  val WEST = 1
  val EAST = 2
  val NORTH = 3
  val SOUTH = 4
}