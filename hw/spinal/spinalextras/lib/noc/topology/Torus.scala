package spinalextras.lib.noc.topology

import spinal.core.{False, IntToBuilder, SInt, UInt, log2Up, when}
import spinalextras.lib.noc.Topology

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

}
