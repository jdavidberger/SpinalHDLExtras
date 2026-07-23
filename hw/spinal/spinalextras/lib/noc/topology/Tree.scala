package spinalextras.lib.noc.topology

import spinal.core._
import spinal.lib._
import spinalextras.lib.noc.topology.Tree.{LOCAL, UP}
import spinalextras.lib.noc.{NoC, NocConfig, RouterNode, Topology}

import scala.collection.mutable
import scala.language.postfixOps

/**
 * Port numbering for a Tree node:
 *   0            = LOCAL  (required by Topology.createNodes, which always wires port 0
 *                  to the NoC's external input/output for every node)
 *   1            = UP     (link to the parent; unused/open on the root)
 *   2 .. 2+k-1   = DOWN_0 .. DOWN_(k-1) (links to up to `maxChildren` children;
 *                  open on any port a node doesn't actually use, e.g. leaves)
 */
object Tree {
  val LOCAL: Int = 0
  val UP: Int = 1
  def DOWN(i: Int): Int = 2 + i
}

/**
 * A k-ary tree topology, addressed with a DFS (preorder) numbering.
 *
 * Indexing scheme: nodes are numbered 0 .. totalNodes-1 in preorder DFS
 * order (root first, then the entirety of its first child's subtree, then
 * the entirety of its second child's subtree, and so on). The defining
 * property of a preorder numbering is that every subtree occupies a single
 * *contiguous* range of indices [lo, hi]. That's chosen specifically to
 * make routing division-free in hardware: since `curr` is always known at
 * elaboration time (it's a Scala Int, one router per index), each node's
 * children's subtree ranges are compile-time constants, so "is dest under
 * child c" is just `dest >= lo_c && dest <= hi_c` -- plain comparators
 * against constants, no divide/mod by `maxChildren` anywhere in hardware.
 *
 * All the tree-shape bookkeeping (who's whose parent/child, and each
 * subtree's [lo, hi]) is computed once in Scala while building the
 * topology; only the resulting constants get baked into comparisons.
 *
 * @param totalNodes  number of nodes in the tree
 * @param maxChildren maximum number of children (leaves) any single node may have
 */
class Tree(totalNodes: Int = 0, maxChildren: Int = 2) extends Topology {

  require(maxChildren >= 1, "Tree needs at least 1 child per node to be a tree")
  require(totalNodes >= 1 || totalNodes == 0, "totalNodes must be non-negative")

  // LOCAL + UP + up to maxChildren DOWN ports
  def defaultConnectivityIn: Int = maxChildren + 2

  override def nodes: Int = totalNodes

  private def portWidth: Int = log2Up(defaultConnectivityIn)

  /** Per-node shape info, built once at elaboration time. */
  private case class NodeShape(
                                index: Int,              // this node's DFS/preorder index
                                parent: Int,             // parent's DFS index, or -1 for the root
                                // children in port order: (childIndex, childSubtreeLo, childSubtreeHi)
                                children: Seq[(Int, Int, Int)],
                                subtreeLo: Int,
                                subtreeHi: Int
                              )

  /**
   * Split `remaining` nodes as evenly as possible across up to
   * `maxChildren` child subtrees (first children get the remainder).
   * This is the only place tree "shape" is decided; any other balancing
   * policy could be dropped in here without touching the routing logic.
   */
  private def childSubtreeSizes(remaining: Int): Seq[Int] = {
    if (remaining <= 0) Seq.empty
    else {
      val childCount = Math.min(maxChildren, remaining)
      val base = remaining / childCount
      val extra = remaining % childCount
      (0 until childCount).map(i => base + (if (i < extra) 1 else 0))
    }
  }

  /**
   * Recursively lay out a subtree of `size` nodes starting at DFS index
   * `startIndex`, with parent `parentIndex` (-1 if this is the root),
   * appending every node's NodeShape (in DFS order) to `acc`.
   */
  private def buildSubtree(startIndex: Int, size: Int, parentIndex: Int, acc: scala.collection.mutable.ArrayBuffer[NodeShape]): Unit = {
    val subtreeLo = startIndex
    val subtreeHi = startIndex + size - 1
    val remaining = size - 1
    val sizes = childSubtreeSizes(remaining)

    // Reserve this node's slot now so children can be appended after it,
    // then patch in its children list once we know their ranges.
    val myPos = acc.length
    acc += NodeShape(startIndex, parentIndex, Nil, subtreeLo, subtreeHi)

    var next = startIndex + 1
    val children = sizes.map { childSize =>
      val childIndex = next
      buildSubtree(childIndex, childSize, startIndex, acc)
      val childHi = next + childSize - 1
      next += childSize
      (childIndex, childIndex, childHi)
    }

    acc(myPos) = acc(myPos).copy(children = children)
  }

  private val shapes: Array[NodeShape] = {
    val acc = scala.collection.mutable.ArrayBuffer[NodeShape]()
    if (totalNodes > 0) buildSubtree(0, totalNodes, -1, acc)
    // buildSubtree appends in DFS order starting from index 0, so acc is
    // already sorted by index; index into it directly.
    acc.toArray
  }

  def nodePortIndicesForCanonicalPorts(address : Int): Seq[Int] = {
    val shape = shapes(address)
    val indices = new mutable.ArrayBuffer[Int]()
    indices.append(Tree.LOCAL)
    if (address != 0)
      indices.append(Tree.UP)
    shape.children.zipWithIndex.foreach { case (_, idx) => {
      indices.append(Tree.DOWN(idx))
    }}

    indices
  }

  /**
   * Route towards `dest` from node `curr`.
   *
   * Because of the preorder numbering, curr's own subtree is exactly
   * [curr, curr's subtreeHi], split into contiguous sub-ranges for each
   * child. So:
   *   - dest == curr                       -> LOCAL
   *   - dest falls in child c's [lo, hi]    -> DOWN(c)
   *   - otherwise (outside curr's subtree)  -> UP
   *
   * Every comparison is against a Scala-Int constant baked in at
   * elaboration time (curr, and every lo/hi, are known at build time) --
   * no division, no dependence on dest's runtime value beyond compares.
   */
  override def resolveDestPort(dest: UInt, curr: Int): UInt = {
    val shape = shapes(curr)
    val w = addressSize

    val result = UInt(log2Up(nodePortIndicesForCanonicalPorts(curr).size) bits)

    def setResult(canonicalPort : Int): Unit = {
      val port = resolveCanonicalOutputPort(curr, canonicalPort)
      if(port != -1) {
        result := port
      }
    }

    setResult(Tree.LOCAL)

    when(dest =/= U(curr, w bits)) {
      setResult(Tree.UP)
    }

    for (((_, lo, hi), port) <- shape.children.zipWithIndex) {
      if (lo == hi) {
        when(dest === U(lo, w bits)) {
          setResult(Tree.DOWN(port))
        }
      } else {
        when(dest >= U(lo, w bits) && dest <= U(hi, w bits)) {
          setResult(Tree.DOWN(port))
        }
      }
    }

    result
  }

  def sizeFor(nodes: Int): Topology = {
    if (totalNodes <= nodes)
      this
    else
      new Tree(nodes, maxChildren)
  }

  override def resolveNeighborAddress(address: Int, canonicalPort: Int): (Int, Int) = {
    val shape = shapes(address)
    canonicalPort match {
      case Tree.LOCAL => (address, Tree.LOCAL)
      case Tree.UP => (if (address != 0) shape.parent else -1, {
        val parentShape = shapes(shape.parent)
        Tree.DOWN(parentShape.children.indexWhere(x => x._1 == address))
      })
      case x : Int => (if (shape.children.size > (x-2)) shape.children(x - 2)._1 else -1, Tree.UP)
    }
  }
}