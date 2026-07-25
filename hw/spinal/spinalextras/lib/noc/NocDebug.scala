package spinalextras.lib.noc

import spinal.core.sim._

// Callable from a testbench that has detected a stall (e.g. right before a
// timeout assertion fires) to print exactly which (node, output, lane)
// resources are currently held, by which candidate, and which candidates are
// still requesting but blocked. This is the same shape of information needed
// to find a genuine cyclic buffer dependency, but done as a testbench-side
// simulation peek rather than baking report() calls into the RTL and
// recompiling every time a new stall needs investigating.
object NocDebug {
  def dumpStalledState(noc: NoC): Unit = {
    val cfg = noc.cfg
    val vcCount = cfg.virtualChannels
    println(s"=== NocDebug: stalled-state dump (${cfg.topology.getClass.getSimpleName}, " +
      s"vc=$vcCount, mode=${cfg.virtualChannelMode}) ===")

    for (node <- noc.nodes) {
      val address = node.address

      for ((table, o) <- node.allocators.map(_.crossbar.arbiter).zipWithIndex) {
        val candidateCount = table.io.request.length
        val canonicalPort = cfg.topology.nodePortIndicesForCanonicalPorts(address)(o)

        def describe(c: Int): String = s"candidate=$c (inputPort=${c / vcCount}, sourceVc=${c % vcCount})"

        val busy = for {
          v <- 0 until vcCount
          c <- 0 until candidateCount
          if table.io.grant(v, c).toBoolean
        } yield (v, c)
        val requesting = (0 until candidateCount).filter(c => table.io.request(c).toBoolean)

        if (busy.nonEmpty || requesting.nonEmpty) {
          println(s"  node=$address output=$o(canonical=$canonicalPort)")
          for ((v, c) <- busy) {
            println(s"    BUSY lane=$v <- ${describe(c)}")
          }
          for (c <- requesting) {
            val granted = busy.exists(_._2 == c)
            println(s"    REQ  ${describe(c)}" + (if (granted) " [granted]" else " [BLOCKED]"))
          }
        }
      }
    }
    println("=== end NocDebug dump ===")
  }
}
