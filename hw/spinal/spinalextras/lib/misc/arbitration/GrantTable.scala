package spinalextras.lib.misc.arbitration

import spinal.core._
import spinal.lib._
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalData, FormalProperties, FormalProperty}

class GrantTable(allowed: Seq[Seq[Boolean]]) extends Bundle with FormalData {
  // allowed(v)(c) -- v-major, same layout as GrantTable's own `grant` state
  // -- restricts which (lane, candidate) pairings can ever be granted. Only
  // those pairings get a real wire here; every other (v, c) combination is a
  // compile-time False, since GrantTable can never set it. Its shape alone
  // (rows = channels, columns = candidates) fully determines both counts, so
  // callers only ever need to hand over `allowed`.
  val channelCount = allowed.length
  val candidateCount = allowed.headOption.map(_.length).getOrElse(0)
  require(allowed.forall(_.length == candidateCount),
    s"allowed rows must all have the same length (candidateCount=$candidateCount)")

  val allowedPairs: Seq[(Int, Int)] =
    for (v <- 0 until channelCount; c <- 0 until candidateCount if allowed(v)(c)) yield (v, c)
  private val pairIndex: Map[(Int, Int), Int] = allowedPairs.zipWithIndex.toMap

  val grant = Vec(Bool(), allowedPairs.length)

  def apply(v: Int, c: Int): Bool = pairIndex.get((v, c)) match {
    case Some(i) => grant(i)
    case None => False
  }

  def allowed(v: Int, c: Int): Boolean = allowed(v)(c)

  def laneBusy(v: Int): Bool =
    allowedPairs.zipWithIndex.collect { case ((vv, _), i) if vv == v => grant(i) }.foldLeft(False: Bool)(_ || _)

  def candidateBusy(c: Int): Bool =
    allowedPairs.zipWithIndex.collect { case ((_, cc), i) if cc == c => grant(i) }.foldLeft(False: Bool)(_ || _)

  def laneBusy(v: UInt): Bool = Vec.tabulate(channelCount)(vv => laneBusy(vv))(v)

  def candidateBusy(c: UInt): Bool = Vec.tabulate(candidateCount)(cc => candidateBusy(cc))(c)

  def init(): this.type = {
    grant.foreach(_.init(False))
    this
  }

  def asReg(): GrantTable = {
    val r = Reg(new GrantTable(allowed)).init()
    this <> r
    r
  }

  def allowedMask = Vec(allowed.map(row => Vec(row.map(Bool(_)))))

  def clearLane(v: Int): Unit =
    allowedPairs.zipWithIndex.foreach { case ((vv, _), i) => if (vv == v) grant(i) := False }

  def claim(v: Int, c: Int): Unit = pairIndex.get((v, c)).foreach(i => grant(i) := True)

  def createArbiter(roundRobinArbitration: Boolean): GrantTableArbiter = {
    val arbiter = new GrantTableArbiter(roundRobinArbitration = roundRobinArbitration, allowed = allowed)
    arbiter.io.grant <> this
    arbiter
  }

  def createRouter[T <: Data](payloadType: HardType[T]) : GrantTableStreamRouter[T] = {
    val router = new GrantTableStreamRouter(payloadType, allowed)
    router.io.grant <> this
    router
  }

  /**
   * @return Whether or not the current state of the bundle is valid. Typically either asserted or assumed by a
   *         component which has this bundle as an input or an output.
   *
   *         For complicated properties, consider using the helper class `FormalProperties`
   */
  override def formalIsStateValid(): Seq[FormalProperty] = new FormalProperties() {
    // At most one candidate granted per lane -- io.dests(v) can only ever
    // forward one payload at a time.
    for (v <- 0 until channelCount) {
      val bits = allowedPairs.zipWithIndex.collect { case ((vv, _), i) if vv == v => grant(i) }
      addFormalProperty(CountOne(bits) <= 1,
        s"grant lane $v must not be granted to more than one candidate at once")
    }

    // At most one lane granted per candidate -- io.sources(c) can only ever
    // be consumed by one lane at a time.
    for (c <- 0 until candidateCount) {
      val bits = allowedPairs.zipWithIndex.collect { case ((_, cc), i) if cc == c => grant(i) }
      addFormalProperty(CountOne(bits) <= 1,
        s"candidate $c must not be granted more than one lane at once")
    }

  }
}



object GrantTable {
  // No restriction: any candidate may be granted any lane -- fully
  // adaptive/Dynamic behavior. v-major (allowed(v)(c)), matching GrantTable's
  // own `grant` state layout.
  def allowAll(candidateCount: Int, channelCount: Int): Seq[Seq[Boolean]] =
    Seq.fill(channelCount)(Seq.fill(candidateCount)(true))

  // candidateCount must be a multiple of channelCount, laid out as
  // candidateOf(i, s) = i * channelCount + s (see VirtualIdAllocator):
  // candidate c may only ever be granted lane (c % channelCount), i.e.
  // destVc is pinned to the candidate's own source-vc slot and never
  // reassigned -- Static behavior.
  def diagonal(candidateCount: Int, channelCount: Int): Seq[Seq[Boolean]] =
    Seq.tabulate(channelCount, candidateCount) { (v, c) => (c % channelCount) == v }
}


