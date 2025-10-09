package spinalextras.lib.formal.fillins

import spinal.core.Component.push
import spinal.core.Data
import spinal.lib.{Counter, CounterUpDown, Flow, IMasterSlave, Stream, StreamFifo, StreamJoin, slave}
import spinalextras.lib.formal.StreamFormal.StreamExt
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalDataWithEquivalnce, FormalMasterSlave, FormalProperties, FormalProperty, StreamFormal, fillins}
import spinal.core._
import spinal.core.internals.AssertStatementKind.ASSERT
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Ax, Axi4B, Axi4Bus, Axi4Config, Axi4R, Axi4ReadOnly, Axi4Shared, Axi4W, Axi4WriteOnly}
import spinalextras.lib.misc.CounterVariableChange
import spinalextras.lib.testing.test_funcs

import scala.collection.mutable
import scala.language.postfixOps
import scala.reflect.ClassTag

object Axi4Formal {
  val maxFormalQueue : Int = 8

  def addTree(tree : Seq[UInt]): UInt = {
    tree.size match {
      case 0 => U(0)
      case 1 => tree.head
      case _ => addTree(tree.slice(0, tree.size / 2)) +^ addTree(tree.slice(tree.size / 2, tree.size))
    }
  }

  def makeAxiCounter(a_fire: Bool, size: UInt, r: Bool) = {
    val outstanding = new CounterVariableChange(32)
    when(a_fire) {
      outstanding.increment_by(size +^ 1)
    }
    when(r) {
      outstanding.decrement_by(1)
    }

    Component.toplevel.addPrePopTask(() => {
      spinal.core.assume(!outstanding.willOverflow) // This is required for the inductive formal methods to work
    })

    outstanding
  }

  def createFifo[T <: Data](f : Flow[T], maxQueue : Int, latency : Int = 0) = {
    val fifoOverflow = Bool()
    val fifo = StreamFifo(cloneOf(f.payload), maxQueue, latency = latency)
    f.toStream(fifoOverflow) <> fifo.io.push
    assume(!fifoOverflow)

    fifo
  }

  def validFire[T <: Data](x: Stream[T]): Flow[T] = {
    val f = Flow(cloneOf(x.payload))
    val alreadyFired = RegNext(x.isStall, False)
    f.valid := x.valid && !alreadyFired
    f.payload := x.payload
    f
  }

  case class Axi4ReadStreamContract(config : Axi4Config, id : Int, maxQueue : Int = maxFormalQueue) extends Component {
    val io = new Bundle {
      val arComplete = slave(Flow(UInt(config.lenWidth bits)))
      val r = slave(Flow(Bool()))

      val outstandingReads = out UInt(32 bits)
      val outstandingBursts = out UInt(log2Up(maxQueue + 1) bits)
    }

    val counter = Counter(config.lenWidth bits, inc = io.r.fire)
    val arFifo = createFifo(io.arComplete, maxQueue, latency = 1)
    io.outstandingBursts := arFifo.io.occupancy

    withAutoPull()
    val unfinishedArs = addTree(test_funcs.formalMapRam(arFifo).map(x => x.valid ? (x.value +^ 1) | U(0)))
    val deducedReads = unfinishedArs - counter.value
    io.outstandingReads := deducedReads.resized

    val rFlow = io.r.takeWhen(io.r.payload).translateWith(counter.value)
    when(io.r.fire && io.r.payload) {
      counter.clear()
    }

    val noArForRead, mismatchedLength = CombInit(False)

    when(io.r.fire || counter > 0) {
      noArForRead := arFifo.io.pop.valid === False
    }

    val arFifoNext = arFifo.io.pop.valid ? arFifo.io.pop.payload | U(0)
    arFifo.io.pop.ready := rFlow.fire
    when(rFlow.fire) {
      mismatchedLength := arFifoNext =/= rFlow.payload
    } elsewhen(io.r.fire) {
      mismatchedLength := arFifoNext < counter.valueNext
    } otherwise {
      mismatchedLength := arFifoNext < counter.value
    }

    def formalIsConsumerValid() = new FormalProperties(this) {
      addFormalProperty(!noArForRead, s"No AR for read")
      addFormalProperty(!mismatchedLength, s"Mismatched length")
    }

    def formalAssertEquivalence(that: Axi4ReadStreamContract) = new FormalProperties() {
      addFormalProperty(counter === that.counter)
      addFormalProperty(unfinishedArs === that.unfinishedArs)
      addFormalProperties(test_funcs.formalAssertEquivalence(arFifo, that.arFifo))
    }
  }

  class Axi4ReadContractEnforcer[T <: Axi4Ax](axType: HardType[T], config : Axi4Config, maxQueue : Int = maxFormalQueue) extends Component {
    val activeStreams = 1 << config.idWidth
    val io = new Bundle {
      val arComplete = slave Flow(axType)
      val r = slave Flow(Axi4R(config))

      val outstandingReadsPerId = out Vec(UInt(32 bits), activeStreams)
      val outstandingBurstsPerId = out Vec(UInt(log2Up(maxFormalQueue + 1) bits), activeStreams)
    }

    // The spec allows AW and W to stream through independently. So we can have all the W beats first and then the
    // AW beats follow or vice versa. This means we have to track this.
    val individualStreamContracts = (0 until (1 << config.idWidth)).map(id => Axi4ReadStreamContract(config, id)).toSeq
    individualStreamContracts.zipWithIndex.foreach(x => {
      val (c, idx) = x
      c.io.arComplete.valid := io.arComplete.fire && idx === io.arComplete.id
      c.io.arComplete.payload := io.arComplete.len

      c.io.r.valid := io.r.valid && idx === io.r.id
      c.io.r.payload := io.r.last

      io.outstandingReadsPerId(idx) := c.io.outstandingReads
      io.outstandingBurstsPerId(idx) := c.io.outstandingBursts
    })

    def formalIsConsumerValid() = new FormalProperties(this) {
      addFormalProperties(individualStreamContracts.flatMap(_.formalIsConsumerValid()))
    }

    def formalAssertEquivalence(that: Axi4ReadContractEnforcer[_]) =  {
      individualStreamContracts.zip(that.individualStreamContracts).flatMap(x => x._1.formalAssertEquivalence(x._2))
    }
  }

  class Axi4ReadContract[T <: Axi4Ax](bus: Nameable, ar: Stream[T], r: Stream[Axi4R], is_write: Bool = False) extends Composite(bus) {
    val enforcer = new Axi4ReadContractEnforcer(ar.payload, r.config)
    enforcer.setWeakName(s"axi_${bus.name}_read_enforcer")

    enforcer.io.arComplete <> ar.toFlowFire
    enforcer.io.r <> validFire(r)

    val outstandingBurstsPerId = enforcer.io.outstandingBurstsPerId
    val outstandingReadsPerId = enforcer.io.outstandingReadsPerId.zipWithIndex.map(x => x._1 +^ RegNext((r.id === x._2 && r.isStall).asUInt, U(0)))
    val outstandingReads = addTree(outstandingReadsPerId)

    def formalIsProducerValid() = new FormalProperties(bus) {
      addFormalProperties(StreamFormal.formalIsProducerValid(ar))
    }

    def formalIsConsumerValid() = new FormalProperties(bus) {
      addFormalProperties(StreamFormal.formalIsProducerValid(r))
      addFormalProperties(enforcer.formalIsConsumerValid())
    }

    def formalAssertEquivalence(that: Axi4ReadContract[_]) = {
      enforcer.formalAssertEquivalence(that.enforcer)
    }
  }

  class Axi4WriteContractEnforcer[T <: Axi4Ax](axType: HardType[T], config : Axi4Config, maxQueue : Int = maxFormalQueue) extends ComponentWithFormalProperties {
    val io = new Bundle {
      val aw = slave Flow(axType)
      val awComplete = slave Flow(axType)

      val b = slave Flow(Axi4B(config))

      val w = slave Flow(Axi4W(config))
      val wComplete = slave Flow(Axi4W(config))
    }
    val (aw, w, b) = (io.aw, io.w, io.b)

    // The spec allows AW and W to stream through independently. So we can have all the W beats first and then the
    // AW beats follow or vice versa. This means we have to track this.
    val beatCounter = Counter(aw.len.maxValue, inc = w.fire)
    val beatLengthFifo = createFifo(w.takeWhen(w.last).translateWith(beatCounter.value), maxQueue, latency = 1)
    when(beatLengthFifo.io.push.fire) {
      beatCounter.clear()
    }
    assume(beatLengthFifo.io.push.ready)


    // When 'LAST' exists, it is legal to have FEWER beats than LEN but you can never have more beats than specified in
    // LEN. So we track AW lens and counted beats and compare them as we have the info for them. 128 is arbitrary but
    // it is unlikey that this limits our proving power realistically.
    val awLengthFifo = createFifo(aw.map(_.len), maxQueue, latency = 1)
    val beatStream = StreamJoin(awLengthFifo.io.pop, beatLengthFifo.io.pop)
    beatStream.freeRun()

    val writeLastCounters = Vec(RegInit(U(0, 8 bits)), 1 << config.idWidth)
    // Assume no overflows
    writeLastCounters.foreach(c => assume(c.getAheadValue() =/= 0 || !c.andR))

    val lastCounterVerify = Flow(writeLastCounters.dataType)
    lastCounterVerify.setIdle()

    val awCompleteFifo = createFifo(io.awComplete.map(_.asInstanceOf[Axi4Ax]), maxQueue)
    val wCompleteFifo = createFifo(io.wComplete.takeWhen(io.wComplete.last), maxQueue)
    val awComplete = StreamJoin(awCompleteFifo.io.pop, wCompleteFifo.io.pop).toFlow.map(_._1)
    awComplete.freeRun()

    when(awComplete.fire) {
      writeLastCounters(awComplete.id) := writeLastCounters(awComplete.id) + 1
    }

    when(b.fire) {
      lastCounterVerify.valid := True
      lastCounterVerify.payload := writeLastCounters(b.id)
      writeLastCounters(b.id) := writeLastCounters(b.id) - 1
    }

    def formalIsProducerValid() = new FormalProperties(this) {
      when(beatStream.fire) {
        addFormalProperty(beatStream.payload._1 >= beatStream.payload._2, "Beats mismatch -- sent more than specified")
      }

      when(lastCounterVerify.valid) {
        addFormalProperty(lastCounterVerify.payload =/= 0, "Last count write verification")
      }
    }

    def formalIsConsumerValid() = new FormalProperties(this) {
      when(lastCounterVerify.fire) {
        addFormalProperty(lastCounterVerify.payload =/= 0, "Last counter verify")
      }
    }

    def formalAssertEquivalence(that: Axi4WriteContractEnforcer[_]) = new FormalProperties {
      addFormalProperty(writeLastCounters === that.writeLastCounters)
      addFormalProperty(beatCounter === that.beatCounter)
      addFormalProperties(test_funcs.formalAssertEquivalence(awCompleteFifo, that.awCompleteFifo))
      addFormalProperties(test_funcs.formalAssertEquivalence(wCompleteFifo, that.wCompleteFifo))
      addFormalProperties(test_funcs.formalAssertEquivalence(awLengthFifo, that.awLengthFifo))
      addFormalProperties(test_funcs.formalAssertEquivalence(beatLengthFifo, that.beatLengthFifo))
    }
  }

  class Axi4WriteContract[T <: Axi4Ax](bus: Nameable, aw: Stream[T], w: Stream[Axi4W], b: Stream[Axi4B], is_write: Bool) extends Composite(bus, "writeContract") {
    require(w.payload.id == null)

    val enforcer = new Axi4WriteContractEnforcer(aw.payload, aw.config)
    enforcer.setWeakName(s"axi_${bus.name}_write_enforcer")
    enforcer.io.aw <> validFire(aw).takeWhen(is_write)
    enforcer.io.awComplete <> aw.toFlowFire.takeWhen(is_write)
    enforcer.io.wComplete <> w.toFlowFire

    enforcer.io.b <> validFire(b)
    enforcer.io.w <> validFire(w)

    def formalIsProducerValid() = new FormalProperties(bus) {
      Seq(aw, w).foreach(s => addFormalProperties(StreamFormal.formalIsProducerValid(s)))
      addFormalProperties(enforcer.formalIsProducerValid())
    }

    def formalIsConsumerValid() = new FormalProperties(bus) {
      addFormalProperties(enforcer.formalIsConsumerValid())
      addFormalProperties(StreamFormal.formalIsProducerValid(b))
      //addFormalProperty(!outstandingWriteLasts.willUnderflow, "Outstanding write lasts should not go negative")
    }

    def formalAssertEquivalence(that: Axi4WriteContract[_]) = {
      enforcer.formalAssertEquivalence(that.enforcer)
    }
  }

  val readContracts = new mutable.WeakHashMap[Axi4Bus, Axi4ReadContract[_]]()
  val writeContracts = new mutable.WeakHashMap[Axi4Bus, Axi4WriteContract[_]]()

  object Axi4ReadContract {
    def apply(axi: Axi4Shared) = {
      new Axi4ReadContract(axi, axi.arw, axi.r, axi.arw.write)
    }

    def apply(axi: Axi4) = {
      new Axi4ReadContract(axi, axi.ar, axi.r)
    }

    def apply(axi: Axi4ReadOnly) = {
      new Axi4ReadContract(axi, axi.ar, axi.r)
    }
  }

  object Axi4WriteContract {
    def apply(axi: Axi4Shared) = {
      new Axi4WriteContract(axi, axi.arw, axi.w, axi.b, axi.arw.write)
    }

    def apply(axi: Axi4) = {
      new Axi4WriteContract(axi, axi.aw, axi.w, axi.b, True)
    }

    def apply(axi: Axi4WriteOnly) = {
      new Axi4WriteContract(axi, axi.aw, axi.w, axi.b, True)
    }
  }

  abstract class Axi4BaseExt(bus: Data with IMasterSlave) extends FormalMasterSlave with FormalDataWithEquivalnce[Axi4BaseExt] {
    val readContract: Axi4ReadContract[_] = null
    val writeContract: Axi4WriteContract[_] = null

    /**
     * @return True if and only if the driving signals are valid
     */
    override def formalIsProducerValid() = new FormalProperties(bus) {
      if (readContract != null) {
        addFormalProperties(readContract.formalIsProducerValid())
      }
      if (writeContract != null) {
        addFormalProperties(writeContract.formalIsProducerValid())
      }
    }

    /**
     * @return True if and only if the response signals are valid
     */
    override def formalIsConsumerValid() = new FormalProperties(bus) {
      if (readContract != null) {
        addFormalProperties(readContract.formalIsConsumerValid())
      }
      if (writeContract != null) {
        addFormalProperties(writeContract.formalIsConsumerValid())
      }
    }

    override def underlyingData: Data = bus

    override def asIMasterSlave: IMasterSlave = bus

    override def selfClassTag: ClassTag[Axi4BaseExt] = scala.reflect.classTag[Axi4BaseExt]

    override def formalAssertEquivalence(that: Axi4BaseExt): Unit = {
      if (readContract != null) {
        this.readContract.formalAssertEquivalence(that.readContract).foreach(_(ASSERT))
      }
      if (this.writeContract != null) {
        this.writeContract.formalAssertEquivalence(that.writeContract).foreach(_(ASSERT))
      }
    }
  }

  implicit class Axi4SharedFormalExt(bus: Axi4Shared) extends Axi4BaseExt(bus) {
    override val readContract = readContracts.getOrElseUpdate(bus, Axi4ReadContract(bus))
    override val writeContract: Axi4WriteContract[_] = writeContracts.getOrElseUpdate(bus, Axi4WriteContract(bus))
  }

  implicit class Axi4FormalExt(bus: Axi4) extends Axi4BaseExt(bus) {
    override val readContract = readContracts.getOrElseUpdate(bus, Axi4ReadContract(bus))
    override val writeContract = writeContracts.getOrElseUpdate(bus, Axi4WriteContract(bus))
  }


  implicit class Axi4ReadOnlyExt(bus: Axi4ReadOnly) extends Axi4BaseExt(bus) {
    override val readContract = readContracts.getOrElseUpdate(bus, Axi4ReadContract(bus))
  }

  implicit class Axi4WriteOnlyExt(bus: Axi4WriteOnly) extends Axi4BaseExt(bus) {
    override val writeContract = writeContracts.getOrElseUpdate(bus, Axi4WriteContract(bus))
  }
}