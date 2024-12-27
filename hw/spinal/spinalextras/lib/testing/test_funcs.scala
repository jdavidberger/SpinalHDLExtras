package spinalextras.lib.testing


import spinal.core.native.globalData
import spinal.core.sim._
import spinal.core._
import spinal.core.formal.{FormalConfig, SpinalFormalConfig, past}
import spinal.lib.{CounterUpDown, Stream, StreamFifo}
import spinal.lib.bus.simple.PipelinedMemoryBus
import spinal.lib.sim.{ScoreboardInOrder, StreamDriver, StreamMonitor}
import spinalextras.lib.Config
import spinalextras.lib.logging.{GlobalLogger, SignalLogger}
import spinalextras.lib.misc.{AsyncStream, ComponentWithKnownLatency}

import java.io.IOException
import scala.collection.mutable
import scala.language.postfixOps

object test_funcs {
  def assertAsyncStreamContract[T <: Data](stream: AsyncStream[T]) = new Area {
    //if (globalData.config.flags.contains(GenerationFlags.simulation)) {

    val wasValid = RegNext(stream.async_valid) init (False)
    val wasReady = RegNext(stream.async_ready) init (False)
    val wasFired = RegNext(stream.async_fire) init (False)

    val invalidValidChange = wasValid && !stream.async_valid && !wasFired
    invalidValidChange.setWeakName(stream.name + "_invalidValidChange")
    assert(!invalidValidChange, s"${stream} deasserted async_valid before a async_ready")

    val outstanding_cnt = CounterUpDown(1L << 16, stream.async_fire, stream.flow.valid)
    assume(~outstanding_cnt.willOverflow)

    val async_flow_bounded = Bool()
    async_flow_bounded := outstanding_cnt > 0 || ~outstanding_cnt.decrementIt
    assume(async_flow_bounded)//, s"${pmb} PMB has miscounted responses")
    //}
  }
  def assumeStreamContract[T <: Data](stream: Stream[T]): Unit = {
    val wasValid = RegNext(stream.valid) init (False)
    val payload = RegNext(stream.payload)
    val wasFired = RegNext(stream.fire) init (False)

    assume(stream.valid || ~wasValid || wasFired)
    when(stream.valid && wasValid && !wasFired) {
      assume(stream.payload === past(stream.payload))
    }
  }

  def formalFifoAsserts[T <: Data](fifo: StreamFifo[T]) = new Area {
    val push_pop_occupancy : UInt = {
      if(fifo.logic != null) {
        if(fifo.withExtraMsb) {
          Mux(fifo.logic.ptr.push >= fifo.logic.ptr.pop,
            fifo.logic.ptr.push -^ fifo.logic.ptr.pop,
            (fifo.depth << 1) -^ fifo.logic.ptr.pop +^ fifo.logic.ptr.push)
        } else {
          Mux(fifo.logic.ptr.push === fifo.logic.ptr.pop,
            Mux(fifo.logic.ptr.wentUp, fifo.depth, 0),
            Mux(fifo.logic.ptr.push > fifo.logic.ptr.pop, fifo.logic.ptr.push -^ fifo.logic.ptr.pop,
              if(!fifo.withExtraMsb) {
                fifo.depth -^ fifo.logic.ptr.pop +^ fifo.logic.ptr.push
              } else {
                fifo.depth -^ fifo.logic.ptr.pop.dropHigh(1).asUInt +^ fifo.logic.ptr.push.dropHigh(1).asUInt
              }
            )
          )
        }
      } else {
        0
      }
    }

    val extraOccupancy : UInt = {
      if(fifo.logic != null && fifo.logic.pop.sync != null) {
        fifo.logic.pop.sync.readArbitation.valid.asUInt
      } else {
        0
      }
    }

    val calculate_occupancy = {
      push_pop_occupancy +^ extraOccupancy
    }

    if(fifo.depth <= 1) {
      // Fifo depth of 0 is just a direct connect
      // Fifo depth of 1 is just a staged stream; so it can't be in an invalid state
    } else if(fifo.depth > 1) {

      assert(calculate_occupancy === fifo.io.occupancy)
      assert(fifo.io.availability === fifo.depth -^ fifo.io.occupancy)

      when(fifo.io.occupancy === 0) {
        assert(fifo.io.pop.valid === (fifo.io.push.fire && Bool(fifo.withBypass)), "Occupancy check didn't result in right pop valid")
      }

      if (fifo.logic != null) {
        when(fifo.logic.ptr.pop === 0 && Bool(!isPow2(fifo.depth))) {
          assert(fifo.logic.ptr.popOnIo === ((fifo.depth - extraOccupancy) % fifo.depth))
        } otherwise {
          assert(fifo.logic.ptr.popOnIo === (fifo.logic.ptr.pop - extraOccupancy))
        }

        when(fifo.io.availability === 0) {
          assert(fifo.io.push.ready === False)
        }

        if (!fifo.withExtraMsb) {
          assert(fifo.logic.ptr.pop <= (fifo.depth - 1))
          assert(fifo.logic.ptr.push <= (fifo.depth - 1))
        } else {
          assert(fifo.logic.ptr.pop <= ((fifo.depth << 1) - 1))
          assert(fifo.logic.ptr.push <= ((fifo.depth << 1) - 1))
        }

        if (fifo.forFMax) {
          val counterWidth = log2Up(fifo.depth) + 1
          val emptyStart = 1 << (counterWidth - 1)
          val fullStart = (1 << (counterWidth - 1)) - fifo.depth

          assert(fifo.logic.ptr.arb.fmax.fullTracker.value === (fullStart +^ fifo.io.occupancy))
          assert(fifo.logic.ptr.arb.fmax.emptyTracker.value === (emptyStart -^ push_pop_occupancy))
        }
      }

      //assume(fifo.io.occupancy < past(fifo.io.occupancy) || ((past(fifo.io.occupancy) - fifo.io.occupancy) <= 1))
    }
  }

  def assertStreamContract[T <: Data](stream: Stream[T]): Unit = {
    val wasValid = RegNext(stream.valid) init (False)
    val payload = RegNext(stream.payload)
    val wasFired = RegNext(stream.fire) init (False)

    val invalidValidChange = wasValid && !stream.valid && !wasFired
    invalidValidChange.setWeakName(stream.name + "_invalidValidChange")
    assert(!invalidValidChange, s"${stream} deasserted valid before a ready")

    val payloadChangeViolation = False
    payloadChangeViolation.setWeakName(stream.name + "_payloadChangeViolation")
    when(stream.valid && wasValid && !wasFired) {
      payloadChangeViolation := payload =/= stream.payload

      assert(payload === stream.payload, Seq(
        s"${stream} payload changed from ",
        payload.asBits,
        " to ",
        stream.payload.asBits
      ))
    }

  }

  def assign(payload: SInt, v: Int): Unit = {
    payload #= v
  }

  def recover(payload: SInt): Int = {
    payload.toInt
  }

  def doTest
  [InT <: Data, OutT <: Data, TestInT, TestOutT, CT <: Component](dut: CT,
                                                                  Input: Stream[InT], Output: Stream[OutT],
                                                                  testInput: Iterable[TestInT], expectedOutput: Iterable[TestOutT],
                                                                  assign: (InT, TestInT) => Unit,
                                                                  recover: OutT => TestOutT,
                                                                  factor: Option[Float] = None,
                                                                  compare: (TestOutT, TestOutT) => Boolean = (a: TestOutT, b: TestOutT) => a == b,
                                                                  timeout: TimeNumber = 100 us, setup: (CT) => Unit = ((c: CT) => {}), latency1to1: Boolean = true,
                                                                 ): Unit = {
    Output.ready #= false
    Input.valid #= false
    dut.clockDomain.forkStimulus(100 MHz)
    dut.clockDomain.waitSampling()
    setup(dut)

    SimTimeout(timeout)

    val outerCompare = compare
    val scoreboard = new ScoreboardInOrder[TestOutT] {
      override def compare(ref: TestOutT, dut: TestOutT) = outerCompare(ref, dut)
    }
    for (e <- expectedOutput) {
      scoreboard.pushRef(e)
    }

    var (driver, queue) = StreamDriver.queue(Input, dut.clockDomain)
    if (factor.isDefined) {
      driver.setFactor(factor.get)
    }

    for (i <- testInput) {
      queue += (payload => assign(payload, i))
    }

    var sysclk: BigInt = 0
    dut.clockDomain.onSamplings({
      sysclk = sysclk + 1
    })

    val sysclkIn = new mutable.ArrayBuffer[BigInt]()
    val sysclkOut = new mutable.ArrayBuffer[BigInt]()
    StreamMonitor(Input, dut.clockDomain) { payload =>
      sysclkIn.append(sysclk)
    }

    var wait_count = 30
    StreamMonitor(Output, dut.clockDomain) { payload =>
      scoreboard.pushDut(recover(payload))
      sysclkOut.append(sysclk)
      wait_count = 30
    }
    Output.ready #= true

    waitUntil(queue.isEmpty)
    waitUntil(scoreboard.ref.size == scoreboard.dut.size)
    while (wait_count > 0) {
      val wait_count_tmp = wait_count
      wait_count = 0
      dut.clockDomain.waitSampling(wait_count_tmp)
    }

    val latencies = sysclkOut.zip(sysclkIn).map(x => x._1 - x._2).map(_.toInt)
    println(s"${sysclkIn}")
    println(s"${sysclkOut}")
    println(s"Done! ${latencies}")

    dut match {
      case staged: ComponentWithKnownLatency => {


        println(s"Latency of the DUT: ${staged.latency} / ${latencies.min} / ${latencies.max} (factor: ${factor})")

        var latencyCompareVal = latencies.head
        if (factor.getOrElse(0) == 1) {
          val cyclesPerElement = (sysclkIn.last - sysclkIn.head + 1).toInt / sysclkIn.length
          println(s"cyclesPerElement ${cyclesPerElement}", (sysclkIn.last - sysclkIn.head).toInt, sysclkIn.length)
          assert(cyclesPerElement == staged.pipelineCyclesPerElement(), s"Values should equal ${cyclesPerElement} vs ${staged.pipelineCyclesPerElement()}")
          if (latency1to1) {
            latencyCompareVal = latencies.max
          }
        }
        if (latency1to1 || factor.getOrElse(0) == 1) {
          assert(staged.latency == latencyCompareVal, s"Values should match ${staged.latency} == ${latencyCompareVal}")
        }
      }
      case _ => {
        println(s"Latency of the DUT: ${latencies.min} / ${latencies.max}")
      }
    }
    scoreboard.checkEmptyness()
  }
  def willUnderflow(c : CounterUpDown) = {
    (c.value === 0) && c.decrementIt && ~c.incrementIt
  }
  def assertPMBContract(pmb: PipelinedMemoryBus) = {
    new Area {
      test_funcs.assertStreamContract(pmb.cmd)

      val outstanding_cnt = CounterUpDown(1L << 16, pmb.cmd.fire && !pmb.cmd.write, pmb.rsp.valid)
      assume(~outstanding_cnt.willOverflow)

      val pmb_rsp_bounded = Bool()
      pmb_rsp_bounded := outstanding_cnt > 0 || ~outstanding_cnt.decrementIt
      assume(pmb_rsp_bounded)//, s"${pmb} PMB has miscounted responses")

//      SignalLogger.log_assert(pmb_rsp_bounded,
//        s"${pmb} PMB has miscounted responses")

    }.setName(s"${pmb.name}_assertPMBContract")
  }

  var fastClockDomain: Option[ClockDomain] = None

  def getFastClockDomain: ClockDomain = {
    if (fastClockDomain.isEmpty) {
      fastClockDomain = Some(ClockDomain.external("fastClock", frequency = FixedFrequency(200 MHz)).setSynchronousWith(ClockDomain.current))
    }
    fastClockDomain.get
  }

  def doTestFactory[InT <: Data, OutT <: Data, TestInT, TestOutT, CT <: Component](factory: () => (CT, Stream[InT], Stream[OutT]),
                                                                                   testInput: Iterable[TestInT], expectedOutput: Iterable[TestOutT],
                                                                                   assign: (InT, TestInT) => Unit,
                                                                                   recover: OutT => TestOutT,
                                                                                   compare: (TestOutT, TestOutT) => Boolean = (a:TestOutT, b:TestOutT) => a == b,
                                                                                   setup: (CT) => Unit = (c:CT) => (), latency1to1 : Boolean = true): Unit = {
    for (factor <- Vector[Option[Float]](Some(1), Some(.1f), None)) {
      var inStream: Stream[InT] = null
      var outStream: Stream[OutT] = null
      Config.sim.doSim({
        val (dut, is, os) = factory()
        inStream = is
        outStream = os
        dut
      }) { dut =>
        test_funcs.fastClockDomain.foreach(_.forkStimulus(200 MHz))
        test_funcs.doTest(dut, inStream, outStream, testInput, expectedOutput, assign, recover, factor = factor, compare, setup = setup, latency1to1 = latency1to1)
        test_funcs.fastClockDomain = None
      }
    }
  }
}


trait FormalTestSuite {
  val config = FormalConfig._spinalConfig.copy(defaultConfigForClockDomains = ClockDomainConfig(
    resetActiveLevel = HIGH,
    resetKind = SYNC,
  ),
    mergeAsyncProcess = true,
    defaultClockDomainFrequency = FixedFrequency(100 MHz))

  def generateRtl() : Seq[(String, () => Component)]
  def generateRtlBMC() = generateRtl()
  def generateRtlCover() = generateRtl()
  def generateRtlProve() = generateRtl()

  def defaultDepth() = 100
  def BMCConfig() : SpinalFormalConfig = FormalConfig.withConfig(config).withBMC(defaultDepth())
  def CoverConfig() : SpinalFormalConfig = FormalConfig.withConfig(config).withCover(defaultDepth())
  def ProveConfig() : SpinalFormalConfig = FormalConfig.withConfig(config).withProve(defaultDepth())

  def renameDefinition(c : Component, suffix : String) = {
    c.setDefinitionName(c.getClass.getSimpleName + "_" + suffix)
  }

  def formalTests() : Seq[(String, () => Any)] = {
    (/*generateRtlBMC().map(lst => (s"${lst._1}_bmc", () => BMCConfig().doVerify(renameDefinition(lst._2(), "bmc")))) ++*/
      generateRtlProve().map(lst => (s"${lst._1}_prove", () => ProveConfig().doVerify(renameDefinition(lst._2(), "prove")))) ++
      generateRtlCover().map(lst => (s"${lst._1}_cover", () => CoverConfig().doVerify(renameDefinition(lst._2(), "cover"))))).map(t => {
      (t._1, () => {
        try {
          t._2()
        } catch {
          case e : IOException => {
            println(s"Could not find sby ${e}")
          }
        }
      })
    })
  }
}