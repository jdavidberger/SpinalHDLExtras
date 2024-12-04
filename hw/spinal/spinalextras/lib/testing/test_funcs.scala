package spinalextras.lib.testing


import spinal.core.native.globalData
import spinal.core.sim._
import spinal.core._
import spinal.lib.{CounterUpDown, Stream}
import spinal.lib.bus.simple.PipelinedMemoryBus
import spinal.lib.sim.{ScoreboardInOrder, StreamDriver, StreamMonitor}
import spinalextras.lib.Config
import spinalextras.lib.logging.{GlobalLogger, SignalLogger}
import spinalextras.lib.misc.ComponentWithKnownLatency

import scala.collection.mutable

object test_funcs {

  def assertStreamContract[T <: Data](stream: Stream[T]): Unit = {
    if (globalData.config.flags.contains(GenerationFlags.simulation)) {
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

  def assertPMBContract(pmb: PipelinedMemoryBus) = {
    new Area {
      test_funcs.assertStreamContract(pmb.cmd)

      val outstanding_cnt = CounterUpDown(1L << 32, pmb.cmd.fire && !pmb.cmd.write, pmb.rsp.valid)
      val pmb_rsp_bounded = outstanding_cnt.value.asBits.andR =/= True
      assert(pmb_rsp_bounded, s"${pmb} PMB has miscounted responses")

      GlobalLogger(
        Set("asserts"),
        SignalLogger.concat(s"${pmb.name}_asserts", pmb_rsp_bounded.setName(s"${pmb.name}_rsp_bounded"))
      )

    }.setName("assertPMBContract")
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