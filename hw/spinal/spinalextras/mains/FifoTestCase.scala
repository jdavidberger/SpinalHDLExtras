package spinalextras.mains

import spinal.core.ClockDomain.FixedFrequency
import spinal.core._
import spinal.lib._
import spinalextras.lib.Config

import scala.language.postfixOps

/**
 * Test case for what seems like an optimization bug in synplify.
 *
 * The bug is that a standard FIFO gets corrupted pop data due to the optimizer using the register input for the read
 * address instead of the register output. This can cause the read to happen on the same cycle as the write thus getting
 * bad data out.
 *
 * There is an option to add logic to patch out read / write ram checks which sort of addresses the problem, but it isn't
 * clear at all that the bug can't be tripped over in some other way.
 *
 * Oddly this optimization only happens when SYNC style resets are used; ASYNC style resets do not have the same issue.
 *
 * This bug can be seen when running in questasim from lattice radiant 2025.1.1.308.0's linux release when using
 * synplify for synthesis to generate a _syn.vo file. Running the same simulation using just the verilog source or using
 * the _syn.vo file from LSE does not have the issue.
 */
class FifoTestCase extends Component {
  val io = new Bundle {
    val led = out(Bool())
  }
  io.led := False

  noIoPrefix()

  // A wide range of values here exhibit the issue. If it gets small enough to get away with something
  // smaller than PDPSC16K though the issue seemed to go away
  val width = 32
  val fifo = StreamFifo(Bits(width bits), 64)

  val counterFreeRun = RegInit(U(0, 3 bits))
  counterFreeRun := counterFreeRun + 1

  val inputPayload = RegInit(B(0xcafe, width bits))

  val pushTimeoutReached = CombInit(counterFreeRun === 0)
  val inputValid = RegInit(False) setWhen(pushTimeoutReached) clearWhen(fifo.io.push.fire)

  fifo.io.push.payload := inputPayload
  fifo.io.push.valid := inputValid

  val inputFire = KeepAttribute(CombInit(fifo.io.push.fire))
  when(inputFire) {
    // We just need to change the input up so the optimizer doesn't touch it
    inputPayload := (inputPayload |<< 1) | 1
  }

  fifo.io.pop.ready := pushTimeoutReached

  val fifoOutput = KeepAttribute(CombInit(fifo.io.pop.payload))
  val fifoOutputFire = KeepAttribute(CombInit(fifo.io.pop.fire))

  // Make sure the fifo bits aren't optimized away
  when(fifoOutputFire) {
    io.led := fifoOutput.xorR
  }
}

object FifoTestCase{
  def main(args: Array[String]) {
    val source = scala.io.Source.fromFile("SpinalHDLExtras/hw/spinal/spinalextras/mains/FifoTestCase.scala")

    Config.spinal.copy(
        defaultClockDomainFrequency = FixedFrequency(60 MHz),
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC),
      /*
      Only SYNC resets causes the issue, BOOT and ASYNC do not exhibit the same problem.
       */
        //defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC),
        //defaultConfigForClockDomains = ClockDomainConfig(resetKind = BOOT),
        rtlHeader = f"""
```
${source.mkString}
```
    """
      )
    .generateVerilog(new FifoTestCase())
  }
}

class CounterTestCase extends Component {
  val io = new Bundle {
    val fire = in Bool()
    val last = in Bool()

    val output = out UInt(1 bits)
  }
  val lastFire = KeepAttribute(CombInit(io.fire && io.last))
  val counter = RegInit(U(0, 1 bits))
  when(io.fire) {
    counter := counter + 1
  }

  val fragmentCounter = RegInit(counter)
  when(lastFire) {
    fragmentCounter := counter
  }

  io.output := fragmentCounter
}

object CounterTestCase{
  def main(args: Array[String]) {

    Config.spinal.copy(
        defaultClockDomainFrequency = FixedFrequency(60 MHz),
        defaultConfigForClockDomains = ClockDomainConfig(resetKind = ASYNC),
        //defaultConfigForClockDomains = ClockDomainConfig(resetKind = BOOT)
      )
      .generateVerilog(new CounterTestCase())
  }
}
