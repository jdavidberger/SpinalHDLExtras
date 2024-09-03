package spinalextras.lib.misc

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim._
import spinalextras.lib.Config
import spinalextras.lib.memory._

case class FragmentFIFO[T <: Data](dataType : HardType[Fragment[T]],
                                   depth : Int,
                                   elemsPerLast: Int,
                                   var fifoFactory : FifoFactory.create_fifo_fn[T] = FifoFactory.default[T] _) extends Component {
  val fifo = fifoFactory(depth, dataType().fragment)

  val io = slave(new FifoInterface[Fragment[T]](dataType, depth))
  io.occupancy := fifo.occupancy
  io.availability := fifo.availability

  val lastFifo = StreamFifo(UInt(log2Up(depth) bits), 1 + (depth + elemsPerLast) / elemsPerLast)
  val lastCounter = Counter(depth, inc = io.push.fire)
  lastFifo.io.push.payload := lastCounter.value
  lastFifo.io.push.valid := io.push.lastFire
  assert(~lastFifo.io.push.isStall, "Stalled lastFifo")
  when(io.push.lastFire) {
    lastCounter.clear()
  }

  val popCounter = Reg(UInt(log2Up(depth) bits)) init(0)
  when(fifo.pop.fire) {
    popCounter := popCounter + 1
  }

  val elemsTilLast = lastFifo.io.pop.payload
  lastFifo.io.pop.ready := False
  val isLast = (popCounter === elemsTilLast && lastFifo.io.pop.valid)
  val fifoWLast = fifo.pop.addFragmentLast(isLast)
  when(fifoWLast.lastFire) {
    popCounter := 0
    lastFifo.io.pop.ready := True
  }

  fifo.push << io.push.map(_.fragment)
  fifoWLast.stage() >> io.pop
}

class FragmentFifoFactory[T <: Data](elemsPerLast: Int,
                                     var fifoFactory : FifoFactory.create_fifo_fn[T] = FifoFactory.default[T] _) {
  def apply(depth: Int, dataType: HardType[Fragment[T]]) : FifoInterface[Fragment[T]] = {
    FragmentFIFO(dataType = dataType, depth = depth, elemsPerLast = elemsPerLast, fifoFactory = fifoFactory).io
  }
}

class FragmentFIFOTest extends AnyFunSuite {
  def doTest[T <: BitVector](dataType: HardType[T], depth: Int, disableReady: Boolean = true): Unit = {
    Config.sim.doSim(
       new FragmentFIFO(Fragment(dataType), depth, depth * 10)
    ) { dut =>
      SimTimeout(5000 us)
      dut.io.push.valid #= false
      dut.io.pop.ready #= false
      dut.io.flush #= false
      dut.clockDomain.forkStimulus(100 MHz)
      dut.clockDomain.waitSampling(10)

      dut.io.push.valid #= false

      val sco = new ScoreboardInOrder[(Int, Boolean)]()
      var allDone = false
      StreamMonitor(dut.io.pop, dut.clockDomain) {
        datum => {
          sco.pushDut((datum.fragment.toInt, datum.last.toBoolean))
          println(s"Got ${(datum.fragment.toInt, datum.last.toBoolean)}")
          if(disableReady && !allDone)
            dut.io.pop.ready #= false
        }
      }
      dut.io.pop.ready #= false

      for (i <- 0 until depth * 3) {
        dut.io.push.valid #= true
        val randValue = i //simRandom.nextLong().abs
        val isLast = (i % (depth - 1)) == 10
        println(s"Pushing ${randValue} / ${i}")
        dut.io.push.payload.fragment #= randValue

        dut.io.push.payload.last #= isLast
        sco.pushRef((randValue, isLast))

        if (!dut.io.pop.ready.toBoolean) {
          dut.io.pop.ready.randomize()
        }

        dut.clockDomain.waitSampling()
        while (!dut.io.push.ready.toBoolean) {
          if (!dut.io.pop.ready.toBoolean) {
            dut.io.pop.ready.randomize()
          }
          dut.clockDomain.waitSampling()
        }

      }
      allDone = true
      dut.io.push.valid #= false
      dut.clockDomain.waitSampling()
      dut.io.pop.ready #= true
      while(dut.io.occupancy.toInt > 0) {
        dut.clockDomain.waitSampling(100)
      }

      sco.checkEmptyness()
    }
  }

  test("basic") {
    doTest(Bits(32 bits), 1000)
  }

}