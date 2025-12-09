package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal.{FormalDut, anyseq}
import spinal.lib.StreamFifo
import spinalextras.lib.formal.HasFormalProperties
import spinalextras.lib.memory._
import spinalextras.lib.testing.FormalTestSuite

import scala.language.postfixOps

case class MemoryPoolFIFOConfig[T <: Data](dataType: HardType[T],
                                           sizes: Seq[BigInt],
                                           check_flush : Boolean = false,
                                           checkResponses : Boolean = true,
                                           technologyKind: MemTechnologyKind = auto,
                                           mem_factory: (MemoryRequirement[T], MemTechnologyKind) => HardwareMemory[T] = Memories.apply[T] _,
                                           localFifoDepth: Int = 0) {
  override def toString : String = {
    s"MemoryPoolFIFO_${sizes.size}_${localFifoDepth}"
  }
}

class MemoryPoolFIFOsFormal[T <: Data](config : MemoryPoolFIFOConfig[T]) extends Component {
  import config._

  val dut = FormalDut(new MemoryPoolFIFOs(dataType, sizes,
    technologyKind = technologyKind,
    mem_factory = mem_factory, localFifoDepth = localFifoDepth
  ))
  assumeInitial(ClockDomain.current.isResetActive)

  dut.io.fifos.foreach(f => {
    anyseq(f.pop.ready)


    if(check_flush) {
      anyseq(f.flush)
    } else {
      f.flush := False

//      cover(f.push.fire)
//      cover(f.pop.fire)
    }

    anyseq(f.push.valid)
    anyseq(f.push.payload)

    if(checkResponses) new Composite(this, "checkResp") {
      val testFifo = StreamFifo(f.dataType, f.depth.toInt)
      testFifo.io.push.payload := f.push.payload
      testFifo.io.push.valid := f.push.fire
      testFifo.io.flush := f.flush

      testFifo.io.pop.ready := f.pop.fire

      assert(f.occupancy === testFifo.io.occupancy)
      assert(f.pop.fire === False || testFifo.io.pop.fire)
      assert(f.pop.fire === False || (testFifo.io.pop.payload === f.pop.payload))
    }
  }
  )
  HasFormalProperties.printFormalAssertsReport()
}

class MemoryBackedFIFOsTestFormal extends AnyFunSuite with FormalTestSuite {
  formalTests().foreach(t => test(t._1) { t._2() })

  override def defaultDepth() = 10

  lazy val testConfigs = Seq(
    MemoryPoolFIFOConfig(UInt(8 bits), Seq(10, 50)),
    MemoryPoolFIFOConfig(UInt(8 bits), Seq(10, 50), localFifoDepth = 2),
    //MemoryPoolFIFOConfig(UInt(8 bits), Seq(10, 50), localFifoDepth = 3)
  )

  override def generateRtl() : Seq[(String, () => Component)] =
    testConfigs.map(x => x.toString -> (() => new MemoryPoolFIFOsFormal(x)))

  override def generateRtlCover() = generateRtlProve()
  override def generateRtlProve() =
    testConfigs.map(x => x.toString -> (() => new MemoryPoolFIFOsFormal(x.copy(checkResponses = false))))
}