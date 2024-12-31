package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.formal.{FormalDut, anyseq}
import spinal.core._
import spinal.lib.{Counter, StreamFifo}
import spinalextras.lib.memory.MemoryPoolFIFOs
import spinalextras.lib.testing.test_funcs.formalAssumeLibraryComponents
import spinalextras.lib.testing.{FormalTestSuite, test_funcs}

import scala.language.postfixOps

class MemoryPoolFIFOsFormal[T <: Data](dataType: HardType[T],
                                       sizes: Seq[BigInt],
                                       check_flush : Boolean = false,
                                       checkResponses : Boolean = true) extends Component {
  val dut = FormalDut(new MemoryPoolFIFOs(Bits(8 bits), Seq(10)))
  assumeInitial(ClockDomain.current.isResetActive)

  dut.io.fifos.foreach(f => {
    test_funcs.assertStreamContract(f.pop)
    anyseq(f.pop.ready)

    if(check_flush) {
      anyseq(f.flush)
    } else {
      f.flush := False
    }

    test_funcs.assumeStreamContract(f.push)
    anyseq(f.push.valid)
    anyseq(f.push.payload)

    if(checkResponses) {
      val testFifo = StreamFifo(f.dataType, f.depth.toInt)
      testFifo.io.push.payload := f.push.payload
      testFifo.io.push.valid := f.push.fire
      testFifo.io.flush := f.flush
      test_funcs.assertStreamContract(testFifo.io.push)

      testFifo.io.pop.ready := f.pop.fire

      assert(f.occupancy === testFifo.io.occupancy)
      assert(f.pop.fire === False || testFifo.io.pop.fire)
      assert(f.pop.fire === False || (testFifo.io.pop.payload === f.pop.payload))
    }
  }
  )

  test_funcs.formalAssumeLibraryComponents()
}

class MemoryBackedFIFOsTestFormal extends AnyFunSuite with FormalTestSuite {
  formalTests().foreach(t => test(t._1) { t._2() })

  override def defaultDepth() = 15

  override def generateRtl() = Seq((suiteName, () => new MemoryPoolFIFOsFormal(UInt(8 bits), Seq(10, 50))))
  override def generateRtlCover() = Seq((suiteName, () => new MemoryPoolFIFOsFormal(UInt(8 bits), Seq(10, 50), checkResponses = false)))
  override def generateRtlProve() = Seq((suiteName, () => new MemoryPoolFIFOsFormal(UInt(8 bits), Seq(10, 50), checkResponses = false)))
}