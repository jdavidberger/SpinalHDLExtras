package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.formal.{FormalDut, anyseq}
import spinal.lib.Counter
import spinalextras.lib.{Memories, MemoryRequirement}
import spinal.core.formal._
import spinalextras.lib.testing.FormalTestSuite

import java.io.IOException
import scala.language.postfixOps

class MemoriesFormal[T <: Data](reqs : MemoryRequirement[T], technologyKind: MemTechnologyKind = auto,
                                checkResponses : Boolean = false) extends Component {
  val dut = FormalDut(Memories.factory(reqs, technologyKind))
  assumeInitial(ClockDomain.current.isResetActive)

  dut.io.readPorts.foreach(p => {
    anyseq(p.cmd)
  })
  dut.io.writePorts.foreach(p => {
    anyseq(p.cmd)
  })
  dut.io.readWritePorts.foreach(p => {
    anyseq(p.cmd)
  })
}


class MemoriesFormalTest extends AnyFunSuite with FormalTestSuite {

  val reqs = Seq(
    MemoryRequirement(UInt(8 bits), 1000, 0, 1, 1),
    MemoryRequirement(UInt(8 bits), 512, 1, 0, 0)
  )

  override def defaultDepth() = 200

  formalTests().foreach(t => test(t._1) { t._2() })

  override def generateRtl(): Seq[(String, () => Component)] = reqs.map(r => (r.toString, () => new MemoriesFormal(r)))
}