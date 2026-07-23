package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.{IntToBuilder, UInt}
import spinal.lib.StreamArbiter.{FragmentLock, LowerFirst, NoLock, RoundRobin, SequentialOrder, TransactionLock}
import spinal.lib.{Fragment, StreamArbiter, StreamArbiterFactory}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

import scala.language.postfixOps


class StreamArbiterFormal extends AnyFunSuite with FormalTestSuite {

  override def defaultDepth() = 20

  formalTests().foreach(t => test(t._1) {
    t._2()
  })

  override def generateRtl() = {
    for (arbitrationPolicy <- Seq(LowerFirst, RoundRobin, SequentialOrder); lockPolicy <- Seq(NoLock, FragmentLock, TransactionLock))
    yield
      (s"StreamArbiter_${arbitrationPolicy.getClass.getSimpleName.replace("$", "")}_${lockPolicy.getClass.getSimpleName.replace("$", "")}", () =>
        GeneralFormalDut(() => new StreamArbiter(Fragment(UInt(2 bits)), 2, arbitrationPolicy, lockPolicy)))
  }
}


