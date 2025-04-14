package spinal.tester.scalatest

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._
import spinal.core.formal.{FormalConfig, FormalDut, SmtBmc, anyseq}
import spinal.core.internals.AssertStatementKind
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalMasterSlave, FormalProperties, FormalProperty, HasFormalProperties}
import spinalextras.lib.testing.FormalTestSuite

import scala.language.postfixOps

case class TestBundle() extends Bundle with IMasterSlave with FormalMasterSlave {
  val input = Bits(32 bits)
  val never_true = Bool()

  override def asMaster(): Unit = {
    out(input)
    in(never_true)
  }

  override def formalIsProducerValid() = input =/= 0xdeadbeefL
  override def formalIsConsumerValid() = never_true === False
}

class ChildComponent(val addHelperAssertion : Boolean = true) extends ComponentWithFormalProperties {
  val io = new Bundle {
    val contract = slave(TestBundle())
  }

  var shifter = Reg(Bits(16 bits)) init(0)
  shifter := (shifter << 1).resize(15 bits) ## (io.contract.input === 0xdeadbeefL)
  io.contract.never_true := shifter.msb

  override def formalProperties() = new FormalProperties {
    if(addHelperAssertion) {
      // When doing formal induction with less than shifter.getBitsWidth states, we need to help the solver out --
      // the current state of sby isn't able to resolve this property on it's own.
      addFormalProperty(shifter === 0)
    }
  }.implicitValue ++ super.formalProperties()
}

class FormalChildComponent(val addHelperAssertion : Boolean = false) extends Component {
  val dut = FormalDut(new ChildComponent(addHelperAssertion))
  assumeInitial(ClockDomain.current.isResetActive)

  dut.anyseq_inputs()
  HasFormalProperties.printFormalAssertsReport()
}

class ParentComponent() extends ComponentWithFormalProperties {
  val io = new Bundle {
    val contract = slave(TestBundle())
  }

  val child = new ChildComponent()
  child.io <> io
}

class FormalParentComponent(assumeInputs : Boolean = true) extends Component {
  val dut = FormalDut(new ParentComponent())
  assumeInitial(ClockDomain.current.isResetActive)

  if(!assumeInputs) {
    dut.CurrentInputsAssertionKind = AssertStatementKind.ASSERT
  }

  dut.anyseq_inputs()
}

class FormalHasFormalTester extends AnyFunSuite {
  def shouldFail(body: => Unit) = assert(try {
    body
    false
  } catch {
    case e : Throwable => println(e); true
  })

  val config = FormalConfig._spinalConfig.copy(defaultConfigForClockDomains = ClockDomainConfig(
    resetActiveLevel = HIGH,
    resetKind = SYNC,
  ),
    mergeAsyncProcess = true,
    defaultClockDomainFrequency = FixedFrequency(100 MHz))

  val formalConfig = FormalConfig
    .withDebug
    .withConfig(config)
    .withEngies(Seq(SmtBmc(nopresat = false,
      //solver = SmtBmcSolver.Boolector,
      progress = false, //noincr = false,
      //track_assumes = true, minimize_assumes = true
    )))

  test("Check child") {
    // Show that if prove has a long enough induction length, it does not need the helper assertions.
    formalConfig.withProve(15).doVerify(new FormalChildComponent(addHelperAssertion = false))
  }

  test("CheckChildHelper") {
    // Show that if prove has the helper assertions, it does not need a long induction length
    formalConfig.withProve(5).doVerify(new FormalChildComponent(addHelperAssertion = true))
  }

  test("CheckChildNoHelper") {
    // Show that if prove does not have the helper assertions, and has too short of induction length, it fails.
    shouldFail(
      formalConfig.withProve(5).doVerify(new FormalChildComponent(addHelperAssertion = false))
    )
  }

  test("Check Parent") {
    // Show that when we assume the inputs are valid, it can prove it is valid.
    formalConfig.withProve(15).doVerify(new FormalParentComponent(assumeInputs = true))
  }

  test("Check Parent - no input assumes") {
    // Shows that if we don't assume proper inputs into the parent, that the assumptions in the child class are correctly
    // predicated on the inputs being correct
    shouldFail(
      formalConfig.withProve(15).doVerify(new FormalParentComponent(assumeInputs = false))
    )
  }
}