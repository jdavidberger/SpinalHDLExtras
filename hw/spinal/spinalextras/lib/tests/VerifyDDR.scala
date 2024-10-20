package spinalextras.lib.tests

import spinal.lib._
import spinal.core._
import spinal.core.sim._
import spinalextras.lib.Config
import spinalextras.lib.blackbox.lattice.lifcl.GSR
import spinalextras.lib.io.lattice.{LatticeIDDR, LatticeODDR}
import spinalextras.lib.io._

import scala.language.postfixOps

case class TestClockGen(ECLK_PERIOD : Double, SCLK_MULT : Double) extends BlackBox {
  val io = new Bundle {
    val sclk = out Bool()
    val eclk = out Bool()
    val eclk_90 = out Bool()
    val eclk_270 = out Bool()
    val reset = out Bool()
  }
  addGenerics(("ECLK_PERIOD", ECLK_PERIOD), ("SCLK_MULT", SCLK_MULT))
  noIoPrefix()
}

case class VerifyODDRTestBench(ddr_factor : Int = 4, ODDRFactory : (DDRRequirements) => ODDR) extends Component {
  val clockGen = TestClockGen(10, ddr_factor / 2)

  val eclk = clockGen.io.eclk
  val sclk = clockGen.io.sclk
  val reset = clockGen.io.reset

  val sclkDomain = new ClockDomain(sclk, reset = reset)
  val eclkDomain = new ClockDomain(eclk, reset = reset)

  val sclkArea = new ClockingArea(sclkDomain ) {
    val loddr = new ODDRS(UInt(8 bits), DDRRequirements(ddr_factor), ODDRFactory = Some(ODDRFactory))
    val goddr = new ODDRS(UInt(8 bits), DDRRequirements(ddr_factor), ODDRFactory = Some(ddr_factor => new GenericODDR(ddr_factor, latency = loddr.latency())))

    val counter = Counter(1 << 8)
    counter.increment()
    counter.value.addTag(crossClockDomain)
    var primes = Seq(1, 3, 5, 7, 11, 13, 17, 19, 23, 31, 111)
    for (oddr <- Seq(goddr, loddr)) {
      oddr.io.IN.valid := counter.value > 10
      for (i <- 0 until goddr.input_per_output) {
        oddr.io.IN.payload(i) := (counter.value + primes(i)).resized
      }
      oddr.io.ECLK := eclk
    }
  }

  val eclkArea = new ClockingArea(eclkDomain) {
    val valid = Reg(Bool()) init(True)

    valid.addTag(crossClockDomain)
    val goddr = sclkArea.goddr
    val loddr = sclkArea.loddr
    val ecounter = Counter(255)
    ecounter.increment()
    when(!goddr.io.IN.valid) {
      ecounter.clear()
    }

    when(goddr.io.OUT.valid) {
      valid := goddr.io.OUT.asBits === loddr.io.OUT.asBits
    } otherwise {
      valid := goddr.io.OUT.valid === loddr.io.OUT.valid
    }
    assert(valid, "goddr not valid")
  }
}

case class VerifyIDDRTestBench(ddr_factor : Int = 4, IDDRFactory : (DDRRequirements) => IDDR) extends Component {
  val clockGen = TestClockGen(10, ddr_factor / 2)

  val eclk = clockGen.io.eclk
  val eclk_90 = clockGen.io.eclk_90
  val sclk = clockGen.io.sclk
  val reset = clockGen.io.reset

  Clock.sync(sclk, eclk)
  Clock.sync(eclk_90, eclk)
  Clock.sync(clockGen.io.eclk_270, eclk)
  val sclkDomain = new ClockDomain(sclk, reset = reset)
  val eclkDomain = new ClockDomain(eclk, reset = reset)

  val eclkArea = new ClockingArea(eclkDomain) {
    val counter = CounterFreeRun(16)
  }
  val sclkArea = new ClockingArea(sclkDomain ) {
    val liddr = new IDDRS(UInt(8 bits), DDRRequirements(ddr_factor), IDDRFactory = Some(IDDRFactory))
    val giddr = new IDDRS(UInt(8 bits), DDRRequirements(ddr_factor), IDDRFactory = Some(ddr_factor => new GenericIDDR(ddr_factor, latency = liddr.latency())))

    val oddr = new ODDRS(UInt(8 bits), DDRRequirements(ddr_factor), ODDRFactory = Some(ddr_factor => new GenericODDR(ddr_factor)))
    val primes = Seq(1, 3, 5, 7, 11, 13, 17, 19, 23, 31, 111)

    val valid = Reg(Bool()) init(True)
    valid.addTag(crossClockDomain)
    when(giddr.io.OUT.valid) {
      valid := giddr.io.OUT.asBits === liddr.io.OUT.asBits && (giddr.io.OUT.valid === liddr.io.OUT.valid)
    } otherwise {
      valid := giddr.io.OUT.valid === liddr.io.OUT.valid
    }
    when(!valid) {
      report(Seq(s"Mismatch in giddr and liddr. Latency should be ${liddr.latency()} ", giddr.io.OUT.payload.asBits, " ", liddr.io.OUT.payload.asBits, " ", giddr.io.OUT.valid, " ", liddr.io.OUT.valid))
    }

    for(iddr <- Seq(giddr, liddr)) {
      when(iddr.io.OUT.valid) {
        report(Seq(s"${iddr.name} ", eclkArea.counter.value, " ", iddr.io.OUT.payload.asBits))
      }
    }
    when(oddr.io.IN.valid) {
      report(Seq(s"${oddr.name} IN ", eclkArea.counter.value, " ", oddr.io.IN.payload.asBits))
    }

    assert(valid, "Mismatch between giddr and liddr")

    val counter = Counter(1 << 8)
    counter.increment()

    for (iddr <- Seq(giddr, liddr)) {
      iddr.io.ECLK := clockGen.io.eclk_270
      iddr.io.IN.valid := counter.value > 11
      iddr.io.IN.payload <> oddr.io.OUT.payload
    }

    oddr.io.IN.valid := counter.value > 10
    for (i <- 0 until oddr.input_per_output) {
      oddr.io.IN.payload(i) := (counter.value + primes(i)).resized
    }
    oddr.io.ECLK := eclk

  }
}

object LatticeODDRSim extends App {
    Config.spinal.generateVerilog(
      new Component {
        val GSR_INST = GSR.no_op()

        for(f <- Seq(2, 4, 8, 10)) {
          val dut = VerifyODDRTestBench(f, x => new LatticeODDR(x)).setDefinitionName(s"VerifyDDRTestBench_${f}").setName(s"verifyDDRTestBench_${f}")
        }
      }.setDefinitionName(s"LatticeODDRSim")
    )
}


object LatticeIDDRSim extends App {
  Config.spinal.generateVerilog(
    new Component {
      val GSR_INST = GSR.no_op()

      for(f <- Seq(2)) {
        val dut = VerifyIDDRTestBench(f, x => new LatticeIDDR(x)).setDefinitionName(s"VerifyIDDRTestBench_${f}").setName(s"verifyIDDRTestBench_${f}")
      }
    }.setDefinitionName(s"LatticeIDDRSim")
  )
}
