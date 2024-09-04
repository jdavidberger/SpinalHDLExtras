package spinalextras.lib.io

import spinal.core.sim._
import spinal.core._
import spinal.lib._
import spinalextras.lib.Config

/***
 * Generic ODDR implementation. This is mostly useful for verification of HW ODDRs and simulations. Might work on real
 * hardware but probably isn't the best thing.
 *
 * Ported from https://zipcpu.com/blog/2020/08/22/oddr.html
 * @param input_per_output The number of inputs per output pin (gearing)
 * @param latency The latency in edge clk cycles from valid input to valid output. Useful to mimic real ODDR behavior
 *                and verify that designs using ODDR aren't dependent on a given latency
 */
class GenericODDR(input_per_output : Int = 2, latency : Int = 1) extends ODDR(input_per_output) {
  setDefinitionName(s"GenericODDR_x${input_per_output}_l${latency}")
  val pclock = ClockDomain(clock = io.ECLK, reset = ClockDomain.current.reset, config = ClockDomainConfig(clockEdge = RISING))
  val nclock = ClockDomain(clock = io.ECLK, reset = ClockDomain.current.reset, config = ClockDomainConfig(clockEdge = FALLING))

  val pArea = new ClockingArea(pclock) {
    var IN = io.IN
    for(i <- 0 until latency - 1) {
      IN = IN.m2sPipe(crossClockData = true).setName(s"IN_${i}")
    }
    IN.setName("IN")

    val g = if(input_per_output > 2) {
      val gc = Counter((input_per_output + 1) / 2) init(0)
      gc.setName("gc")

      when(IN.valid) {
        gc.increment()
      } otherwise {
        gc.clear()
      }

      (gc.value << 1)
    } else {
      U(0, 1 bits)
    }

    val (idx0, idx1) = if(input_per_output % 2 == 0) {
      (g.resized, (g + 1).resized)
    } else {
      (Mux(g >= input_per_output, g - input_per_output, g).resized,
        Mux((g + 1) >= input_per_output, g + 1 - input_per_output, g + 1).resized)
    }

    val cp, cnp = Reg(Bool()) init(False) addTag(crossClockDomain)
    when(IN.valid) {
      assert(idx0.expand < IN.payload.getWidth)
      assert(idx1.expand < IN.payload.getWidth)
      cp := IN.payload(idx0) ^ cnp
      cnp := IN.payload(idx1) ^ IN.payload(idx0) ^ cnp
    } otherwise {
      cp.assignDontCare()
    }
  }

  val nArea = new ClockingArea(nclock) {
    val cn = Reg(Bool()).addTag(crossClockDomain)
    cn := pArea.cnp
  }

  io.OUT.payload := pArea.cp ^ nArea.cn

  override def latency() = latency
}

/***
 * Generic IDDR implementation. This is mostly useful for verification of HW IDDRs and simulations. Might work on real
 * hardware but probably isn't the best thing.
 *
 * @param output_per_input The number of inputs per output pin (gearing)
 * @param latency The latency in edge clk cycles from valid input to valid output
 */
class GenericIDDR(output_per_input : Int = 2, latency : Int = 1) extends IDDR(output_per_input) {
  val pclock = ClockDomain(clock = io.ECLK, reset = ClockDomain.current.reset, config = ClockDomainConfig(clockEdge = RISING))
  val nclock = ClockDomain(clock = io.ECLK, reset = ClockDomain.current.reset, config = ClockDomainConfig(clockEdge = FALLING))

  var OUT = io.OUT.payload.clone()

  def regX(p : Bool): Bool = {
    var rtn = p;
    for(i <- 0 until latency - 1) {
      rtn = RegNext(rtn)
    }
    rtn.addTag(crossClockDomain)
  }
  val pArea = new ClockingArea(pclock) {
    val c = regX(io.IN.payload)
    val r = Reg(Vec(Bool(), output_per_input/2))

    val g = if(output_per_input > 2) {
      val gc = Counter((output_per_input + 1) / 2) init(0)
      gc.setName("gc")

      when(io.IN.valid) {
        gc.increment()
      } otherwise {
        gc.clear()
      }

      gc.valueNext
    } else {
      U(0, 0 bits)
    }

    r(g) := c
  }

  val nArea = new ClockingArea(nclock) {
    val c = regX(RegNext(io.IN.payload))
    val r = Reg(Vec(Bool(), output_per_input/2))

    r(pArea.g) := c
  }

  OUT := Vec(nArea.r.zip(pArea.r).flatMap(x => Seq(x._1, x._2))).asBits
  io.OUT.payload <> RegNext(OUT)

  override def latency(): Int = latency
}


/***
 * Generic ODDR implementation. This is mostly useful for verification of HW ODDRs and simulations. Might work on real
 * hardware but probably isn't the best thing.
 *
 * Ported from https://zipcpu.com/blog/2020/08/22/oddr.html
 * @param input_per_output The number of inputs per output pin (gearing)
 * @param latency The latency in edge clk cycles from valid input to valid output. Useful to mimic real ODDR behavior
 *                and verify that designs using ODDR aren't dependent on a given latency
 */
class GenericIDDR1(output_per_input : Int = 2, latency : Int = 1) extends ODDR(output_per_input) {
  setDefinitionName(s"GenericODDR_x${input_per_output}_l${latency}")
  val pclock = ClockDomain(clock = io.ECLK, reset = ClockDomain.current.reset, config = ClockDomainConfig(clockEdge = RISING))
  val nclock = ClockDomain(clock = io.ECLK, reset = ClockDomain.current.reset, config = ClockDomainConfig(clockEdge = FALLING))

  val pArea = new ClockingArea(pclock) {
    var IN = io.IN
    for(i <- 0 until latency - 1) {
      IN = IN.m2sPipe(crossClockData = true).setName(s"IN_${i}")
    }
    IN.setName("IN")

    val g = if(input_per_output > 2) {
      val gc = Counter((input_per_output + 1) / 2) init(0)
      gc.setName("gc")

      when(IN.valid) {
        gc.increment()
      } otherwise {
        gc.clear()
      }

      (gc.value << 1)
    } else {
      U(0, 1 bits)
    }

    val (idx0, idx1) = if(input_per_output % 2 == 0) {
      (g.resized, (g + 1).resized)
    } else {
      (Mux(g >= input_per_output, g - input_per_output, g).resized,
        Mux((g + 1) >= input_per_output, g + 1 - input_per_output, g + 1).resized)
    }

    val cp, cnp = Reg(Bool()) init(False) addTag(crossClockDomain)
    when(IN.valid) {
      assert(idx0.expand < IN.payload.getWidth)
      assert(idx1.expand < IN.payload.getWidth)
      cp := IN.payload(idx0) ^ cnp
      cnp := IN.payload(idx1) ^ IN.payload(idx0) ^ cnp
    } otherwise {
      cp.assignDontCare()
    }
  }

  val nArea = new ClockingArea(nclock) {
    val cn = Reg(Bool()).addTag(crossClockDomain)
    cn := pArea.cnp
  }

  io.OUT.payload := pArea.cp ^ nArea.cn

  override def latency() = latency
}

object GenericIODDR extends App {
  Config.sim.doSim(new Component {
    val oddr = new GenericODDR()
    val iddr = new GenericIDDR()
    val input_per_output = oddr.input_per_output

    oddr.io.OUT <> iddr.io.IN

    val io = new Bundle {
      val IN = slave Flow(Bits(oddr.input_per_output bits))
      val OUT = master Flow(Bits(oddr.input_per_output bits))
      val ECLK = in Bool()
      val ECLK_90 = in Bool()
    }
    oddr.io.ECLK <> io.ECLK
    iddr.io.ECLK <> io.ECLK_90

    io.IN <> oddr.io.IN
    io.OUT <> iddr.io.OUT
  }.setDefinitionName("GenericIODDR")) { dut =>
    dut.io.IN.valid #= false
    val clockDomain = ClockDomain(dut.io.ECLK)
    dut.clockDomain.forkStimulus(100 MHz)
    clockDomain.forkStimulus(100 * dut.input_per_output MHz)

    sleep((2.5) ns)
    ClockDomain(dut.io.ECLK_90).forkStimulus(100 * dut.input_per_output MHz)

    dut.clockDomain.waitSampling(20)

    for(i <- 0 until 100) {
      dut.io.IN.payload #= simRandom.nextInt(1 << (dut.input_per_output))
      dut.io.IN.valid #= true
      dut.clockDomain.waitSampling()
    }
  }
}
