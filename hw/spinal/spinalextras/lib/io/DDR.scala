package spinalextras.lib.io

import spinal.lib._
import spinal.core._
import spinal.core.sim._
import spinal.lib.io._
import spinalextras.lib.Config
import spinalextras.lib.impl.ImplementationSpecificFactory
import spinalextras.lib.io.lattice.{LatticeIDDR, LatticeODDR}
import spinalextras.lib.misc.{ComponentWithKnownLatency, DelayedSignal}

import scala.collection.mutable
import scala.language.postfixOps

abstract class IDDR(reqs : DDRRequirements) extends Component with ComponentWithKnownLatency {
  def output_per_input : Int = reqs.signal_multiple
  val io = new Bundle {
    val IN = slave(Flow(Bool()))
    val ECLK = in(Bool())
    val OUT = master(Flow(Bits(output_per_input bits)))

    var DELAY : Option[Stream[UInt]] = None
  }

  val validArea = new ClockingArea(ClockDomain(io.ECLK, reset = ClockDomain.current.readResetWire, config = ClockDomainConfig(clockEdge = RISING))) {
    val valid = Reg(Bits(latency() bits)) init (0) addTag(crossClockDomain)
    valid := ((valid << 1) | io.IN.valid.asBits.resized).resized
    io.OUT.valid := valid.msb
  }
}

abstract class ODDR(reqs : DDRRequirements) extends Component with ComponentWithKnownLatency {
  def input_per_output : Int = reqs.signal_multiple
  val io = new Bundle {
    //       D0 D1 Dx DN
    // ECLK |¯¯|__|¯¯|__|
    // CLK  |¯¯¯¯¯|_____|

    val IN = slave(Flow(Bits(input_per_output bits)))
    val ECLK = in(Bool())
    val OUT = master(Flow(Bool()))

    val BUSY = out(Bool())
    val LAST_SEND = out(Bool())

    var DELAY : Option[Stream[UInt]] = None
  }

  val validArea = new ClockingArea(ClockDomain(io.ECLK, reset = ClockDomain.current.readResetWire, config = ClockDomainConfig(clockEdge = RISING))) {
    val valid = DelayedSignal(latency(), crossClockDomain = true)
    valid.io.input := io.IN.valid
    io.OUT.valid := valid.io.output

    io.BUSY := valid.io.pipe =/= 0
    if(valid.io.pipe.high == 0) {
      io.LAST_SEND := ~io.IN.valid
    } else {
      io.LAST_SEND := valid.io.output && ~valid.io.pipe(valid.io.pipe.high - 1)
    }
  }

}

case class DDRRequirements(signal_multiple : Int = 2, delayable : Boolean = false) {

}

object ODDR {
  def factory = new ImplementationSpecificFactory[ODDR, DDRRequirements] {
    simulationHandler = {case _ => (reqs => new GenericODDR(reqs))}
    AddHandler { case Device("lattice", "lifcl", name, resetKind) => { reqs => new LatticeODDR(reqs)}}
    AddHandler { case _ => reqs => {
      println(s"Warning: Using simulation driver for ODDR since no device matches found.")
      new GenericODDR(reqs)
    }}

  }

  def apply(reqs : DDRRequirements) = factory(reqs)
}


object IDDR {
  def factory = new ImplementationSpecificFactory[IDDR, DDRRequirements] {
    simulationHandler = {case _ => (reqs => new GenericIDDR(reqs))}
    AddHandler { case Device("lattice", "lifcl", name, resetKind) => { reqs => new LatticeIDDR(reqs)}}
    AddHandler { case _ => reqs => {
      println(s"Warning: Using simulation driver for IDDR since no device matches found.")
      new GenericIDDR(reqs)
    }}
  }
  def apply(reqs : DDRRequirements) = factory(reqs)
}


case class ODDRArray[T <: BitVector](payloadType : HardType[T], reqs : DDRRequirements = DDRRequirements(), ODDRFactory : Option[(DDRRequirements) => ODDR] = None) extends ComponentWithKnownLatency {
  val input_per_output = reqs.signal_multiple

  val bitsWidth = payloadType.getBitsWidth
  ///setDefinitionName(s"ODDR_x${input_per_output}_w${bitsWidth}")
  val oddrs = Array.fill(bitsWidth)(ODDRFactory.getOrElse(x => ODDR(x))(reqs))
  val io = new Bundle {
    val IN= slave(Flow(Vec(payloadType, input_per_output)))
    val ECLK = in(Bool())
    val OUT = master(Flow(payloadType))
    val BUSY = out(Bool())
    val LAST_SEND = out(Bool())

    val DELAY = oddrs.head.io.DELAY.map(x => slave(x.clone()))
  }
  io.DELAY.map(_.allowOverride())
  io.DELAY.foreach(delay => oddrs.foreach(_.io.DELAY.get <> delay))

  noIoPrefix()
  for((oddr, bit_idx) <- oddrs.zipWithIndex) {
    oddr.io.ECLK := io.ECLK

    oddr.io.IN.valid := io.IN.valid
    oddr.io.IN.payload.assignDontCare()
    for (g <- 0 until input_per_output) {
      //when(io.IN.valid) {
        oddr.io.IN.payload(g) := io.IN.payload(g).asBits(bit_idx)
      //}
    }

    io.OUT.payload(bit_idx) := oddr.io.OUT.payload
  }

  io.LAST_SEND := oddrs.head.io.LAST_SEND
  io.BUSY := oddrs.head.io.BUSY
  io.OUT.valid := oddrs.head.io.OUT.valid

  override def latency(): Int = oddrs.head.latency()
}

case class IDDRArray[T <: BitVector](payloadType : HardType[T], reqs : DDRRequirements, IDDRFactory : Option[(DDRRequirements) => IDDR] = None) extends ComponentWithKnownLatency {
  val bitsWidth = payloadType.getBitsWidth
  val output_per_input = reqs.signal_multiple
  setDefinitionName(s"IDDR_x${output_per_input}_w${bitsWidth}")
  val iddrs = Array.fill(bitsWidth)(IDDRFactory.getOrElse(x => IDDR(x))(reqs))
  val io = new Bundle {
    val IN = slave(Flow(payloadType))
    val ECLK = in(Bool())
    val OUT = master(Flow(Vec(payloadType, output_per_input)))

    val DELAY = iddrs.head.io.DELAY.map(x => slave(x.clone()))
  }
  io.DELAY.map(_.allowOverride())
  io.DELAY.foreach(delay => iddrs.foreach(_.io.DELAY.get <> delay))

  noIoPrefix()

  for((iddr, bit_idx) <- iddrs.zipWithIndex) {
    iddr.io.ECLK := io.ECLK
    iddr.io.IN.payload := io.IN.payload(bit_idx)
    iddr.io.IN.valid := io.IN.valid
  }

  for(g <- 0 until output_per_input) {
    val v = Bits(bitsWidth bits)
    for((iddr, bit_idx) <- iddrs.zipWithIndex) {
      v(bit_idx) := iddr.io.OUT.payload(g)
    }
    io.OUT.payload(g).assignFromBits(v)
  }

  io.OUT.valid := iddrs.head.io.OUT.valid

  override def latency(): Int = iddrs.head.latency()
}


case class IODDRArray[T <: BitVector](payloadType : HardType[T],
                                      reqs: DDRRequirements) extends ComponentWithKnownLatency {
  val bitsWidth = payloadType.getBitsWidth
  val gear = reqs.signal_multiple
  setDefinitionName(s"IODDR_x${gear}_w${bitsWidth}")

  val data_in = IDDRArray(payloadType, reqs)
  val data_out = ODDRArray(payloadType, reqs)
  val tristates = TristateBufferArray(payloadType)

  val io = new Bundle {
    val ECLK = in(Bool())
    val OUT = slave(Flow(Vec(payloadType, gear)))
    val OUT_VALID = out(Bool())
    val IN_CAPTURE = in(Bool())
    val PHY = inout(Analog(Bits(bitsWidth bits)))
    val IN = master(Flow(Vec(payloadType, gear)))

    val BUSY = out(Bool())
    val LAST_SEND = out(Bool())

    val DELAY_IN = data_in.io.DELAY.map(x => slave(x.clone()))
    val DELAY_OUT = data_out.io.DELAY.map(x => slave(x.clone()))
  }

  data_in.io.DELAY.foreach(_ <> io.DELAY_IN.get)
  data_out.io.DELAY.foreach(_ <> io.DELAY_OUT.get)

  io.LAST_SEND := data_out.io.LAST_SEND
  io.BUSY := data_out.io.BUSY
  io.OUT_VALID := data_out.io.OUT.valid
  data_in.io.IN.valid := ~data_out.io.OUT.valid && io.IN_CAPTURE

  data_in.io.ECLK := io.ECLK
  data_out.io.ECLK := io.ECLK

  data_in.io.OUT <> io.IN
  data_out.io.IN <> io.OUT

  tristates.io.phy <> io.PHY
  tristates.io.input <> data_out.io.OUT.payload
  tristates.io.output <> data_in.io.IN.payload
  tristates.io.output_enable <> data_out.io.OUT.valid

  override val latency: Int = data_out.oddrs.head.latency()
}

object ODDRArraySim extends App {
  Config.spinal.generateVerilog(new Component {
    val ddr_factor = 4
    val eclk = ClockDomain.current.readClockWire

    val sclkDomain = if(ddr_factor > 2 && ddr_factor % 4 == 0) {
      val sync_clock = Counter(ddr_factor / 4)
      sync_clock.increment()
      val sclk = Reg(Bool()) init (False)
      when(sync_clock.willOverflow) {
        sclk := ~sclk
      }
      val reset = Counter(ddr_factor / 2 * 10)
      when(~reset.willOverflowIfInc) {
        reset.increment()
      }
      new ClockDomain(sclk, reset = ~reset.willOverflowIfInc)
    } else if(ddr_factor == 2) {
      ClockDomain.current
    } else {
      val sclk = in Bool()
      val reset = Counter(ddr_factor / 2 * 10)
      when(~reset.willOverflowIfInc) {
        reset.increment()
      }
      new ClockDomain(sclk, reset = ~reset.willOverflowIfInc)
    }

    val valid = out Bool()
    valid.addTag(crossClockDomain)
    assert(valid)

    val sclk = sclkDomain.readClockWire
    val sclkArea = new ClockingArea(sclkDomain ) {
      val loddr = new ODDRArray(UInt(4 bits), DDRRequirements(ddr_factor), ODDRFactory = Some(x => new LatticeODDR(x)))
      val goddr = new ODDRArray(UInt(4 bits), DDRRequirements(ddr_factor), ODDRFactory = Some(x => new GenericODDR(x, latency = loddr.latency())))

      valid.setAsReg() init(True)
      when(goddr.io.OUT.valid) {
        valid := goddr.io.OUT === loddr.io.OUT
      } otherwise {
        valid := goddr.io.OUT.valid === loddr.io.OUT.valid
      }

      val counter = Counter(1 << 4)
      counter.increment()
      counter.value.addTag(crossClockDomain)

      for (oddr <- Seq(goddr, loddr)) {
        oddr.io.IN.valid := counter.value > 10
        for (i <- 0 until goddr.input_per_output) {
          oddr.io.IN.payload(i) := (counter.value + 11 * i).resized
        }
        oddr.io.ECLK := eclk
      }
    }
  }.setDefinitionName("ODDRArraySim"))
}

object ODDRArray extends App {
  Config.sim.doSim(new ODDRArray(UInt(8 bits))) { dut =>
    dut.io.IN.valid #= false
    val clockDomain = ClockDomain(dut.io.ECLK)
    dut.clockDomain.forkStimulus(100 MHz)
    clockDomain.forkStimulus(100 * dut.input_per_output MHz)
    dut.clockDomain.waitSampling(20)

    for(i <- 0 until 100) {
      dut.io.IN.payload(0) #= simRandom.nextInt(255)
      dut.io.IN.payload(1) #= simRandom.nextInt(255)
      dut.io.IN.valid #= true
      dut.clockDomain.waitSampling()
    }
  }
}
