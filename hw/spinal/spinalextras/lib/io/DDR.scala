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

trait DelayBlock {
  def IN : Bool
  def OUT : Bool
}

trait DelayController {
  def delay : Stream[UInt]
  def create_delay_block() : DelayBlock
}

class IDDRMeta(latency : Int) extends Component {
  val io = new Bundle {
    val IN_valid = in((Bool()))
    val ECLK = in(Bool())
    val OUT_valid = out(Bool())
  }

  val validArea = new ClockingArea(ClockDomain(io.ECLK, reset = ClockDomain.current.isResetActive, config = ClockDomainConfig(clockEdge = RISING))) {
    val valid = Reg(Bits(latency bits)) init (0) addTag(crossClockDomain)
    valid := ((valid << 1) | io.IN_valid.asBits.resized).resized
    io.OUT_valid := valid.msb
  }

}

abstract class IDDR(reqs : DDRRequirements) extends Component with ComponentWithKnownLatency {
  def output_per_input : Int = reqs.signal_multiple
  val io = new Bundle {
    val IN = slave(Flow(Bool()))
    val ECLK = in(Bool())
    val OUT = master(Flow(Bits(output_per_input bits)))

    //var DELAY : Option[Stream[UInt]] = None
  }

  val validArea = new ClockingArea(ClockDomain(io.ECLK, reset = ClockDomain.current.isResetActive, config = ClockDomainConfig(clockEdge = RISING))) {
    val valid = Reg(Bits(latency() bits)) init (0) addTag(crossClockDomain)
    valid := ((valid << 1) | io.IN.valid.asBits.resized).resized
    io.OUT.valid := valid.msb
  }

  def create_delay_controller(): DelayController = null
  def attach_delay_controller(controller : DelayController): Unit = { }
}

class ODDRMeta(latency : Int) extends Component {
  val io = new Bundle {
    val ECLK = in(Bool())

    val IN_valid = in(Bool())
    val OUT_valid = out(Bool())

    val BUSY = out(Bool())
    val LAST_SEND = out(Bool())
  }

  val validArea = new ClockingArea(ClockDomain(io.ECLK, reset = ClockDomain.current.isResetActive, config = ClockDomainConfig(clockEdge = RISING))) {
    val valid = DelayedSignal(latency, crossClockDomain = true)
    valid.io.input := io.IN_valid
    io.OUT_valid := valid.io.output

    io.BUSY := valid.io.pipe =/= 0
    if(valid.io.pipe.high == 0) {
      io.LAST_SEND := ~io.IN_valid
    } else {
      io.LAST_SEND := valid.io.output && ~valid.io.pipe(valid.io.pipe.high - 1)
    }
  }
}

abstract class ODDR(reqs : DDRRequirements) extends Component with ComponentWithKnownLatency {
  require(reqs.signal_multiple > 1)

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

    //var DELAY : Option[Stream[UInt]] = None
  }

  def create_delay_controller(): DelayController = null
  def attach_delay_controller(controller : DelayController): Unit = {}

  val meta = new ODDRMeta(latency())
  meta.io.ECLK <> io.ECLK
  meta.io.IN_valid <> io.IN.valid
  meta.io.OUT_valid <> io.OUT.valid
  meta.io.BUSY <> io.BUSY
  meta.io.LAST_SEND <> io.LAST_SEND
}

case class DDRRequirements(signal_multiple : Int = 2,
                           delayable : Boolean = false,
                           static_delay : TimeNumber = 0 fs,
                           mock : Boolean = false
                          ) {
  override def toString = {

    s"x${signal_multiple}${if(delayable) "_delayable" else ""}" +
    (if(static_delay > (0 fs)) s"_delay${static_delay.decomposeString.replace(".", "p").replace(" ", "")}" else "")
  }
}


class SynthODDR(reqs : DDRRequirements) extends ODDR(reqs) with ComponentWithKnownLatency {
  require(input_per_output > 2 && input_per_output % 2 == 0)

  lazy val cnt = CounterFreeRun(input_per_output / 2)

  lazy val reqs_x2 = reqs.copy(signal_multiple = 2)
  lazy val edge_clock_area = new ClockingArea(new ClockDomain(io.ECLK, config = ClockDomain.current.config.copy(resetKind = BOOT))) {
    val oddr = ODDR(reqs_x2)
    val input = BufferCC(io.IN)
    oddr.io.ECLK := io.ECLK
    val edge_cnt = BufferCC(cnt.value)
    oddr.io.IN.valid := input.valid
    oddr.io.IN.payload.assignDontCare()
    when(input.valid) {
      oddr.io.IN.payload := (input.payload >> (edge_cnt * 2)).resized
    }
    io.OUT.payload <> oddr.io.OUT.payload
  }

  override def create_delay_controller(): DelayController = edge_clock_area.oddr.create_delay_controller()
  override def attach_delay_controller(controller : DelayController): Unit = edge_clock_area.oddr.attach_delay_controller(controller)

  override def latency(): Int = edge_clock_area.oddr.latency() + 2
}

class SynthIDDR(reqs : DDRRequirements) extends IDDR(reqs) with ComponentWithKnownLatency {
  require(output_per_input > 2 && output_per_input % 2 == 0)

  lazy val cnt = CounterFreeRun(output_per_input / 2)

  lazy val reqs_x2 = reqs.copy(signal_multiple = 2)
  lazy val edge_clock_area = new ClockingArea(new ClockDomain(io.ECLK, reset = ClockDomain.current.reset)) {
    val iddr = IDDR(reqs_x2)

    iddr.io.ECLK := io.ECLK
    val edge_cnt = BufferCC(cnt.value)
    val payload = Reg(io.OUT.payload.clone())

    when(iddr.io.IN.valid) {
      payload((edge_cnt * 2), 2 bits) := iddr.io.OUT.payload
    }

    iddr.io.IN.valid := io.IN.valid
    io.IN.payload <> iddr.io.IN.payload
  }

  io.OUT.payload := BufferCC(edge_clock_area.payload)

  override def create_delay_controller(): DelayController = edge_clock_area.iddr.create_delay_controller()
  override def attach_delay_controller(controller : DelayController): Unit = edge_clock_area.iddr.attach_delay_controller(controller)

  override def latency(): Int = edge_clock_area.iddr.latency() + 2
}

object ODDR {
  def factory = new ImplementationSpecificFactory[ODDR, DDRRequirements] {
    simulationHandler = {case _ => (reqs => new GenericODDR(reqs))}
    formalHandler = {case _ => (reqs => new MockODDR(reqs))}
    AddHandler { case Device("lattice", "lifcl", name, resetKind) => { reqs => LatticeODDR(reqs)}}
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
    formalHandler = {case _ => (reqs => new MockIDDR(reqs))}
    AddHandler { case Device("lattice", "lifcl", name, resetKind) => { reqs => LatticeIDDR(reqs)}}
    AddHandler { case _ => reqs => {
      println(s"Warning: Using simulation driver for IDDR since no device matches found.")
      new GenericIDDR(reqs)
    }}
  }
  def apply(reqs : DDRRequirements) = factory(reqs)
}


case class ODDRS[T <: BitVector](payloadType : HardType[T], reqs : DDRRequirements = DDRRequirements(), ODDRFactory : Option[(DDRRequirements) => ODDR] = None) extends ComponentWithKnownLatency {
  val input_per_output = reqs.signal_multiple

  val bitsWidth = payloadType.getBitsWidth
  ///setDefinitionName(s"ODDR_x${input_per_output}_w${bitsWidth}")
  val oddrs = Array.fill(bitsWidth)(ODDRFactory.getOrElse(x => ODDR(x))(reqs))

  val delay_controller = oddrs.head.create_delay_controller()
  oddrs.foreach(_.attach_delay_controller(delay_controller))

  val io = new Bundle {
    val IN= slave(Flow(Vec(payloadType, input_per_output)))

    val ECLK = in(Bool())
    val OUT = master(Flow(payloadType))

    val BUSY = out(Bool())
    val LAST_SEND = out(Bool())

    val DELAY = if(delay_controller != null) Some(slave(delay_controller.delay.clone())) else None
  }
  io.DELAY.foreach(_ <> delay_controller.delay)

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

case class IDDRS[T <: BitVector](payloadType : HardType[T], reqs : DDRRequirements, IDDRFactory : Option[(DDRRequirements) => IDDR] = None) extends ComponentWithKnownLatency {
  val bitsWidth = payloadType.getBitsWidth
  val output_per_input = reqs.signal_multiple
  setDefinitionName(s"IDDR_x${output_per_input}_w${bitsWidth}", false)
  val iddrs = Array.fill(bitsWidth)(IDDRFactory.getOrElse(x => IDDR(x))(reqs))

  val delay_controller = iddrs.head.create_delay_controller()
  iddrs.foreach(_.attach_delay_controller(delay_controller))

  val io = new Bundle {
    val IN = slave(Flow(payloadType))
    val ECLK = in(Bool())
    val OUT = master(Flow(Vec(payloadType, output_per_input)))

    val DELAY = if(delay_controller != null) Some(slave(delay_controller.delay.clone())) else None
  }
  io.DELAY.foreach(_ <> delay_controller.delay)

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

case class IODDRS_IO[T <: BitVector](payloadType : HardType[T],
                                     gear : Int,
                                     mock : Boolean = false) extends Bundle with IMasterSlave {
  val bitsWidth = payloadType.getBitsWidth

  val OUTPUT_SUPPRESS = (Bits(bitsWidth bits)) default(0)

  val IN_CAPTURE = (Bool())

  // data_out.io.IN
  val OUT = (Flow(Vec(payloadType, gear)))

  // data_out.io.OUT.valid
  //val OUT_VALID = (Bool())

  // data_in.io.OUT
  val IN_payload = (Vec(payloadType, gear))

  def IN = ({
    val rtn = Flow(Vec(payloadType, gear))
    rtn.payload := IN_payload

    val metaIn = new IDDRMeta(4)
    metaIn.io.ECLK <> ClockDomain.readClockWire
    metaIn.io.IN_valid := IN_CAPTURE
    rtn.valid := metaIn.io.OUT_valid

    rtn
  })
  // data_out.io.BUSY
  //val BUSY = (Bool())

  // data_out.io.LAST_SEND
  //val LAST_SEND = (Bool())

  override def asMaster(): Unit = {
    out(OUTPUT_SUPPRESS, IN_CAPTURE)
    //in(OUT_VALID, BUSY, LAST_SEND)
    in(IN_payload)
    master(OUT)
  }

  def meta_signals(latency : Int) = new Area {
    val meta = new ODDRMeta(4)
    meta.io.ECLK := ClockDomain.readClockWire
    meta.io.IN_valid := OUT.valid
    val OUT_VALID = meta.io.OUT_valid

    val BUSY = meta.io.BUSY
    val LAST_SEND = meta.io.LAST_SEND
    //
  }
}

case class IODDRS[T <: BitVector](payloadType : HardType[T],
                                  in_reqs: DDRRequirements,
                                  out_reqs: DDRRequirements) extends ComponentWithKnownLatency {
  require(in_reqs.mock == out_reqs.mock)

  val bitsWidth = payloadType.getBitsWidth
  val gear = in_reqs.signal_multiple
  require(gear == out_reqs.signal_multiple)

  setDefinitionName(s"IODDR_x${gear}_w${bitsWidth}", false)

  val data_in = IDDRS(payloadType, in_reqs)
  val data_out = ODDRS(payloadType, out_reqs)
  val tristates = TristateBuffers(payloadType)

  val io = new Bundle {
    val ioddr = slave(new IODDRS_IO(payloadType, gear,
      mock = in_reqs.mock
    ))
    val ECLK = in(Bool())
    val PHY = inout(Analog(Bits(bitsWidth bits)))

    val DELAY_IN = data_in.io.DELAY.map(x => slave(x.clone()))
    val DELAY_OUT = data_out.io.DELAY.map(x => slave(x.clone()))
  }

  data_in.io.DELAY.foreach(_ <> io.DELAY_IN.get)
  data_out.io.DELAY.foreach(_ <> io.DELAY_OUT.get)

//  io.ioddr.LAST_SEND := data_out.io.LAST_SEND
//  io.ioddr.BUSY := data_out.io.BUSY
//  io.ioddr.OUT_VALID := data_out.io.OUT.valid
  data_in.io.IN.valid := ~data_out.io.OUT.valid && io.ioddr.IN_CAPTURE

  data_in.io.ECLK := io.ECLK
  data_out.io.ECLK := io.ECLK

  io.ioddr.IN_payload := data_in.io.OUT.payload
  data_out.io.IN <> io.ioddr.OUT

  tristates.io.phy <> io.PHY
  tristates.io.input <> data_out.io.OUT.payload
  tristates.io.output <> data_in.io.IN.payload
  tristates.io.output_enable <> data_out.io.OUT.valid
  tristates.io.output_enable_suppress <> io.ioddr.OUTPUT_SUPPRESS

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
      val loddr = new ODDRS(UInt(4 bits), DDRRequirements(ddr_factor), ODDRFactory = Some(x => new LatticeODDR(x)))
      val goddr = new ODDRS(UInt(4 bits), DDRRequirements(ddr_factor), ODDRFactory = Some(x => new GenericODDR(x, latency = loddr.latency())))

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

object ODDRS extends App {
  Config.sim.doSim(new ODDRS(UInt(8 bits))) { dut =>
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
