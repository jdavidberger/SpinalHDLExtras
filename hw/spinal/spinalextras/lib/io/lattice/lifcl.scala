package spinalextras.lib.io.lattice

import spinal.core._
import spinal.lib._
import spinal.lib.io.InOutWrapperPlayground.D
import spinalextras.lib.blackbox.lattice.lifcl.IO._
import spinalextras.lib.blackbox.lattice.lifcl._
import spinalextras.lib.io._
import spinalextras.lib.misc.ComponentWithKnownLatency

import java.security.InvalidParameterException
import scala.language.postfixOps

class LatticeTristateBuffer() extends TristateBuffer {
  val bb = new BB()

  // Connection to pad
  bb.B <> io.phy

  // Tri-state control
  bb.T := ~io.output_enable

  // Data from pad
  bb.O <> io.output

  // Data to pad
  bb.I <> io.input
}

case class LatticeDelayController(var static_delay : TimeNumber) extends Component with DelayController {
  val io = new Bundle {
    val delay = slave Stream(UInt(8 bits))

    val COARSE0, COARSE1, LOAD_N, DIRECTION, MOVE = out Bool()
    val CFLAG = in Bool()
  }

  require(static_delay >= (0 fs))

  if(static_delay >= (3.2 ns)) {
    SpinalWarning(s"Static delay for LIFCL delay blocks has a max of 3.2ns; capping given value of ${static_delay}")
    static_delay = static_delay.min((3.2 ns) - (12.5 ps))
  }

  val init_target = (static_delay / (12.5 ps)).rounded.toInt

  def create_delay_block(): DELAYA = {
    val delay_block = DELAYA(COARSE_DELAY_MODE="DYNAMIC",
      DEL_VALUE = (init_target % 128).toString(),
      COARSE_DELAY = if(init_target >= 128) "1P6NS" else "0NS"
    )

    delay_block.io.COARSE0 := io.COARSE0
    delay_block.io.COARSE1 := io.COARSE1
    delay_block.io.LOAD_N := io.LOAD_N
    delay_block.io.DIRECTION := io.DIRECTION
    delay_block.io.MOVE := io.MOVE

    delay_block
  }

  val target = RegNextWhen(io.delay.payload, io.delay.valid) init(init_target)
  val current_delay = CounterUpDown(128) init(init_target & 0x7f)
  val direction = RegNext(current_delay > target.resize(7 bits)) init(False)

  val needs_change = RegNext(current_delay =/= target.resize(7 bits))
  val bring_pulse_low = RegNext(io.MOVE) init(False)
  val move = needs_change && ~bring_pulse_low

  io.COARSE0 := False
  io.COARSE1 := target.msb
  io.LOAD_N := ~ClockDomain.current.readResetWire
  io.DIRECTION := direction
  io.MOVE := move


  io.delay.ready := False
  when(!needs_change && Delay(io.delay.valid, 2)) {
    io.delay.ready := True
  }

  when(io.CFLAG) {
    assert(current_delay.value === 0 || current_delay.value.andR === True)
  }

  when(move.rise(False)) {
    when(io.DIRECTION) {
      current_delay.decrement()
    } otherwise {
      current_delay.increment()
    }
  }

  override def delay: Stream[UInt] = io.delay
}

case class LatticeDelay(var static_delay : TimeNumber) extends Component {
  val io = new Bundle {
    val delay = slave Stream(UInt(8 bits))

    val IN = in Bool()
    val OUT = out Bool()
  }

  require(static_delay >= (0 fs))

  if(static_delay >= (3.2 ns)) {
    SpinalWarning(s"Static delay for LIFCL delay blocks has a max of 3.2ns; capping given value of ${static_delay}")
    static_delay = static_delay.min((3.2 ns) - (12.5 ps))
  }

  val init_target = (static_delay / (12.5 ps)).rounded.toInt

  var delay_block = DELAYA(COARSE_DELAY_MODE="DYNAMIC",
    DEL_VALUE = (init_target % 128).toString(),
    COARSE_DELAY = if(init_target >= 128) "1P6NS" else "0NS"
  )
  delay_block.io.A := io.IN
  io.OUT := delay_block.io.Z

  val target = RegNextWhen(io.delay.payload, io.delay.valid) init(init_target)
  val current_delay = CounterUpDown(128) init(init_target & 0x7f)

  delay_block.io.COARSE0 := False
  delay_block.io.COARSE1 := target.msb
  delay_block.io.LOAD_N := ~ClockDomain.current.readResetWire
  delay_block.io.DIRECTION := RegNext(current_delay > target.resize(7 bits)) init(False)

  val needs_change = RegNext(current_delay =/= target.resize(7 bits))
  val bring_pulse_low = RegNext(delay_block.io.MOVE) init(False)
  delay_block.io.MOVE := needs_change && ~bring_pulse_low

  io.delay.ready := False
  when(!needs_change && Delay(io.delay.valid, 2)) {
    io.delay.ready := True
  }

  when(delay_block.io.CFLAG) {
    assert(current_delay.value === 0 || current_delay.value.andR === True)
  }

  when(delay_block.io.MOVE.rise(False)) {
    when(delay_block.io.DIRECTION) {
      current_delay.decrement()
    } otherwise {
      current_delay.increment()
    }
  }
}

object LatticeODDR {
  def apply(reqs : DDRRequirements) : ODDR = {
    if(reqs.signal_multiple == 2) {
      new LatticeODDR(reqs)
    } else {
      new SynthODDR(reqs)
    }
  }
}
class LatticeODDR(reqs : DDRRequirements) extends ODDR(reqs) with ComponentWithKnownLatency {
  def gear = reqs.signal_multiple
  val QD = gear match {
    case 2 => {
      val oddr = ODDRX1()
      oddr.io.SCLK := io.ECLK
      oddr.io.RST := ClockDomain.current.readResetWire
      (oddr.io.Q, oddr.io.D) // 3
    }
    case 4 => {
      val oddr = ODDRX2()
      oddr.io.ECLK := io.ECLK
      (oddr.io.Q, oddr.io.D) // 7
    }
    case 7 => {
      val oddr = ODDR71()
      oddr.io.ECLK := io.ECLK
      (oddr.io.Q, oddr.io.D)
    }
    case 8 => {
      val oddr = ODDRX4()
      oddr.io.ECLK := io.ECLK
      (oddr.io.Q, oddr.io.D) // 11
    }
    case 10 => {
      val oddr = ODDRX5()
      oddr.io.ECLK := io.ECLK
      (oddr.io.Q, oddr.io.D) // 12
    }
    case _ => throw new InvalidParameterException()
  }

  (0 until gear).foreach(i => QD._2(i) := io.IN.payload(i))

  if(reqs.delayable || reqs.static_delay != (0 fs)) {
    //delay.io.IN := QD._1
    //io.OUT.payload := delay.io.OUT
  } else {
    io.OUT.payload := QD._1
  }

  override def create_delay_controller(): DelayController = {
    if(reqs.delayable || reqs.static_delay != (0 fs)) {
      LatticeDelayController(reqs.static_delay)
    } else {
      null
    }
  }
  override def attach_delay_controller(controller : DelayController): Unit = {
    if(controller != null) {
      withAutoPull()
      var restoreStack = Component.push(this)
      val delay_block = controller.create_delay_block()
      delay_block.IN := QD._1
      io.OUT.payload := delay_block.OUT
      restoreStack.restore()
    }
  }

  override def latency(): Int = {
    gear match {
      case 7 => 9
      case 8 => 10
      case 10 => 12
      case _ => log2Up(gear) * 4 - 1
    }
  }
  setDefinitionName(s"LatticeODDR_l${latency()}_${reqs.toString}")
}

object LatticeIDDR {
  def apply(reqs: DDRRequirements): IDDR = {
    if (reqs.signal_multiple == 2) {
      new LatticeIDDR(reqs)
    } else {
      new SynthIDDR(reqs)
    }
  }
}

class LatticeIDDR(reqs : DDRRequirements) extends IDDR(reqs) with ComponentWithKnownLatency {
  val gear = reqs.signal_multiple
  assert(gear >= 2)

  val QD = gear match {
    case 2 => {
      val ddr = IDDRX1()
      (ddr.io.Q, ddr.io.D)
    }
    case 4 => {
      val ddr = IDDRX2()
      ddr.io.ECLK := io.ECLK
      (ddr.io.Q, ddr.io.D)
    }
    case 7 => {
      val ddr = IDDR71()
      ddr.io.ECLK := io.ECLK
      (ddr.io.Q, ddr.io.D)
    }
    case 8 => {
      val ddr = IDDRX4()
      ddr.io.ECLK := io.ECLK
      (ddr.io.Q, ddr.io.D)
    }
    case 10 => {
      val ddr = IDDRX5()
      ddr.io.ECLK := io.ECLK
      (ddr.io.Q, ddr.io.D)
    }
    case _ => throw new InvalidParameterException()
  }
  (0 until gear).foreach(i => io.OUT.payload(i) := QD._1(i))

  if(reqs.delayable) {
//    val delay = LatticeDelay(reqs.static_delay)
//    delay.io.IN := io.IN.payload
//    QD._2 := delay.io.OUT
//    io.DELAY = Some(slave (delay.io.delay.clone()))
//    delay.io.delay <> io.DELAY.get
  } else {
    QD._2 := io.IN.payload
  }

  override def create_delay_controller(): DelayController = {
    if(reqs.delayable) LatticeDelayController(reqs.static_delay) else null
  }

  override def attach_delay_controller(controller : DelayController): Unit = {
    if(controller == null) {
      return
    }
    withAutoPull()
    val restoreStack = Component.push(this)
    val delay_block = controller.create_delay_block()
    delay_block.IN := io.IN.payload
    QD._2 := delay_block.OUT
    restoreStack.restore()
  }

  //setDefinitionName(s"LatticeIDDR_l${latency()}_${reqs.toString}")

  override def latency(): Int = {
    gear match {
      case 2 => 1
      case 4 => 5
      case 7 => 9
      case 8 => 10
      case 10 => 12
      case _ => 1
    }
  }
}
