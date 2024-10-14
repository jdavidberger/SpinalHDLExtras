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
  bb.B <> io.phy
  bb.T := ~io.output_enable
  bb.O <> io.output
  bb.I <> io.input
}

case class LatticeDelay() extends Component {
  val io = new Bundle {
    val delay = slave Stream(UInt(8 bits))

    val IN = in Bool()
    val OUT = out Bool()
  }

  var delay_block = DELAYA()
  delay_block.io.A := io.IN
  io.OUT := delay_block.io.Z

  val target = RegNextWhen(io.delay.payload, io.delay.valid) init(0)
  val current_delay = CounterUpDown(128)

  delay_block.io.COARSE0 := False
  delay_block.io.COARSE1 := target.msb
  delay_block.io.LOAD_N := ~ClockDomain.current.readResetWire
  delay_block.io.DIRECTION := RegNext(current_delay > target.resize(7 bits)) init(False)

  val needs_change = RegNext(current_delay =/= target.resize(7 bits))
  val bring_pulse_low = RegNext(delay_block.io.MOVE)
  delay_block.io.MOVE := needs_change && ~bring_pulse_low

  io.delay.ready := False
  when(!needs_change && io.delay.valid) {
    io.delay.ready := True
  }

  when(delay_block.io.MOVE.rise(False)) {
    when(delay_block.io.DIRECTION) {
      current_delay.decrement()
    } otherwise {
      current_delay.increment()
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

  if(reqs.delayable) {
    val delay = LatticeDelay()
    io.DELAY = Some(slave (delay.io.delay.clone()))
    delay.io.delay <> io.DELAY.get
    delay.io.IN := QD._1
    io.OUT.payload := delay.io.OUT
  } else {
    io.OUT.payload := QD._1
  }



  override def latency(): Int = {
    gear match {
      case 7 => 9
      case 8 => 10
      case 10 => 12
      case _ => log2Up(gear) * 4 - 1
    }
  }
  setDefinitionName(s"LatticeODDR_x${gear}_l${latency()}")
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
    val delay = LatticeDelay()
    delay.io.IN := io.IN.payload
    QD._2 := delay.io.OUT
    io.DELAY = Some(slave (delay.io.delay.clone()))
    delay.io.delay <> io.DELAY.get
  } else {
    QD._2 := io.IN.payload
  }

  setDefinitionName(s"LatticeIDDR_x${gear}_l${latency()}")

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
