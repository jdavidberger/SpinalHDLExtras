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

class LatticeODDR(gear : Int = 2) extends ODDR(gear) with ComponentWithKnownLatency {
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

  io.OUT.payload := QD._1

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

class LatticeIDDR(gear : Int = 2) extends IDDR(gear) with ComponentWithKnownLatency {
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
  QD._2 := io.IN.payload

  setDefinitionName(s"LatticeIDDR_x${gear}_l${latency()}")
  override def latency(): Int = 1
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
