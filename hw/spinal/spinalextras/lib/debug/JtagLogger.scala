package spinalextras.lib.debug

import spinal.core._
import spinal.lib._
import spinal.lib.com.jtag.{Jtag, JtagTap, JtagTapInstructionCtrl}

import scala.language.postfixOps

object JtagLoggerTap {
  def apply(log_stream : Stream[Bits], tap: JtagTap, jtag_cd : ClockDomain, instr : Int = 0x24) = {
    val log_stream_cc = log_stream.queue(4, ClockDomain.current, popClock = jtag_cd)

    val ctrl = new ClockingArea(jtag_cd) {
      tap.map(StreamJtagInstrCtrl(log_stream_cc), instr)
    }
  }
}
class JtagLoggerTap(bitWidth : Int, instr : Int = 0x24) extends Component {
  val io = new Bundle {
    val jtag    = slave(Jtag())
    val log_stream = slave(Stream(Bits(bitWidth bits)))
  }

  val jtag_cd = ClockDomain(io.jtag.tck, reset = ClockDomain.current.reset,
    config = ClockDomain.current.config.copy(resetKind = ASYNC))

  val ctrl = new ClockingArea(jtag_cd) {
    val tap = new JtagTap(io.jtag, 8)
    tap.idcode(B"h010003d1")(instructionId = 0xe0)
  }
  noIoPrefix()

  JtagLoggerTap(io.log_stream, ctrl.tap, jtag_cd, instr)
}

class JtagChain(device_count : Int) extends Component {
  val io = new Bundle {
    val jtag = slave(Jtag())
    val jtags = Array.fill(device_count)(master(Jtag()))
  }
  noIoPrefix()

  var td = io.jtag.tdi
  io.jtags.foreach(jtag_tap => {
    jtag_tap.tck := io.jtag.tck
    jtag_tap.tms := io.jtag.tms
    jtag_tap.tdi := td
    td = jtag_tap.tdo
  })
  io.jtag.tdo := td
}

object JtagChain {
  def apply(jtag_devs : Jtag*): JtagChain = {
    val jtag_devs_nonnull = jtag_devs.filter(_ != null)
    val c = new JtagChain(jtag_devs_nonnull.size)

    for (elem <- jtag_devs_nonnull.zip(c.io.jtags)) {
      elem._2 <> elem._1
    }

    c
  }
}