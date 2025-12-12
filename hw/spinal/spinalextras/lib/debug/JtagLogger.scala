package spinalextras.lib.debug

import spinal.core._
import spinal.lib._
import spinal.lib.com.jtag.{Jtag, JtagTap, JtagTapInstructionCtrl}
import spinalextras.lib.Constraints
import spinalextras.lib.clocking.ClockUtils

import scala.language.postfixOps

object JtagLoggerTap {
  def apply(outStream : Stream[Bits],
            inFlow: Flow[Bits],
            tap: JtagTap, jtag_cd : ClockDomain, instr : Int = 0x24) = {
    val log_stream_cc = outStream.ccToggle(ClockDomain.current, popClock = jtag_cd)

    val ctrl = new ClockingArea(jtag_cd) {
      val inFlow_cc = inFlow.clone()
      tap.map(StreamJtagInstrCtrl(log_stream_cc, inFlow_cc), instr)
    }

    inFlow <> ctrl.inFlow_cc.ccToggle(jtag_cd, ClockDomain.current)
  }
}
class JtagLoggerTap(bitWidth : Int, instr : Int = 0x24, frequency : HertzNumber = 12 MHz) extends Component {
  val io = new Bundle {
    val jtag    = slave(Jtag())
    val outStream = slave(Stream(Bits(bitWidth bits)))
    val inFlow = master(Flow(Bits(bitWidth bits)))
  }

  ClockUtils.asAsyncReset(ClockDomain.current) on new Area {
    val jtag_cd = ClockUtils.createAsyncClock(io.jtag.tck, FixedFrequency(12 MHz))

    val ctrl = new ClockingArea(jtag_cd) {
      val tap = new JtagTap(io.jtag, 8)
      tap.idcode(B"h010003d1")(instructionId = 0xe0)
    }
    noIoPrefix()

    JtagLoggerTap(io.outStream, io.inFlow, ctrl.tap, jtag_cd, instr)
  }
}

class JtagChain(device_count : Int) extends Component {
  val io = new Bundle {
    val jtag = slave(Jtag())
    val jtags = Array.fill(device_count)(master(Jtag()))
  }
  noIoPrefix()
  Constraints.create_clock(io.jtag.tck, 12 MHz)
  Constraints.set_max_skew(10 ns, io.jtag.tck, io.jtag.tdi, io.jtag.tdo, io.jtag.tms)

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
    if(jtag_devs_nonnull.isEmpty)
      null
    else {
      val c = new JtagChain(jtag_devs_nonnull.size)

      for (elem <- jtag_devs_nonnull.zip(c.io.jtags)) {
        elem._2 <> elem._1
      }

      c
    }
  }
}