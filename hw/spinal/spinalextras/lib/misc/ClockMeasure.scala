package spinalextras.lib.misc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class ClockMeasure(cntTil : BigInt = 1000000, cntBits : Int = 32) extends Component {
  val io = new Bundle {
    val clk_meas = in Bool()
    val output_cnt = master Flow(UInt(cntBits bits))

    val flush = in Bool() default(False)
    val min_output_cnt = out UInt(cntBits bits)
  }

  io.min_output_cnt.setAsReg() init( (1L << cntBits) - 1 )
  io.output_cnt.payload.setAsReg() init((1L << cntBits) - 1)
  
  when(io.flush) {
    io.min_output_cnt.setAll()
    io.output_cnt.payload.setAll()
  }

  val toggle = BufferCC(new ClockingArea(new ClockDomain(io.clk_meas, config = ClockDomainConfig(resetKind = BOOT))) {
    val timeout = new Timeout(cntTil)
    val b = RegInit(False)
    when(timeout) {
      timeout.clear()
      b := !b
    }
  }.b)

  val timer = CounterFreeRun(cntBits bits)
  io.output_cnt.valid := False
  when(toggle.edge()) {
    io.output_cnt.payload := timer.value
    io.output_cnt.valid := True
    timer.clear()
  }

  when(io.output_cnt.payload < io.min_output_cnt) {
    io.min_output_cnt := io.output_cnt.payload
  }
}

object ClockMeasure {
  def apply(clk: Bool, cntTil: BigInt = 1000000, flush: Bool = null): ClockMeasure = {
    val meas = new ClockMeasure(cntTil)
    meas.io.clk_meas := clk
    if(flush != null)
      meas.io.flush := flush
    meas
  }
}