package spinalextras.lib.misc

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.internals.Expression
import spinal.core.sim.{SimBaseTypePimper, SimBoolPimper, SimClockDomainHandlePimper, SimPublic, SimTimeout}
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config, Axi4Shared}
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.wishbone._
import spinal.lib.misc.Timer
import spinal.lib.sim.StreamMonitor
import spinalextras.lib.Config
import spinalextras.lib.tests.WishboneGlobalBus.GlobalBus_t

import scala.collection.mutable
import scala.language.postfixOps
import scala.reflect.ClassTag



case class WishboneTx(config : WishboneConfig) extends Bundle {
  val ADR       = UInt(config.addressWidth bits)
  val WE        = Bool()
  val DAT  = Bits(config.dataWidth bits)
  val SEL       = if(config.useSEL)   Bits(config.selWidth bits) else null
}
object WishboneBusLogger {
  def flows(wishbone: Wishbone*): Seq[(Data, Flow[Bits])] = {
    wishbone.map(wb => {
      val wbLog = WishboneTx(wb.config)
      wbLog.ADR := wb.ADR
      wbLog.WE := wb.WE
      wbLog.DAT := Mux(wb.WE, wb.DAT_MOSI, wb.DAT_MISO)
      wbLog.SEL := wb.SEL

      val stream = Flow(Bits(wbLog.getBitsWidth bits)).setName(wb.name)
      stream.payload := wbLog.asBits
      stream.valid := wb.isResponse
      (wbLog.setName(wb.name), stream)
    })
  }
}

object SignalLogger {
  def flows(signals: Data*): Seq[(Data, Flow[Bits])] = {
    val signalWidth = signals.map(_.getBitsWidth).max
    signals.map(signal => {
      val stream = Flow(Bits(signal.getBitsWidth bits))
      val lastValue = RegNext(signal)
      stream.payload := signal.asBits
      stream.valid := signal =/= lastValue
      (signal, stream.setName(signal.name))
    })
  }

}

class SignalLoggerTest extends AnyFunSuite {
  test("SignalLoggerTest") {
    Config.sim.doSim(
      new Component {
        val gpio = Bool()
        val logger = FlowLogger(SignalLogger.flows(gpio))
        val fifo = StreamFifo(cloneOf(logger.io.log.payload), 4)
        val io = new Bundle {
          val gpio = in(Bool())
          val log = master(cloneOf(logger.io.log))
          val flush = in(Bool())
        }
        io.flush <> fifo.io.flush
        io.gpio <> gpio

        logger.io.log <> fifo.io.push
        io.log <> fifo.io.pop

      }.setDefinitionName("SignalLoggerTest")
    ) { dut =>
      dut.clockDomain.forkStimulus(100 MHz)
      dut.clockDomain.waitSampling()
      SimTimeout(100 us)

      dut.io.log.ready #= false
      dut.io.flush #= false

      for(i <- 0 until 200) {
        dut.io.gpio #= (i % 2) == 0
        dut.clockDomain.waitSampling()
      }

      dut.io.log.ready #= true
      StreamMonitor(dut.io.log, dut.clockDomain) { payload =>
        println(f">> ${payload.toBigInt >> 2} ${payload.toBigInt & 2}")
      }

      for(i <- 0 until 20) {
        dut.io.gpio #= (i % 2) == 0
        dut.clockDomain.waitSampling()
      }

      for(i <- 0 until 20) {
        dut.clockDomain.waitSampling()
      }

      println(s"Dropped  ${dut.logger.io.dropped_events.toBigInt}")
      println(s"Captured ${dut.logger.io.captured_events.toBigInt}")
    }

  }
}