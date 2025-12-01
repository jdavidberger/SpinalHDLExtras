package spinalextras.lib.logging

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim.{SimBaseTypePimper, SimBoolPimper, SimClockDomainHandlePimper, SimTimeout}
import spinal.idslplugin.Location
import spinal.lib._
import spinal.lib.bus.misc.{AddressMapping, AllMapping}
import spinal.lib.bus.wishbone._
import spinal.lib.sim.StreamMonitor
import spinalextras.lib.Config
import spinalextras.lib.bus.WishboneExt
import spinalextras.lib.misc.RateLimitFlow

import scala.language.postfixOps



case class WishboneTx(config : WishboneConfig) extends Bundle {
  val CYC       = Bool()
  val STB       = Bool()
  val ACK       = Bool()
  val STALL     = if(config.useSTALL) Bool()                     else null

  val ADR       = UInt(config.addressWidth bits)
  val WE        = Bool()
  val DAT  = Bits(config.dataWidth bits)
  val SEL       = if(config.useSEL)   Bits(config.selWidth bits) else null

  override def toString(): String = {
    if(name == null)
      "anon_wb_tx"
    else
      super.toString()
  }
}

object WishboneBusLogger {
  def flows(addressMapping: AddressMapping, address_width : Int, wishbone: Wishbone*): Seq[(Data, Flow[Bits])] = {
    wishbone.map(wb => {
      val wbLog = WishboneTx(wb.config.copy(addressWidth = Math.min(address_width, wb.config.addressWidth)))
      wbLog.CYC := wb.CYC
      wbLog.STB := wb.STB
      wbLog.ACK := wb.ACK
      if(wbLog.STALL != null)
        wbLog.STALL := wb.STALL
      wbLog.ADR := wb.ADR.resized
      wbLog.WE := wb.WE
      wbLog.DAT := Mux(wb.WE, wb.DAT_MOSI, wb.DAT_MISO)
      if(wbLog.SEL != null)
        wbLog.SEL := wb.SEL

      val stream = Flow(Bits(wbLog.getBitsWidth bits)).setName(wb.name)
      stream.payload := wbLog.asBits
      stream.valid := wb.isResponse && addressMapping.hit(wb.ADR)
      (wbLog.setName(wb.name), stream)
    })
  }
  def flows(addressMapping: AddressMapping, wbs: Wishbone*): Seq[(Data, Flow[Bits])] = {
    flows(addressMapping, Int.MaxValue, wbs:_*)
  }
  def flows(address_width : Int, wbs: Wishbone*): Seq[(Data, Flow[Bits])] = {
    flows(AllMapping, address_width, wbs:_*)
  }
  def flows(wbs: Wishbone*): Seq[(Data, Flow[Bits])] = {
    flows(AllMapping, Int.MaxValue, wbs:_*)
  }
}

object SignalLogger {
  var assert_id = 0
  def log_assert(must_be_true: Bool, msg : String = "")(implicit location: Location): Unit = {
    val st = new Throwable().getStackTrace.mkString("\n")
    GlobalLogger.add_comment(s"Assert ${assert_id}: \n${st}")
    GlobalLogger(
      Set("asserts"),
      SignalLogger.concat(s"${must_be_true.name}_asserts_${assert_id}", must_be_true)
    )
    assert_id = assert_id + 1
  }
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
  def concat(name : String, signals: Any*): Seq[(Data, Flow[Bits])] = {
    def process_signal(s : Any): Seq[(String, Data)] = {
      s match {
        case v : Vector[Data] =>
          v.map(d => (d.getName(), d))
        case b : Bundle =>
          b.elements.map(x => (f"${b.name}_${x._1}", x._2))
        case d : Data =>
          Seq((d.getName(), d))
        case e : Seq[_] =>
          e.flatMap(x => process_signal(x))
        case t : (String, Data) => Seq(t)
        case _ => {
          assert(false)
          Seq()
        }
      }
    }

    val signal_elments = signals.filter(_ != null).flatMap(process_signal)
    signal_elments.foreach(x => assert(x._1.nonEmpty))
    signal_elments.foreach(_._2.addTag(crossClockDomain))

    val bundle = new Bundle {
      elements.append(signal_elments:_*)
    }.setName(s"${name}")
    val bundleFlow = Flow(Bits(bundle.getBitsWidth bits)).setName(s"${name}Flow")
    bundleFlow.payload.assignFromBits(bundle.asBits)
    bundleFlow.valid := bundle.asBits =/= RegNext(bundle.asBits).setName(name + "_prior")
    bundleFlow.setName(s"${name}")
    Seq((bundle, bundleFlow))
  }

  def concat(timeNumber: TimeNumber, name : String, signals: Data*): Seq[(Data, Flow[Bits])] = {
    concat(name, signals:_*).map(x => (x._1, RateLimitFlow(timeNumber, x._2)))
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

        logger.io.manual_trigger.setIdle()
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