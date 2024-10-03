package spinalextras.lib.mipi

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.regif.AccessType.{ROV, WC}
import spinal.lib.bus.regif.{BusIf, SymbolName}
import spinal.lib.sim.{FlowMonitor, ScoreboardInOrder}
import spinalextras.lib.Config

import scala.language.postfixOps

case class byte2pixel(cfg : MIPIConfig, byte_cd: ClockDomain, pixel_cd: ClockDomain = ClockDomain.current) extends Component {
  val io = new Bundle {
    val mipi_header = slave Flow(MIPIPacketHeader(cfg))
    val payload = slave Flow(Bits(cfg.GEARED_LANES bits))

    val pixelFlow = master(PixelFlow(cfg.DT_WIDTH))
  }

  private def lcm(numberOne: Int, numberTwo: Int) = {
    val bigger = Math.max(numberOne, numberTwo)
    val smaller = Math.min(numberOne, numberTwo)

    (1 until smaller).filter((factor) => (factor * bigger) % smaller == 0).map((factor) => Math.abs(factor * bigger)).toVector(0)
  }

  val lcm_width = lcm(cfg.GEARED_LANES, cfg.DT_WIDTH)
  val fifo_min_depth = Math.pow(2, (1 + Math.log(lcm_width / cfg.GEARED_LANES) / Math.log(2)).ceil).toInt
  val fifo = new StreamFifoCC(TupleBundle2(Bits(2 bits), Bits(cfg.GEARED_LANES bits)), fifo_min_depth,
    pushClock = byte_cd, popClock = pixel_cd)

  val byte_count = 2400
  val clock_ratio = (byte_cd.frequency.getValue /  pixel_cd.frequency.getValue).toDouble
  val delay_time_ratio = 8.0 * ((1.0 / cfg.GEARED_LANES) - clock_ratio / cfg.DT_WIDTH)
  val delay_time = byte_count * delay_time_ratio
  // TODO -- if desired you can use delay_time above to keep line_valid solid for one full row

  require(byte_cd.frequency.getValue * cfg.GEARED_LANES <= pixel_cd.frequency.getValue * cfg.DT_WIDTH)
  val byte_clk_area = new ClockingArea(byte_cd) {
    val isRefDt, fv = RegInit(False)

    val lv = io.payload.valid && isRefDt
    val fv_lv_state = fv ## lv
    fifo.io.push.payload._1 := fv_lv_state
    fifo.io.push.payload._2 := io.payload.payload

    fifo.io.push.valid := True
    when(fv_lv_state =/= RegNext(fv_lv_state)) {
      fifo.io.push.valid := True
    }
    assert(fifo.io.push.ready, "Fifo full on b2p")

    when(io.mipi_header.fire) {
      when(io.mipi_header.is_long_av_packet) {
        isRefDt := True
      } elsewhen(io.mipi_header.is_long_packet) {
        isRefDt := False
      }

      when(io.mipi_header.is_short_packet) {
        when(io.mipi_header.datatype === 0) {
          fv := True
        } elsewhen(io.mipi_header.datatype === 1) {
          fv := False
        }
      }
    }
  }

  def csi_remap(d : Vec[Bits]): Vec[Bits] = {
    val dw = MIPIDataTypes.bit_width(cfg.ref_dt)
    if(dw == 8)
      return d

    assert(dw == 12 || dw == 10)
    val group_cnt = 8 / (dw - 8)
    val groups = d.grouped(group_cnt + 1).toSeq

    val remaped_groups = groups.flatMap(partition => {
      val lcb = partition.last.asBits.subdivideIn(8 / group_cnt bits).reverse
      partition.dropRight(1).zip(lcb).map(x => x._1 ## x._2)
    })

    Vec(remaped_groups)
  }

  val pixel_clk_area = new ClockingArea(pixel_cd) {
    val lcm_stream = Stream(Vec(Bits(8 bits), lcm_width/8))
    val pixel_stream = Stream(Vec(Bits(MIPIDataTypes.bit_width(cfg.ref_dt) bits), cfg.OUTPUT_LANES))
    pixel_stream.ready := True

    fifo.io.pop.ready := True

    val fv = RegNextWhen(fifo.io.pop.payload._1(1), fifo.io.pop.fire)

    val bytes = Flow(Bits(cfg.GEARED_LANES bits))
    bytes.valid := fifo.io.pop.fire && fifo.io.pop.payload._1(0)
    bytes.payload := fifo.io.pop.payload._2

    val overflow = Bool()
    assert(overflow === False, "bytes overflow")
    StreamWidthAdapter(bytes.toStream(overflow), lcm_stream)
    StreamWidthAdapter(lcm_stream.map(csi_remap).stage(), pixel_stream)

    io.pixelFlow.frame_valid := Delay(fv, 5)
    io.pixelFlow.valid := pixel_stream.valid
    io.pixelFlow.payload := pixel_stream.payload.asBits

    assert(!(io.pixelFlow.frame_valid == False && io.pixelFlow.valid == True), "Linevalid when frame valid is false")
  }

  def assignMIPIBytes(bytes : Flow[Bits]): Unit = {
    io.payload << bytes
  }

  def assignMIPIHeader(header : Flow[MIPIPacketHeader]) = {
    io.mipi_header << header
  }


  def attach_bus(busSlaveFactory: BusIf): Unit = {
    Component.current.withAutoPull()

    val sig = busSlaveFactory.newReg("b2p start")
    val signature = sig.field(Bits(32 bit), ROV, BigInt("F000A803", 16), "ip sig")

    for(error_signal <- Seq(
      BufferCC(~fifo.io.push.ready).setPartialName("fifo_full"),
      io.pixelFlow.line_valid,
      io.pixelFlow.frame_valid,
      io.pixelFlow.frame_valid.setPartialName("not_frame_valid"),
      BufferCC(io.payload.valid).setPartialName("payload_valid")
    )) {
      val reg = busSlaveFactory.newReg(error_signal.name)(SymbolName(s"${error_signal.name}"))
      val cnt = reg.field(UInt(32 bits), WC, error_signal.name)(SymbolName(s"${error_signal.name}_cnt")) init(0)
      when(error_signal) {
        cnt := cnt + 1
      }
    }
  }
}

class byte2PixelTest extends AnyFunSuite {
  val cfg = new MIPIConfig(2, 8, 1, MIPIDataTypes.RAW12)
  test("Basic") {
    Config.sim.withConfig(Config.spinalConfig.copy(defaultClockDomainFrequency = FixedFrequency(83.333 MHz))).doSim(
      new byte2pixel(cfg, ClockDomain.external("byte", frequency = FixedFrequency(50 MHz)))) { dut =>
      dut.clockDomain.forkStimulus(83.333 MHz)
      dut.byte_cd.forkStimulus(50 MHz)

      dut.byte_cd.waitSampling(1)
      dut.io.mipi_header.valid #= false
      dut.io.payload.valid #= false

      dut.clockDomain.waitSampling(5)

      val sco = new ScoreboardInOrder[Int]

      FlowMonitor(dut.io.pixelFlow, dut.clockDomain) {
        px => sco.pushDut(px.toInt)
      }

      dut.io.mipi_header.payload.datatype #= cfg.ref_dt.id
      dut.io.mipi_header.payload.is_long_av_packet #= false
      dut.io.mipi_header.payload.is_long_packet #= false


      def send_mipi_hdr(dt : Int, long : Boolean): Unit = {
        dut.io.mipi_header.payload.datatype #= dt
        dut.io.mipi_header.valid #= true
        dut.io.mipi_header.is_long_packet #= long
        dut.io.mipi_header.is_long_av_packet #= long
        dut.byte_cd.waitSampling(1)
        dut.io.mipi_header.valid #= false
      }

      for(n <- 0 until  5) {
        send_mipi_hdr(0, false)
        send_mipi_hdr(cfg.ref_dt.id, true)
        dut.byte_cd.waitSampling(20)
        for(i <- 0 until  10) {
          for (j <- 0 until  20) {
            dut.io.payload.valid #= true
            val pix = j //simRandom.nextInt(1 << 16)
            dut.io.payload.payload #= pix
            dut.byte_cd.waitSampling()
            dut.io.payload.valid #= false
            dut.io.payload.payload #= simRandom.nextInt(1 << 10)
          }
        }
        dut.byte_cd.waitSampling(20)
        send_mipi_hdr(1, false)
        dut.byte_cd.waitSampling(20)
      }

      //sco.checkEmptyness()
    }
  }
}