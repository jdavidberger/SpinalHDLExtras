package spinalextras.lib.mipi

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.regif.AccessType.{RO, ROV, WC}
import spinal.lib.bus.regif.{BusIf, SymbolName}
import spinal.lib.sim.{FlowMonitor, ScoreboardInOrder}
import spinalextras.lib.Config

import scala.language.postfixOps

case class byte2pixel(cfg : MIPIConfig,
                      byte_cd: ClockDomain,
                      pixel_cd: ClockDomain = ClockDomain.current) extends Component {
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


  def csi_unpack_to_pixel(d : Vec[Bits]): Vec[Bits] = {
    val dw = MIPIDataTypes.bit_width(cfg.ref_dt)
    if(dw == 8)
      return d

    // MIPI binary backing is sorta weird. It takes the 8 MS bits of each pixel and sends then in order, and then when
    // there are 8 'leftover' bits, thats the next byte
    assert(dw == 12 || dw == 10)

    // How many full bytes do we get before a leftover byte
    val group_cnt = 8 / (dw - 8)

    // Each full byte gets this many bits added on
    val leftover_bits = 8 / group_cnt

    // Make groups of the full bytes and the leftover byte. The last byte is the leftover byte
    val groups = d.grouped(group_cnt + 1).toSeq

    Vec(groups.flatMap(group => {
      val full_bytes = group.dropRight(1)
      val leftover_byte = group.last
      val leftover_bit_groups = leftover_byte.subdivideIn(leftover_bits bits).reverse

      // Merge all the full bytes with the leftover_bits by concatenating the lsb's onto the end.
      full_bytes.zip(leftover_bit_groups).map { case (byte, lsb) => byte ## lsb }
    }))
  }

  val lcm_width = lcm(cfg.GEARED_LANES, cfg.DT_WIDTH)
  val fifo_min_depth = 16 // Math.pow(2, (1 + Math.log(lcm_width / cfg.GEARED_LANES) / Math.log(2)).ceil).toInt

  val byte_cd_freq = byte_cd.frequency.getValue
  val byte_phy_freq = cfg.dphy_byte_freq

  val byte_clock_fast_enough = byte_cd_freq >= byte_phy_freq
  val pixel_clock_fast_enough = byte_phy_freq * cfg.GEARED_LANES <= pixel_cd.frequency.getValue * cfg.DT_WIDTH

  if(!byte_clock_fast_enough || !pixel_clock_fast_enough) {
    println("Byte To pixel component configuration not viable")
    println(s"Byte Clock: ${byte_cd_freq}")
    println(s"Pixel Clock: ${pixel_cd.frequency.getValue}")
    println(s"Byte data rate: ${byte_phy_freq * cfg.GEARED_LANES}")
    println(s"Pixel data rate: ${pixel_cd.frequency.getValue * cfg.DT_WIDTH}")
  }
  require(byte_clock_fast_enough)
  require(pixel_clock_fast_enough)

  val byte_count = 2400
  val clock_ratio = (byte_phy_freq / pixel_cd.frequency.getValue).toDouble
  val delay_time_ratio = 8.0 * ((1.0 / cfg.GEARED_LANES) - clock_ratio / cfg.DT_WIDTH)
  val delay_time = byte_count * delay_time_ratio
  // TODO -- if desired you can use delay_time above to keep line_valid solid for one full row

  val byte_clock_fifo = cfg.DT_WIDTH > cfg.GEARED_LANES
  val fifoWidth = if(byte_clock_fifo) cfg.DT_WIDTH else cfg.GEARED_LANES
  val fifoType = TupleBundle2(Bits(2 bits), Bits(fifoWidth bits))
  val fifo = new StreamFifoCC(fifoType, fifo_min_depth,
    pushClock = byte_cd, popClock = pixel_cd)

  val conversion_clock = if(byte_clock_fifo) byte_cd else pixel_cd
  val conversion_area = new ClockingArea(conversion_clock) {
    val lcm_stream = Stream(Vec(Bits(8 bits), lcm_width/8))
    val pixel_stream = Stream(Vec(Bits(MIPIDataTypes.bit_width(cfg.ref_dt) bits), cfg.OUTPUT_LANES))
    pixel_stream.ready := True

    val bytes = Flow(Bits(cfg.GEARED_LANES bits))
    val overflow = Bool()

    assert(overflow === False, "bytes overflow")
    StreamWidthAdapter(bytes.toStream(overflow), lcm_stream)
    StreamWidthAdapter(lcm_stream.map(csi_unpack_to_pixel).stage(), pixel_stream)
  }

  val byte_clk_area = new ClockingArea(byte_cd) {
    val isRefDt, fv = RegInit(False)

    val watermarkReg = RegInit(fifo.io.pushOccupancy)
    when(fifo.io.pushOccupancy > watermarkReg) {
      watermarkReg := fifo.io.pushOccupancy
    }

    val stallCount = Counter(32 bits)
    when(fifo.io.push.isStall) {
      stallCount.increment()
    }

    when(io.mipi_header.fire) {
      when(io.mipi_header.is_long_av_packet) {
        isRefDt := True
      } elsewhen(io.mipi_header.is_long_packet) {
        isRefDt := False
      }

      when(io.mipi_header.is_short_packet) {
        when(io.mipi_header.datatype === 0) { // Frame start short packet
          fv := True
        } elsewhen(io.mipi_header.datatype === 1) { // Frame end short packet
          fv := False
        }
      }
    }

    if(byte_clock_fifo) {
      conversion_area.bytes << io.payload.takeWhen(isRefDt)

      fifo.io.push.payload._1 := Delay(fv, 5) ## conversion_area.pixel_stream.valid
      fifo.io.push.payload._2 := conversion_area.pixel_stream.payload.asBits

    } else {
      fifo.io.push.payload._1 := fv ## (io.payload.valid && isRefDt)
      fifo.io.push.payload._2 := io.payload.payload
    }

    // When the byte cd freq is slower than the pixel clock; we can just flood the fifo. But when the byte clock
    // is faster, only do it on changes.
    fifo.io.push.valid := {
      if (byte_cd_freq > pixel_cd.frequency.getValue)
        fifo.io.push.payload._1 =/= RegNext(fifo.io.push.payload._1)
      else
        True
    }

    assert(fifo.io.push.ready, "Fifo overflow")
  }

  val pixel_clk_area = new ClockingArea(pixel_cd) {
    fifo.io.pop.ready := True
    val fv = RegNextWhen(fifo.io.pop.payload._1(1), fifo.io.pop.fire)

    val watermarkReg = RegInit(fifo.io.popOccupancy)
    when(fifo.io.popOccupancy > watermarkReg) {
      watermarkReg := fifo.io.popOccupancy
    }

    if(byte_clock_fifo) {
      io.pixelFlow.frame_valid := fv
      io.pixelFlow.valid := fifo.io.pop.fire && fifo.io.pop.payload._1(0)
      io.pixelFlow.payload := fifo.io.pop.payload._2.asBits
    } else {
      conversion_area.bytes.valid := fifo.io.pop.fire && fifo.io.pop.payload._1(0)
      conversion_area.bytes.payload := fifo.io.pop.payload._2

      io.pixelFlow.frame_valid := Delay(fv, 5)
      io.pixelFlow.valid := conversion_area.pixel_stream.valid
      io.pixelFlow.payload := conversion_area.pixel_stream.payload.asBits
    }

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

    for(counter : UInt <- Seq(
      byte_clk_area.stallCount.value.setName("stall"),
      byte_clk_area.watermarkReg.setName("push_occ_watermark"),
      pixel_clk_area.watermarkReg.setName("pop_occ_watermark")
    )) {
      val reg = busSlaveFactory.newReg(counter.name)(SymbolName(s"${counter.name}"))
      val cnt = reg.field(UInt(32 bits), RO, counter.name)(SymbolName(s"${counter.name}_cnt"))
      cnt := BufferCC(counter).resized
    }

    for(error_signal <- Seq(
      BufferCC(io.pixelFlow.line_valid).setPartialName("line_valid"),
      BufferCC(io.pixelFlow.frame_valid).setPartialName("frame_valid"),
      BufferCC(~io.pixelFlow.frame_valid).setPartialName("not_frame_valid"),
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

