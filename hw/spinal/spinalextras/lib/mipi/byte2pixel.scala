package spinalextras.lib.mipi

import com.kjetland.jackson.jsonSchema._
import com.fasterxml.jackson.databind._
import com.fasterxml.jackson.databind.node._
import com.fasterxml.jackson.databind.module.SimpleModule
import com.fasterxml.jackson.dataformat.yaml._
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import spinal.core._
import spinal.lib._
import spinal.lib.bus.regif.AccessType.{RO, ROV, WC}
import spinal.lib.bus.regif.{BusIf, SymbolName}
import spinalextras.lib.Config
import spinalextras.lib.ipgen.{IPGenerator, IPGeneratorOptions, IPGenerator_}
import spinalextras.lib.lattice.IPX
import spinalextras.lib.logging.{FlowLogger, GlobalLogger}
import spinalextras.lib.mipi.MIPIDataTypes.{MIPIDataTypes, RAW10}
import spinalextras.lib.misc.{ClockSpecification, HertzDeserializer}

import scala.collection.JavaConverters._
import java.io.FileReader
import scala.Console.println
import scala.language.postfixOps

class Byte2PixelConfig(val mipiConfig : MIPIConfig,
                       val pixelClockFrequency : ClockSpecification
                      ) {
}

case class byte2pixel(cfg : MIPIConfig,
                      byte_cd: ClockDomain,
                      var pixel_cd: ClockDomain = null) extends Component {
  if(pixel_cd == null) {
    pixel_cd = ClockDomain.current
  }
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
    val dw = MIPIDataTypes.bit_width(cfg.refDt)
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
  val byte_phy_freq = cfg.dphyByteFreq

  val byte_clock_fast_enough = byte_cd_freq >= byte_phy_freq
  val pixel_clock_fast_enough = byte_phy_freq * cfg.GEARED_LANES <= pixel_cd.frequency.getValue * cfg.DT_WIDTH

  if(!byte_clock_fast_enough || !pixel_clock_fast_enough) {
    System.err.println("Byte To pixel component configuration not viable")
    System.err.println(s"Byte Clock: ${byte_cd_freq.decomposeString} should be >= ${byte_phy_freq.decomposeString}")
    System.err.println(s"Pixel Clock: ${pixel_cd.frequency.getValue} should be >= ${byte_phy_freq * cfg.GEARED_LANES / cfg.DT_WIDTH}")
    System.err.println(s"Byte data rate: ${byte_phy_freq * cfg.GEARED_LANES}")
    System.err.println(s"Pixel data rate: ${pixel_cd.frequency.getValue * cfg.DT_WIDTH}")
  }

  require(byte_clock_fast_enough)
  require(pixel_clock_fast_enough)

  {
    val pixelCounter, lineCounter = Counter(32 bits)
    val pixelCounterFlow = Flow(TupleBundle(UInt(32 bits), UInt(32 bits)))
    pixelCounterFlow.setName("pixelCounterFlow")
    pixelCounterFlow.valid := False
    pixelCounterFlow.payload._1 := pixelCounter
    pixelCounterFlow.payload._2 := lineCounter
    when(~io.pixelFlow.line_valid) {
      lineCounter.clear()
    }
    when(~io.pixelFlow.frame_valid) {
      pixelCounter.clear()
      when(pixelCounter =/= 0) {
        pixelCounterFlow.valid := True
      }
    }
    when(io.pixelFlow.valid) {
      pixelCounter.increment()
      lineCounter.increment()
    }

    GlobalLogger(Set("mipi"),
      FlowLogger.flows(pixelCounterFlow)
    )
  }

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
    val pixel_stream = Stream(Vec(Bits(MIPIDataTypes.bit_width(cfg.refDt) bits), cfg.outputLanes))
    pixel_stream.ready := True

    val bytes = Flow(Bits(cfg.GEARED_LANES bits))
    val overflow = Bool()

    assert(overflow === False, "bytes overflow")
    StreamWidthAdapter(bytes.toStream(overflow), lcm_stream)
    StreamWidthAdapter(lcm_stream.map(csi_unpack_to_pixel).stage(), pixel_stream)
  }

  val byte_clk_area = new ClockingArea(byte_cd) {
    val isRefDt, fv, inFSPacket = RegInit(False)

    val watermarkReg = Reg(fifo.io.pushOccupancy) init(0)
    when(fifo.io.pushOccupancy > watermarkReg) {
      watermarkReg := fifo.io.pushOccupancy
    }

    val stallCount = Counter(32 bits)
    when(fifo.io.push.isStall) {
      stallCount.increment()
    }

    when(io.mipi_header.fire) {
      when(inFSPacket) {
        fv := True
        inFSPacket := False
      }

      when(io.mipi_header.is_long_av_packet) {
        isRefDt := True
      } elsewhen(io.mipi_header.is_long_packet) {
        isRefDt := False
      }

      when(io.mipi_header.is_short_packet) {
        when(io.mipi_header.datatype === 0) { // Frame start short packet
          inFSPacket := True
          fv := False
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

    GlobalLogger(Set("mipi"),
      FlowLogger.flows(io.mipi_header)
    )
  }

  val pixel_clk_area = new ClockingArea(pixel_cd) {
    fifo.io.pop.ready := True
    val fv = RegNextWhen(fifo.io.pop.payload._1(1), fifo.io.pop.fire)

    val watermarkReg = Reg(fifo.io.popOccupancy) init(0)
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



class GenerateByte2Pixel extends IPGenerator_[Byte2PixelConfig]{

  override def defaultClockDomainFrequency(cfg : Byte2PixelConfig): IClockDomainFrequency = {
    cfg.pixelClockFrequency.toClockFrequency()
  }

  override def Schema(): JsonNode = {
    val schema = super.Schema()
    MIPIConfig.patchEnumFields(json_mapper, schema)
    schema
  }

  override def processConfig(options: IPGeneratorOptions, config: Byte2PixelConfig): Unit = {
    processRtl(
      options,
      config,
      () => new byte2pixel(config.mipiConfig,
        ClockDomain.external("byte_cd", frequency = FixedFrequency(config.mipiConfig.dphyByteFreq)))
    )
  }

  override def customMappings(module: SimpleModule): Unit = {
    super.customMappings(module)
    module.addDeserializer(classOf[MIPIDataTypes], MIPIDatatypeDeserializer())
    module.addSerializer(classOf[MIPIDataTypes], MIPIDatatypeSerializer())
    module.addDeserializer(classOf[HertzNumber], HertzDeserializer())
  }

  override def DefaultConfig: Option[Byte2PixelConfig] = super.DefaultConfig

  override def ConfigExample: Byte2PixelConfig = {
    new Byte2PixelConfig(
      mipiConfig = MIPIConfig(
        numRXLanes = 2,
        rxGear = 8,
        outputLanes = 1,
        refDt = RAW10,
        dphyByteFreq = 50 MHz
      ),
      pixelClockFrequency = ClockSpecification(80 MHz),
    )
  }
  override def Labels: Seq[String] = Seq("MIPI")
  override def Name: String = "MIPI Byte To Pixel"
  override def Description: String =
    """
      |An IP block that takes in raw MIPI bytes and produces pixels of a given width. The pixel output has a last flag
      |which signifies the end of a byte packet stream.
      |""".stripMargin

}

object GenerateByte2Pixel {
  IPGenerator.KnownGenerators.update(new GenerateByte2Pixel().Name, () => new GenerateByte2Pixel())
  def main(args: Array[String]): Unit = {
    new GenerateByte2Pixel().cli_main(args)
  }
}

