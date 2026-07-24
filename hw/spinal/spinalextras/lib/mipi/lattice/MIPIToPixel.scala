package spinalextras.lib.mipi.lattice

import spinal.core._
import spinal.lib._
import spinal.lib.bus.regif.BusIf
import spinalextras.lib.blackbox.lattice.lifcl.dphy_rx
import spinalextras.lib.mipi._

import scala.language.postfixOps

case class MIPIToPixel(cfg : MIPIConfig,
                       sync_cd : ClockDomain,
                       pixel_cd : ClockDomain,
                       byte_cd : ClockDomain = null,
                       sensor_name : String = "",
                       clock_suffix : Boolean = true,
                       is_continous_clock : Option[Boolean] = None
                 ) extends Component {
  val io = new Bundle {
    val mipi = slave(MIPIIO(cfg.numRXLanes))
    val pll_lock = in(Bool())

    val tx_rdy = in(Bool()) default(True)

    val pixelFlow = master(Flow(Fragment(Vec(Bits(cfg.PIX_WIDTH bits), cfg.outputLanes))))

    /**
     * Header events in pixel_cd.
     * sof/eof: short packet dt 0/1; line: long AV with CSI-2 payload byte count.
     */
    val stats = master Flow(MipiCameraStatsEvent())
  }
  val byte_freq = cfg.dphyByteFreq

  if(sensor_name != "") {
    io.mipi.setPartialName(s"${sensor_name}_mipi")
    io.pixelFlow.setPartialName(s"${sensor_name}_pixelFlow")
  }

  noIoPrefix()
  val mipi_to_bytes = new dphy_rx(cfg, sync_cd = sync_cd, byte_cd = byte_cd, clock_suffix = clock_suffix, is_continous_clock = is_continous_clock,
  //  enable_fifo_misc_signals = Some(true)
  )

  mipi_to_bytes.assignMIPI(io.mipi)

  mipi_to_bytes.io.pll_lock_i := io.pll_lock
  mipi_to_bytes.io.tx_rdy_i := io.tx_rdy
  // ref_dt_i is left to its port default (cfg.refDt.id). When attach_bus() is called it
  // is overridden by the runtime-writable ref_dt register (see dphy_rx.attach_bus).

  mipi_to_bytes.io.rxcsr_dropnull_i := False
  mipi_to_bytes.io.rxcsr_vcx_on_i := False

  val bytes_to_pixels = byte2pixel(cfg, pixel_cd = pixel_cd, byte_cd = mipi_to_bytes.byte_cd())

  val mipiHdr = mipi_to_bytes.MIPIPacketHeader
  bytes_to_pixels.assignMIPIHeader(mipiHdr)
  bytes_to_pixels.assignMIPIBytes(mipi_to_bytes.MIPIBytes)

  // Header sideband: byte_cd -> pixel_cd. Prefer handshaking StreamCC; FlowCCByToggle
  // is unsafe if events arrive faster than the destination can sample.
  val byteCd = mipi_to_bytes.byte_cd()
  val statsByte = new ClockingArea(byteCd) {
    val flow = Flow(MipiCameraStatsEvent())
    val sof = mipiHdr.fire && mipiHdr.is_short_packet && mipiHdr.datatype === 0
    val eof = mipiHdr.fire && mipiHdr.is_short_packet && mipiHdr.datatype === 1
    val line = mipiHdr.fire && mipiHdr.is_long_av_packet
    flow.valid := sof || eof || line
    flow.sof := sof
    flow.eof := eof
    flow.line := line
    flow.word_count := Mux(line, mipiHdr.word_count, U(0, 16 bits))
    flow
  }.flow

  if (ClockDomain.areSynchronous(byteCd, pixel_cd)) {
    io.stats << statsByte
  } else {
    val overflow = Bool()
    io.stats << statsByte.toStream(overflow).ccToggle(byteCd, pixel_cd).toFlow
  }

  io.pixelFlow <> PixelFlow2Fragment(bytes_to_pixels.io.pixelFlow).map(f => {
    val outFlow = Fragment(Vec(Bits(cfg.PIX_WIDTH bits), cfg.outputLanes))
    outFlow.last := f.last
    outFlow.fragment.assignFromBits(f.fragment)
    outFlow
  })

  def byte_clock_domain() : ClockDomain = {
    mipi_to_bytes.byte_cd()
  }

  val input_rate = cfg.rxGear * cfg.numRXLanes * cfg.dphyByteFreq.toDouble
  val sink_rate = cfg.DT_WIDTH * pixel_cd.frequency.getValue.toDouble
  require(input_rate <= sink_rate, s"Configuration doesn't work; pixel clock can't keep up with the output ${input_rate} >= ${sink_rate}")

  def attach_bus(busSlaveFactory: BusIf): Unit = {
    mipi_to_bytes.attach_bus(busSlaveFactory)
    bytes_to_pixels.attach_bus(busSlaveFactory)
  }
}
