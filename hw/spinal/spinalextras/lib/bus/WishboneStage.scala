package spinalextras.lib.bus

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.wishbone.{AddressGranularity, Wishbone, WishboneConfig}
import spinal.lib.sim.ScoreboardInOrder
import spinal.lib.wishbone.sim.{WishboneDriver, WishboneMonitor, WishboneSequencer, WishboneTransaction}
import spinalextras.lib.Config
import spinalextras.lib.logging.{GlobalLogger, PipelinedMemoryBusLogger, WishboneBusLogger}

case class WishboneCmd(config: WishboneConfig) extends Bundle {
  val WE        = Bool()
  val ADR       = UInt(config.addressWidth bits)
  val DAT_MOSI  = Bits(config.dataWidth bits)

  val SEL       = if(config.useSEL)   Bits(config.selWidth bits) else null
  val LOCK      = if(config.useLOCK)  Bool()                     else null
  val CTI       = if(config.useCTI)   Bits(3 bits)               else null

  val TGD_MOSI  = if(config.useTGD)   Bits(config.tgdWidth bits) else null
  val TGA       = if(config.useTGA)   Bits(config.tgaWidth bits) else null
  val TGC       = if(config.useTGC)   Bits(config.tgcWidth bits) else null
  val BTE       = if(config.useBTE)   Bits(2 bits)               else null

  def connect(bus : Wishbone): Unit = {
    val el = Map(elements:_*)
    val bus_el = Map(bus.elements:_*)
    for ((k,v) <- el) {
      if(v != null)
        v <> bus_el(k)
    }
  }
}

case class WishboneRsp(config: WishboneConfig) extends Bundle {
  val DAT_MISO  = Bits(config.dataWidth bits)
  val TGD_MISO  = if(config.useTGD)   Bits(config.tgdWidth bits) else null
  val ERR       = if(config.useERR)   Bool()                     else null
  val RTY       = if(config.useRTY)   Bool()                     else null

  def connect(bus : Wishbone): Unit = {
    val el = Map(elements:_*)
    val bus_el = Map(bus.elements:_*)
    for ((k,v) <- el) {
      if(v != null)
        v <> bus_el(k)
    }
  }
}

case class WishboneStream(config : WishboneConfig) extends Bundle with IMasterSlave {
  val cmd = Stream(WishboneCmd(config))
  val rsp = Flow(WishboneRsp(config))

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }

  def cmdM2sPipe(): WishboneStream = {
    val ret = cloneOf(this)
    this.cmd.m2sPipe() >> ret.cmd
    this.rsp << ret.rsp
    ret
  }

  def cmdS2mPipe(): WishboneStream = {
    val ret = cloneOf(this)
    this.cmd.s2mPipe() >> ret.cmd
    this.rsp << ret.rsp
    ret
  }

  def rspPipe(): WishboneStream = {
    val ret = cloneOf(this)
    this.cmd >> ret.cmd
    this.rsp << ret.rsp.stage()

    ret
  }

  def <<(m : WishboneStream) : Unit = {
    val s = this
    assert(m.config.addressWidth >= s.config.addressWidth)
    assert(m.config.dataWidth == s.config.dataWidth)
    s.cmd << m.cmd
    m.rsp >> s.rsp
  }
  def >>(s : WishboneStream) : Unit = s << this

}

case class Wb2WishboneStream_s2m(config : WishboneConfig, rspPipe : Boolean) extends Component {
  val io = new Bundle {
    val bus = slave(Wishbone(config))
    val stream = master(WishboneStream(config))
  }

  val stream = cloneOf(io.stream)

  stream.cmd.connect(io.bus)
  stream.rsp.connect(io.bus)

  stream.cmd.valid := io.bus.masterHasRequest
  io.bus.ACK := stream.rsp.valid

  if(rspPipe) {
    io.stream <> stream.rspPipe()
  } else {
    io.stream <> stream
  }

}

case class Wb2WishboneStream_m2s(config : WishboneConfig) extends Component {
  val io = new Bundle {
    val bus = master(Wishbone(config))
    val stream = slave(WishboneStream(config))
  }

  val stream = cloneOf(io.stream)

  stream.cmd.connect(io.bus)
  stream.rsp.connect(io.bus)

  io.bus.CYC := stream.cmd.valid
  io.bus.STB := stream.cmd.valid

  stream.cmd.ready := io.bus.isRequestAck
  stream.rsp.valid := io.bus.isResponse

  io.stream <> stream
}

object WishboneStream {
  def apply(bus : Wishbone, rspPipe : Boolean) : WishboneStream = {
    require(bus.isMasterInterface || bus.isSlaveInterface)
    if(bus.isMasterInterface) {
      val dut = Wb2WishboneStream_m2s(bus.config)
      dut.io.bus <> bus
      if (rspPipe) dut.io.stream.rspPipe() else dut.io.stream
    } else {
      val dut = Wb2WishboneStream_s2m(bus.config, rspPipe = rspPipe)
      dut.io.bus <> bus
      dut.io.stream
    }
  }

  def apply(stream : WishboneStream, rspPipe : Boolean) : Wishbone = {
    //assert(stream.isMasterInterface || stream.isSlaveInterface)
    if(!stream.isSlaveInterface) {
      val dut = Wb2WishboneStream_m2s(stream.config)
      dut.io.stream <> (if (rspPipe) stream.rspPipe() else stream)
      dut.io.bus
    } else {
      val dut = Wb2WishboneStream_s2m(stream.config, rspPipe)
      dut.io.stream <> stream
      dut.io.bus
    }
  }
}

object WishboneStage {
  def apply(bus : Wishbone): Wishbone = {
      val adapter_pmb = WishboneToPipelinedMemoryBus(bus, 1)
      PipelinedMemoryBusToWishbone(adapter_pmb.cmdM2sPipe(), 1, bus.config)
  }

  /**
   * Takes in / returns a master-driven signal
   * @return
   */
  def apply(bus: Wishbone, m2s_stage: Boolean, s2m_stage: Boolean = false): Wishbone = {
    {
      if(!m2s_stage && !s2m_stage)
        return bus

      val adapter_pmb = WishboneToPipelinedMemoryBus(bus, 1)
      val out_bus = PipelinedMemoryBusToWishbone(
        (m2s_stage, s2m_stage) match {
          case (true, true) => adapter_pmb.cmdM2sPipe().cmdS2mPipe().rspPipe()
          case (false, true) => adapter_pmb.cmdS2mPipe().rspPipe()
          case (true, false) => adapter_pmb.cmdM2sPipe()
          case (false, false) => adapter_pmb
        },
        1, bus.config)

      GlobalLogger(
        Set("debug-wb"),
        WishboneBusLogger.flows(bus, out_bus.setName("out_bus")),
        PipelinedMemoryBusLogger.flows(adapter_pmb.setName("adapter_pmb"))
      )

      out_bus
    }
  }
}


