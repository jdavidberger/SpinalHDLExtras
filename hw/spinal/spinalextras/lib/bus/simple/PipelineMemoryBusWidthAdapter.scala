package spinalextras.lib.bus.simple

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.simple._
import spinal.lib._
import spinal.lib.bus.misc.{AddressMapping, SizeMapping}
import spinal.lib.bus.regif.BusIf
import spinal.lib.sim.{FlowMonitor, ScoreboardInOrder}
import spinalextras.lib.Config
import spinalextras.lib.bus.{PipelinedMemoryBusCmdExt, PipelinedMemoryBusConfigExt}
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties, FormalProperty}
import spinalextras.lib.logging.{FlowLogger, GlobalLogger, PipelinedMemoryBusLogger, SignalLogger}
import spinalextras.lib.testing.test_funcs

import scala.language.postfixOps

object PipelineMemoryBusClockAdapter {
  def apply(bus: PipelinedMemoryBus, busClock: ClockDomain, newClock: ClockDomain = ClockDomain.current): PipelinedMemoryBus = {
    new Area {
      val busNew = PipelinedMemoryBus(bus.config)
      val inFactor = (newClock.frequency.getValue / busClock.frequency.getValue).floatValue().ceil.toInt

      val queue_size = 32 //1 << log2Up(inFactor.max(4)) + 1
      val overflow = Bool()
      if (bus.isMasterInterface) {
        busNew.cmd.queue(inFactor.max(32), newClock, busClock) >> bus.cmd
        bus.rsp.toStream(overflow).queue(inFactor.max(32), busClock, newClock).toFlow >> busNew.rsp
      } else {
        bus.cmd.queue(queue_size, busClock, newClock) <> busNew.cmd
        busNew.rsp.toStream(overflow).queue(queue_size, newClock, busClock).toFlow >> bus.rsp
      }
      assert(!overflow, s"Clocked bus pipeline has overflowed ${busClock.frequency.getValue} vs ${newClock.frequency.getValue}")

      GlobalLogger(
        Set("asserts"),
        SignalLogger.concat("pmbCCC", overflow.setName("overflow"))
      )

//      new ClockingArea(busClock) {
//        test_funcs.assertPMBContract(bus)
//      }
//      new ClockingArea(newClock) {
//        test_funcs.assertPMBContract(busNew)
//      }
    }.setName("Adapter").busNew
  }
}

case class PipelineMemoryBusWidthAdapter(pmbIn : PipelinedMemoryBusConfig,
                                         pmbOut : PipelinedMemoryBusConfig,
                                         rspQueue : Int = 4,
                                         endianness: Endianness = LITTLE
                                        ) extends Component {
  val io = new Bundle {
    val input = slave(PipelinedMemoryBus(pmbIn))
    val output = master(PipelinedMemoryBus(pmbOut))
  }

  val input_words = pmbIn.dataWidth / 8
  val output_words = pmbOut.dataWidth / 8
  val shift_in = log2Up(input_words)
  val shift_out = log2Up(output_words)

  val work =
    if(pmbIn.dataWidth == pmbOut.dataWidth) new Area {
      io.input <> io.output
    } else if(pmbIn.dataWidth > pmbOut.dataWidth) new Area {
      val factor = pmbIn.dataWidth / pmbOut.dataWidth

      val cmd = cloneOf(io.output.cmd)
      StreamTransactionExtender(io.input.cmd, cmd, factor - 1, noDelay = true) {
        (id, payload, _) => {
          val rid = if(endianness == BIG) (factor - id - 1) else id
          val cmd = PipelinedMemoryBusCmd(pmbOut)
          //cmd.address := (payload.address << (shift_in - shift_out)) + id
          cmd.address := payload.address + (id << cmd.config.wordAddressShift)
          cmd.data := (payload.data >> (cmd.data.getWidth * rid)).resized
          cmd.write := payload.write
          cmd.mask := (payload.mask >> (output_words * rid)).resized
          cmd
        }
      }
      cmd.throwWhen(cmd.write && cmd.mask === 0).stage() <> io.output.cmd
      //cmd.stage() <> io.output.cmd

      val rspStream = Stream(io.input.rsp.data.clone())
      val overflow = Bool()

      StreamWidthAdapter(io.output.rsp.map(_.data).toStream(overflow = overflow), rspStream, endianness = endianness)
      assert(!overflow, "Width adapter overflow")

      rspStream.ready := True
      io.input.rsp.valid := rspStream.valid
      io.input.rsp.data := rspStream.payload
    } else new Area {
      val input_size_per_output_size = shift_out - shift_in
      //require(endianness == LITTLE)

      val index = io.input.cmd.payload.wordAddress.resize((input_size_per_output_size) bits)
      val rindex = if(endianness == LITTLE) index else ((1 << input_size_per_output_size) - 1 - index)

      val cmdStream = io.input.cmd.map(payload => {
        val cmd = PipelinedMemoryBusCmd(pmbOut)

        //cmd.address := (payload.address >> (shift_out - shift_in)).resized
        cmd.address := (payload.address >> cmd.config.wordAddressShift) << cmd.config.wordAddressShift
        cmd.data := (payload.data << (pmbIn.dataWidth * rindex)).resized
        cmd.mask := (payload.mask << (input_words * rindex)).resized
        cmd.write := payload.write
        TupleBundle(cmd, rindex)
      })

      val (toOut, toQueue) = StreamFork2(cmdStream)
      val indices = toQueue.throwWhen(toQueue.payload._1.write).map(_._2)

      val q = if(rspQueue > 1) indices.queue(rspQueue) else indices.s2mPipe()
      toOut.map(_._1).throwWhen(toOut._1.write && toOut._1.mask === 0) <> io.output.cmd

      val overflow = Bool()
      StreamJoin(q, io.output.rsp.toStream(overflow).stage()).map( matched_rsp => {
        val rsp = PipelinedMemoryBusRsp(pmbIn)
        rsp.data := (matched_rsp._2.data >> (pmbIn.dataWidth * matched_rsp._1)).resized
        rsp
      }).toFlow <> io.input.rsp
      assert(!overflow, "Width adapter overflow 114")
    }

  def attach_debug_registers(busSlaveFactory: BusIf): Unit = {
    PipelinedMemoryBusLogger.attach_debug_registers(busSlaveFactory,
      io.input.setName("in_width_bus"),
      io.output.setName("out_width_bus"),
    )
  }
}

object PipelineMemoryBusWidthAdapter {
  def apply(bus: PipelinedMemoryBus, dataWidth: Int): PipelinedMemoryBus = {
    val newBus = PipelinedMemoryBus(bus.config.copy(dataWidth = dataWidth))
    val adapter = PipelineMemoryBusWidthAdapter(bus.config, newBus.config)
    adapter.io.input <> bus
    adapter.io.output <> newBus
    newBus
  }

  def apply(busIn: PipelinedMemoryBus, busOut: PipelinedMemoryBus): PipelineMemoryBusWidthAdapter = {
    val adapter = PipelineMemoryBusWidthAdapter(busIn.config, busOut.config)
    adapter.io.input <> busIn
    adapter.io.output <> busOut
    adapter
  }

}


case class SimpleMemoryProvider(init :  Seq[BigInt] = Seq.empty,
                                mapping : AddressMapping = SizeMapping(0, 65535),
                                config : PipelinedMemoryBusConfig = PipelinedMemoryBusConfig(32, 32)) extends ComponentWithFormalProperties {
  val io = new Bundle {
    val bus = slave(PipelinedMemoryBus(config))
  }
  //val memBusContract = test_funcs.assertPMBContract(io.bus)
  val busConfig = io.bus.config

  var data_width = busConfig.dataWidth
  var addr_width = busConfig.addressWidth

  val mem = Mem(Bits(data_width bits), if(init.nonEmpty) init.size.toBigInt else mapping.highestBound - mapping.lowerBound)
  if (init.nonEmpty) {
    val paddedInit = init ++ Seq.fill(mem.wordCount - init.length)(BigInt(0xFA))
    mem.init(paddedInit.map(B(_)))
  }

  io.bus.cmd.ready := True

  val write_counter, read_counter = Counter(32 bits)
  when(io.bus.cmd.fire && io.bus.cmd.write) {
    write_counter.increment()
  }

  when(io.bus.cmd.fire && !io.bus.cmd.write) {
    read_counter.increment()
  }

  var port = mem.readWriteSyncPort(maskWidth = io.bus.cmd.mask.getWidth)
  val wordAddressShift = busConfig.wordAddressShift
  when(io.bus.cmd.valid) {
    assert(mapping.hit(io.bus.cmd.address))
  }
  val mappedByteAddress = mapping.removeOffset(io.bus.cmd.address)
  port.address := ( mappedByteAddress >> wordAddressShift).resized
  port.wdata := io.bus.cmd.data
  port.mask := io.bus.cmd.mask
  port.write := io.bus.cmd.write
  port.enable := io.bus.cmd.fire

  val read = RegNext(io.bus.cmd.fire && !io.bus.cmd.write) init(False)
  io.bus.rsp.data := port.rdata
  io.bus.rsp.valid := read

  override def formalComponentProperties() = new FormalProperties(this) {

  }
}

