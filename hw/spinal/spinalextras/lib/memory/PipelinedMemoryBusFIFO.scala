package spinalextras.lib.memory

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.{DefaultMapping, SizeMapping}
import spinal.lib.bus.regif.AccessType.{RO, RW}
import spinal.lib.bus.regif.{BusIf, SymbolName}
import spinal.lib.bus.simple._
import spinalextras.lib.bus.PipelineMemoryGlobalBus
import spinalextras.lib.misc.{CounterUpDownUneven, RegisterTools, StreamTools}
import spinalextras.lib.testing.test_funcs

import scala.language.postfixOps

case class PipelinedMemoryBusBuffer[T <: Data](dataType : HardType[T], depth : Int, baseAddress : Int = 0,
                                               var config : PipelinedMemoryBusConfig = null,
                                               rsp_latency : Int = 0, cmd_latency : Int = 0, read_trigger : Int = -1) extends Component {
  if(config == null) {
    config = PipelinedMemoryBusConfig(addressWidth = log2Up(depth), dataWidth = dataType.getBitsWidth)
  }
  val bufferSize = rsp_latency + 1
  val midDatatype = Bits(StreamTools.lcm(dataType.getBitsWidth, config.dataWidth) bits)
  val bufferSizeInBits = bufferSize * config.dataWidth
  val bufferSizeInMidWords = bufferSizeInBits / midDatatype.getWidth
  val readTriggerInBits = read_trigger * config.dataWidth

  val io = new Bundle {
    val bus = master(PipelinedMemoryBus(config))
    val pop = master Stream (dataType)

    val memoryValid = in Bool()
    val memoryRead = out Bool()
  }
  val readBus = io.bus
  test_funcs.assertPMBContract(io.bus)

  val readBusCmd = cloneOf(readBus.cmd)
  readBus.cmd <> readBusCmd.queue(cmd_latency)

  val read_counter = Counter(depth, readBusCmd.fire && !readBusCmd.write)

  readBusCmd.setIdle()
  readBusCmd.write := False
  readBusCmd.address := ((read_counter.value).resize(readBusCmd.address.getBitsWidth bits) + baseAddress)

  var fifo = StreamFifo(midDatatype, bufferSizeInMidWords, latency = 0)
  StreamTools.AdaptWidth(fifo.io.pop, Bits(dataType.getBitsWidth bits)).map(_.as(dataType)) <> io.pop

  val usage = new CounterUpDownUneven(bufferSizeInBits, config.dataWidth, midDatatype.getBitsWidth)
  when(fifo.io.pop.fire) { usage.decrement() }
  when(readBusCmd.fire) { usage.increment() }

  val usageIsMax = usage.isMax

  io.memoryRead := readBusCmd.fire

  val readTrigger =
    if(readTriggerInBits >= 0) {
      RegInit(True) setWhen(usage <= readTriggerInBits) clearWhen(io.pop.fire)
    } else {
      True
    }

  when(~usageIsMax) {
    readBusCmd.valid := io.memoryValid && readTrigger
  }

  val overflow = Bool()

  val read_port_unpack = Flow(midDatatype)
  StreamTools.AdaptWidth(io.bus.rsp.map(_.data.asBits), read_port_unpack)
  read_port_unpack.toStream(overflow) <> fifo.io.push

  assert(~overflow, "PMBBuffer overflowed")
}

case class PipelinedMemoryBusFIFO[T <: Data](dataType : HardType[T],
                                             sm : SizeMapping,
                                             sysBus : Option[PipelineMemoryGlobalBus] = None, localPushDepth : Int = 0, localPopDepth : Int = 0) extends Component {
  val depthInWords = sm.size.toInt
  val baseAddress = sm.base.toInt

  val config = sysBus.map(g => g.config).getOrElse(PipelinedMemoryBusConfig(addressWidth = log2Up(depthInWords), dataWidth = dataType.getBitsWidth))
  val readBus = sysBus.get.add_master(s"pmb_fifo_read_${baseAddress.hexString()}")
  val bus = sysBus.map(g => g.add_master(s"pmb_fifo_write_${baseAddress.hexString()}")).getOrElse(master(PipelinedMemoryBus(config)))

  val io = new Bundle {
    val push = slave Stream(dataType)
    val pop = master Stream(dataType)
    val empty = out Bool()

    val flush = in Bool() default(False)
    val occupancy    = out UInt (log2Up(depthInWords + 1) bits)
    val availability = out UInt (log2Up(depthInWords + 1) bits)
  }

  test_funcs.assertStreamContract(io.push)
  test_funcs.assertStreamContract(io.pop)

  val (writeBus, sharedBus) = sysBus.map((bus, _)).getOrElse(
    {
      val sysBus = new PipelineMemoryGlobalBus(config)
      bus <> sysBus.add_slave("data_store", DefaultMapping)
      (sysBus.add_master(s"pmb_fifo_write_${baseAddress.hexString()}"), sysBus)
    }
  )

  test_funcs.assertStreamContract(writeBus.cmd)

  val ramBackedBuffer = PipelinedMemoryBusBuffer(dataType, depthInWords, baseAddress, sharedBus.config, rsp_latency = localPopDepth)
  ramBackedBuffer.io.bus <> readBus

  val inFlight = new CounterUpDown(depthInWords, handleOverflow = false)
  when(ramBackedBuffer.io.pop.fire) {
    inFlight.decrement()
  }

  val occupancy, ramOccupancy = new CounterUpDown(depthInWords, handleOverflow = false)
  when(writeBus.cmd.fire) {
    occupancy.increment()
    ramOccupancy.increment()
  }
  io.occupancy := occupancy.resized
  io.availability := depthInWords - occupancy.value

  ramBackedBuffer.io.pop <> io.pop
  ramBackedBuffer.io.memoryValid := ramOccupancy > 0
  when(io.pop.fire) {
    occupancy.decrement()
  }

  when(ramBackedBuffer.io.memoryRead) {
    inFlight.increment()
    ramOccupancy.decrement()
  }

  val write_counter = Counter(depthInWords, writeBus.cmd.fire)

  val isFlushing = RegInit(False) setWhen(io.flush)

  val full = occupancy.value === (depthInWords - 1)
  val push = if(localPushDepth > 0) io.push.queue(localPushDepth) else io.push
  push.map(wrd => {
    val cmd = PipelinedMemoryBusCmd(writeBus.config)
    cmd.mask.setAll()
    cmd.write := True
    cmd.address := (write_counter.value).resize(cmd.address.getBitsWidth bits) + baseAddress
    cmd.data := wrd.asBits
    cmd
  }).haltWhen(full | isFlushing) >> writeBus.cmd

  val empty = occupancy === 0 // && readFifo.io.occupancy === 0
  io.empty := empty

  when(isFlushing) {
    occupancy.clearAll()
    isFlushing := inFlight.value === 0
    io.pop.valid := False
    ramBackedBuffer.io.pop.freeRun()
  }

  def attach_bus(busSlaveFactory: BusIf): Unit = {
    for(v : Data <- Seq(io.occupancy, occupancy.value, inFlight.value, full)) {
      val reg = busSlaveFactory.newReg(f"${v.name} ${name}")(SymbolName(v.name))
      val field = reg.field(Bits(32 bits), RO, s"${name}_occ")
      field := v.asBits.resized
    }

    RegisterTools.Counter(busSlaveFactory, "total_stall", io.push.isStall)
    RegisterTools.Counter(busSlaveFactory, "total_push", io.push.fire)
    RegisterTools.Counter(busSlaveFactory, "total_pop", io.push.fire)

    val highwater_reg = busSlaveFactory.newReg(f"highwater ${name}")
    val highwater = highwater_reg.field(cloneOf(io.occupancy), RW, s"${name}_highwater")
    when(RegNext(io.occupancy > highwater)) {
      highwater := io.occupancy
    }

  }
}
