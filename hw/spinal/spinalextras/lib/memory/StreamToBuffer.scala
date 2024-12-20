package spinalextras.lib.memory

import spinal.core.{Bits, Bundle, ClockDomain, ClockingArea, CombInit, Component, Data, HIGH, HardType, IntToBuilder, RegNext, ResetArea, SYNC, True, U, cloneOf, log2Up, out, when}
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusConfig}
import spinal.lib._
import spinal.lib.bus.regif.BusIf
import spinalextras.lib.misc.{RegisterTools, StreamTools}
import spinalextras.lib.testing.test_funcs

case class StreamToBuffer[T <: Data](
    dataType: HardType[T],
    depth: Int,
    baseAddress: BigInt,
    busConfig: PipelinedMemoryBusConfig = PipelinedMemoryBusConfig(32, 32),
) extends Component {
  val depthInBusWords = depth * dataType.getBitsWidth / busConfig.dataWidth
  require((depth * dataType.getBitsWidth % busConfig.dataWidth) == 0)
  val counter = Counter(depthInBusWords)

  val io = new Bundle {
    val push = slave Stream (Fragment(dataType))
    val last = out Bool ()

    val bus = master(PipelinedMemoryBus(busConfig))
  }

  test_funcs.assertStreamContract(io.push)
  test_funcs.assertPMBContract(io.bus)

  val pushMem = Stream(Fragment(Bits(busConfig.dataWidth bits)))
  val writeAddress = RegNext((counter.valueNext) + U(baseAddress, busConfig.addressWidth bits))

  StreamTools.AdaptFragmentWidth(io.push.map(x => {
    val f = Fragment(Bits(x.fragment.getBitsWidth bits))
    f.fragment := x.fragment.asBits
    f.last := x.last
    f
  }), pushMem)

  assert(io.bus.cmd.address.getBitsWidth >= writeAddress.getBitsWidth)

  pushMem.map(d => {
    val cmd = cloneOf(io.bus.cmd.payload)
    cmd.address := writeAddress.resized
    cmd.data := d
    cmd.mask.setAll()
    cmd.write := True
    cmd
  }) <> io.bus.cmd

  when(pushMem.fire) {
    counter.increment()
  }
  when(pushMem.lastFire) {
    counter.clear()
  }
  io.last := counter.willOverflowIfInc && pushMem.lastFire

  def attach_bus(busSlaveFactory: BusIf): Unit = {
    RegisterTools.Counter(busSlaveFactory, "s2b_pushes", io.push.fire)
    RegisterTools.Counter(busSlaveFactory, "s2b_lasts", io.last.rise())
    RegisterTools.ReadOnly(busSlaveFactory, "s2b_writeAddress", writeAddress)

  }
}
