package spinalextras.lib.memory

import spinal.core._
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusConfig}
import spinal.lib._
import spinal.lib.bus.regif.BusIf

import spinalextras.lib.bus.{PipelinedMemoryBusCmdExt, PipelinedMemoryBusConfigExt}
import spinalextras.lib.misc.{GlobalSignals, RegisterTools, StreamTools}
import spinalextras.lib.testing.test_funcs

import scala.language.postfixOps

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
    val debug_fake_write = in(Bool()) default(False)
  }

  test_funcs.assertStreamContract(io.push)
  test_funcs.assertPMBContract(io.bus)

  val pushMem = Stream(Fragment(Bits(busConfig.dataWidth bits)))
  require(baseAddress % (1 << busConfig.wordAddressShift) == 0)
  val baseAddressUInt = U(baseAddress >> busConfig.wordAddressShift, busConfig.addressWidth bits)
  val writeAddress = RegNext(counter.valueNext + baseAddressUInt, init = baseAddressUInt)

  StreamTools.AdaptFragmentWidth(io.push.map(x => {
    val f = Fragment(Bits(x.fragment.getBitsWidth bits))
    f.fragment := x.fragment.asBits
    f.last := x.last
    f
  }), pushMem)

  assert(io.bus.cmd.address.getBitsWidth >= writeAddress.getBitsWidth)

  pushMem.map(d => {
    val cmd = cloneOf(io.bus.cmd.payload)
    cmd.assignWordAddress(writeAddress.resized)
    cmd.data := d
    when(io.debug_fake_write) {
      cmd.mask.clearAll()
    } otherwise {
      cmd.mask.setAll()
    }
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

    val debug_fake_write = GlobalSignals.externalize(io.debug_fake_write)
    debug_fake_write := RegisterTools.Register(busSlaveFactory, "s2b_debug_ctrl", False)
  }
}
