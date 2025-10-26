package spinalextras.lib.soc.peripherals

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3SlaveFactory}
import spinalextras.lib.soc.{CSREventManager, EventSourceProcess}

import scala.language.postfixOps

class SpinexApb3Timer(val baseAddress: BigInt) extends Component {
  val io = new Bundle {
    val apb = slave(Apb3(
      addressWidth = 8,
      dataWidth = 32
    ))
    val interrupt = out Bool()
  }

  val busCtrl = Apb3SlaveFactory(io.apb)
  val busCtrlWrapped = busCtrl

  val load_value = busCtrlWrapped.createReadAndWrite(UInt(32 bits), address = 0, documentation = "load value") init (0)
  val reload_value = busCtrlWrapped.createReadAndWrite(UInt(32 bits), address = 4, documentation = "reload value") init (0)
  val en = busCtrlWrapped.createWriteOnly(Bool(), address = 8, documentation = "enable") init (False)

  val update_value = busCtrlWrapped.createAndDriveFlow(Bool(), address = 12, documentation = "update value")
  val update = busCtrlWrapped.createReadOnly(UInt(32 bits), address = 16, documentation = "update") init (0)

  val counter_value = Reg(UInt(32 bits)) init (0)
  val counter_is_zero = counter_value === 0

  when(update_value.valid) {
    update := counter_value
  }

  when(en) {
    when(counter_is_zero) {
      counter_value := reload_value
    } otherwise {
      counter_value := counter_value - 1
    }
  } otherwise {
    counter_value := load_value
  }

  val uptime_latch = Bool()
  uptime_latch := False
  busCtrlWrapped.onWrite(address = 32, documentation = "uptime latch") {
    uptime_latch := True
  }
  val uptime = CounterFreeRun(64 bits)
  val uptime_cycles = RegNextWhen(uptime.value, uptime_latch) init (0)
  busCtrlWrapped.createReadMultiWord(uptime_cycles, address = 36, documentation = "Uptime cycles") := EndiannessSwap(uptime_cycles)

  io.interrupt := CSREventManager(busCtrl = busCtrlWrapped, addr = 20,
    new EventSourceProcess(counter_is_zero, description = "counter is zero")
  )

  busCtrl.printDataModel()
}
