package spinalextras.lib.bus.general

import spinal.core._
import spinal.lib._
import spinal.lib.formal.ComponentWithFormalAsserts

class GeneralBusTimeout[T <: Data with IMasterSlave](val busAccesor: GeneralBusInterface[T], val timeout_cycles : Int, val pendingMax : Int = 3) extends ComponentWithFormalAsserts {
  import busAccesor._
  def dataType = busAccesor.dataType

  val io = new Bundle {
    val input = slave(dataType)
    val output = master(dataType)
  }

  val rspRequired = io.input.rspsRequired
  val rspPendingCounter = new ResponseCounter(rspRequired.getWidth, pendingMax + 1)
  rspPendingCounter.io.increaseBy.payload := rspRequired - 1
  val latchFirstValid = RegInit(True) clearWhen(rspPendingCounter.io.increaseBy.fire && !io.input.readRequestFire) setWhen(io.input.readRequestFire)
  rspPendingCounter.io.increaseBy.valid := io.input.readRequestValid && latchFirstValid
  rspPendingCounter.io.decrease.ready := io.input.rspFired
  val rspPending = CombInit(rspPendingCounter.io.decrease.valid)

  val cmdWait = (io.input.cmd.valid && rspPending) || (!rspPendingCounter.io.increaseBy.ready && latchFirstValid)

  io.input.cmd <> io.output.cmd

  when(cmdWait) {
    io.input.cmd.ready := False
    rspPendingCounter.io.increaseBy.valid := False
    latchFirstValid := True
    io.output.cmd.valid := False
  }

  val timeout = new Timeout(timeout_cycles)
  when(!rspPending || io.input.rspFired) {
    timeout.clear()
  }

  map_rsp(io.input, io.output)
  when(timeout) {
    io.input.rsp.setBlocked()
  }


}