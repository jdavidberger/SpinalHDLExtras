package spinalextras.lib.tests

import spinal.core._
import spinal.core.in.Bool
import spinal.lib._
import spinalextras.lib.Config
import spinalextras.lib.misc.AutoInterconnect

object AutoInterconnectTest extends App {
  Config.spinal.generateVerilog(
    AutoInterconnect(
      "AutoInterconnectTest",
      () => Iterator(
        new Component {
          val io = new Bundle {
            val port = master(Stream(Bool()))
          }
          val counter = Counter(32)
          counter.increment()
          io.port.valid := counter > 10
          io.port.payload := counter.value.xorR
        }.setName("A"),
        new Component {
          val io = new Bundle {
            val port = slave(Stream(Bool()))
            val portOut = out(Bool())
          }
          io.port.ready := True

          io.portOut := io.port.valid && io.port.payload
        }.setName("B")
      )
    )
  )
}