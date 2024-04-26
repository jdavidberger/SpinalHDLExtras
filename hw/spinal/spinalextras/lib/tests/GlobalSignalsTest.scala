package spinalextras.lib.tests

import spinal.core._
import spinal.lib._
import spinalextras.lib.Config
import spinalextras.lib.misc.GlobalSignals

class ComponentA extends Component {
  val test_out_bool = out(Bool())
  GlobalSignals.externalize(test_out_bool)

  test_out_bool := False
}

class ComponentB extends Component {
  val test_stream_bool = master(Stream(Bool()))
  GlobalSignals.externalize(test_stream_bool)

  test_stream_bool.payload := True
  test_stream_bool.valid := True
}

class ComponentC extends Component {
  val cA = new ComponentA()
  val cB = new ComponentB()

  val test_slave_stream_bool = slave(Stream(Bool()))
  GlobalSignals.externalize(test_slave_stream_bool)

  test_slave_stream_bool.ready := True
}

class ComponentD extends Component {
  val cC = new ComponentC()
}

object GlobalSignalsTest extends App {
  Config.spinal.generateVerilog(
    new ComponentD()
  )
  Config.spinal.generateVerilog(
    new ComponentA()
  )
}

