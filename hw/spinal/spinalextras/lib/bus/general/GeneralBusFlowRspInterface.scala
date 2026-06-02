package spinalextras.lib.bus.general

import spinal.core.{Bool, Data, Nameable}
import spinal.lib.{Flow, IMasterSlave, Stream}

abstract class GeneralBusFlowRspInterface[BUS <: Data with IMasterSlave with Nameable] extends GeneralBusInterface[BUS] {
  def RspStream(b : BUS) : Flow[RSP]

  override def rsp_fire(bus: BUS): Bool = RspStream(bus).valid

  override def rsp_payload(input: BUS): RSP = RspStream(input).payload

  override def map_rsp(input: BUS, output: Stream[RSP]): Unit = RspStream(input) << output.toFlow

  override def map_rsp(input: BUS, output: BUS): Unit = {
    RspStream(input) << RspStream(output)
  }
}

abstract class GeneralBusStreamRspInterface[BUS <: Data with IMasterSlave with Nameable] extends GeneralBusInterface[BUS] {
  def RspStream(b : BUS) : Stream[RSP]

  override def rsp_fire(bus: BUS): Bool = RspStream(bus).valid

  override def rsp_payload(input: BUS): RSP = RspStream(input).payload

  override def map_rsp(input: BUS, output: Stream[RSP]): Unit = RspStream(input) << output

  override def map_rsp(input: BUS, output: BUS): Unit = {
    RspStream(input) << RspStream(output)
  }

  override def set_rsp_ready(input: BUS, v: Bool): Unit = RspStream(input).ready := v

  override def get_rsp_ready(bus: BUS): Bool = RspStream(bus).ready
}
