package spinalextras.lib.bus.general

import spinal.core._
import spinal.lib._

import scala.collection.mutable

object GeneralBusInterface {
  val outstandingRspRegisters = new mutable.WeakHashMap[Any, UInt]()
}

trait GeneralBusInterface[BUS <: Data with IMasterSlave] {
  type CMD <: Data
  type RSP <: Data

  val dataType : HardType[BUS]
  def decodeMissTarget() : BUS = ???
  def address(cmd : CMD) : UInt
  def bus_address(bus : BUS) : UInt = address(cmd(bus).payload)

  def rsp_needed(bus : BUS) : Bool = rsp_needed(cmd(bus))
  def rsp_needed(cmd : Stream[CMD]) : Bool = cmd_requires_response(cmd.payload) && cmd.fire
  def bus_cmd_requires_response(bus : BUS) : Bool = cmd_requires_response(cmd(bus).payload)
  def cmd_requires_response(cmd : CMD) : Bool
  def rsp_required_count(bus : BUS) : UInt
  def rsp_fire(bus : BUS) : Bool
  def rsp_valid(bus : BUS) : Bool = rsp_fire(bus)

  def cmd(bus : BUS) : Stream[CMD]
  //  def cmdTrait : IMemoryBusCmd[CMD]
  //  def rsp : IMemoryBusRsp[RSP]

  def rsp_payload(input : BUS) : RSP
  def map_cmd(input: Stream[CMD], output : Stream[CMD], takeWhen : Bool):Stream[CMD] = input << output.takeWhen(takeWhen)
  def map_cmd(input: BUS, output : BUS, takeWhen : Bool) : Stream[CMD] = map_cmd(cmd(input), cmd(output), takeWhen)

  def map_rsp(input : BUS, output : Stream[RSP], decodeNoHit : Bool)
  def map_rsp_read_error(input : BUS) : Unit

  def map_rsp(input : BUS, output : BUS)
  def set_rsp_idle(input : BUS) : Unit = {}
  def set_rsp_blocked(input : BUS) : Unit = {}

  def formalRspPending(input : BUS) : UInt = formalContract(input).outstandingRsp

  def formalContract(input : BUS) = new Composite(input, "formalContract") {
    val toAdd, toRemove = UInt(32 bits)
    toAdd := 0
    toRemove := 0
    when(cmd(input).fire) {
      toAdd := rsp_required_count(input).resized
    }
    when(rsp_fire(input)) {
      toRemove := 1
    }
    val delta = toAdd.intoSInt - toRemove.intoSInt

    val outstandingRsp =  GeneralBusInterface.outstandingRspRegisters.getOrElseUpdate(input, {
      val outstandingRsp = Reg(UInt(32 bits)) init(0)
      val nextOutstandingRsp = (outstandingRsp.intoSInt +^ delta).asUInt
      assume(nextOutstandingRsp <= outstandingRsp.maxValue)
      outstandingRsp := nextOutstandingRsp.resized
      outstandingRsp
    })

    val wasStall = RegNext(cmd(input).isStall) init(False)
    val canRspBeValid = outstandingRsp =/= 0 || ((toAdd > 0) && wasStall)
    assume((outstandingRsp.intoSInt +^ delta) < outstandingRsp.maxValue)
  }

  def isProducerValid(bus: BUS): Bool = {
    cmd(bus).formalIsValid()
  }

  def isConsumerValid(bus: BUS): Bool = formalContract(bus).canRspBeValid || !rsp_valid(bus)

  case class MapRsp(bus : BUS) {
    def setBlocked() = set_rsp_blocked(bus)
    def setIdle() = set_rsp_idle(bus)
    def connect(that : MapRsp) = map_rsp(bus, that.bus)
    def readError() : Unit = map_rsp_read_error(bus)
    def payload = rsp_payload(bus)
    def fire = rsp_fire(bus)
  }

  val self = GeneralBusInterface.this
  implicit class MapBus(bus : BUS) {
    def cmd = GeneralBusInterface.this.cmd(bus)
    def readRequestFire = rsp_needed(bus)
    def readRequestValid = cmd_requires_response(cmd.payload) && bus.cmd.valid
    def rspFired = rsp_fire(bus)
    def rspsRequired = rsp_required_count(bus)
    def isProducerValid = self.isProducerValid(bus)
    def isConsumerValid = self.isConsumerValid(bus)
    def formalRspPending = self.formalRspPending(bus)

    def byteaAddress = self.bus_address(bus)
    val rsp = MapRsp(bus)
  }
}
