package spinalextras.lib.bus

import spinal.core.{Bits, Bool, False, HardType, Mux, Reg, RegNext, True, UInt, cloneOf, when}
import spinal.lib.{Fragment, Stream}
import spinal.lib.com.spi.ddr.SpiXdrMasterCtrl.{XipBus, XipBusParameters, XipCmd}
import spinalextras.lib.bus.general.GeneralBusInterface
import vexriscv.ip.{InstructionCacheConfig, InstructionCacheMemBus, InstructionCacheMemCmd, InstructionCacheMemRsp}
import vexriscv.plugin.{DBusSimpleBus, DBusSimpleCmd, DBusSimpleRsp}


package object general {

  object DSimpleBusInterfaceExtImpl {
    implicit def apply(bus: DBusSimpleBus) = new DSimpleBusInterfaceExtImpl(bus.bigEndian)
  }

  implicit class DSimpleBusInterfaceExtImpl(bigEndian : Boolean = false) extends GeneralBusInterface[DBusSimpleBus] {
    override  type CMD = DBusSimpleCmd
    override  type RSP = DBusSimpleRsp
    type BUS = DBusSimpleBus
    override  val dataType: HardType[DBusSimpleBus] = new BUS(bigEndian)
    override def cmd_requires_response(cmd:  CMD): Bool = !cmd.wr
    override def rsp_fire(bus:  DBusSimpleBus): Bool = bus.rsp.ready
    override def cmd(bus:  DBusSimpleBus): Stream[CMD] = bus.cmd
    override def map_rsp(input:  DBusSimpleBus, output:  Stream[RSP], decoderMiss : Bool): Unit = {
      output.ready := True
      input.rsp.ready := output.valid
      input.rsp.error := decoderMiss
      input.rsp.data := output.data
    }

    override def decodeMissTarget() = {
      val bus = DBusSimpleBus(bigEndian)
      bus.cmd.ready := True
      bus.rsp.ready := RegNext(bus.cmd.valid && !bus.cmd.wr) init(False)
      bus.rsp.error := True
      bus.rsp.data.assignDontCare()
      bus
    }

    override def map_rsp(input:  DBusSimpleBus, output: DBusSimpleBus): Unit = {
      input.rsp := output.rsp
    }
    override def address(cmd:  DBusSimpleCmd): UInt = cmd.address
    override def rsp_payload(bus:  DBusSimpleBus): DSimpleBusInterfaceExtImpl.this.RSP = bus.rsp

    override def rsp_required_count(bus : DBusSimpleBus) : UInt = Mux(bus.cmd.wr, 0, 1)

    //
  }

  object InstructionCacheMemBusInterfaceExtImpl {
    implicit def apply(bus: InstructionCacheMemBus) = new InstructionCacheMemBusInterfaceExtImpl(bus.p)
  }

  implicit class InstructionCacheMemBusInterfaceExtImpl(cfg: InstructionCacheConfig) extends GeneralBusInterface[InstructionCacheMemBus] {
    override  type CMD = InstructionCacheMemCmd
    override  type RSP = InstructionCacheMemRsp
    type BUS = InstructionCacheMemBus

    override  val dataType: HardType[InstructionCacheMemBus] = InstructionCacheMemBus(cfg)
    override def cmd_requires_response(cmd:  CMD): Bool = True
    override def rsp_fire(bus:  InstructionCacheMemBus): Bool = bus.rsp.fire
    override def cmd(bus:  InstructionCacheMemBus): Stream[CMD] = bus.cmd
    override def map_rsp(input:  InstructionCacheMemBus, output:  Stream[RSP], decoderMiss : Bool): Unit = {
      output.ready := True
      input.rsp.valid := output.valid
      input.rsp.payload := output.payload
    }

    override def map_rsp(input:  InstructionCacheMemBus, output: InstructionCacheMemBus): Unit = input.rsp << output.rsp
    override def address(cmd:  InstructionCacheMemCmd): UInt = cmd.address
    override def rsp_payload(bus:  InstructionCacheMemBus): InstructionCacheMemBusInterfaceExtImpl.this.RSP = bus.rsp.payload

    override def rsp_required_count(bus : InstructionCacheMemBus) : UInt = bus.p.burstSize

    override def decodeMissTarget() : BUS = {
      val bus = InstructionCacheMemBus(cfg)
      bus.cmd.ready := False
      bus.rsp.valid := False
      bus.rsp.error := True
      bus.rsp.data.assignDontCare()


      val rspRequired = rsp_required_count(bus)
      val counter = Reg(cloneOf(rspRequired)) init(0)
      when(counter === 0) {
        bus.cmd.ready := True
        when(bus.cmd.fire) {
          counter := rspRequired
        }
      }
      when(counter =/= 0) {
        bus.rsp.valid := True
        counter := counter - 1
      }

      bus
    }
  }

  implicit class XipBusMemBusInterfaceExtImpl(config: XipBusParameters) extends GeneralBusInterface[XipBus] {
    override type CMD = XipCmd
    override type RSP = Fragment[Bits]
    type BUS = XipBus

    override val dataType: HardType[BUS] = XipBus(config)

    override def cmd_requires_response(cmd: CMD): Bool = True

    override def rsp_fire(bus: BUS): Bool = bus.rsp.fire

    override def cmd(bus: BUS): Stream[CMD] = bus.cmd

    override def map_rsp(input: BUS, output: Stream[RSP], decoderMiss : Bool): Unit = {
      input.rsp << output
    }

    override def map_rsp(input: BUS, output: BUS): Unit = input.rsp << output.rsp

    override def set_rsp_idle(input: BUS) = input.rsp.setIdle()

    override def set_rsp_blocked(input: BUS): Unit = input.rsp.setBlocked()

    override def address(cmd: CMD): UInt = cmd.address

    override def rsp_payload(input: BUS) = input.rsp.payload

    override def rsp_required_count(bus: BUS): UInt = bus.cmd.length +^ 1

    override def formalRspPending(input: BUS) = formalContract(input).outstandingRsp

    //override def isConsumerValid(bus: BUS): Bool = XipBusMemBusExtImpl.isConsumerValid(bus)
  }
}
