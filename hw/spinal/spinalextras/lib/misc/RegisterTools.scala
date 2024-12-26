package spinalextras.lib.misc

import spinal.core._
import spinal.lib.StreamCCByToggle
import spinal.lib.bus.regif.AccessType.{RO, RW}
import spinal.lib.bus.regif.{BusIf, SymbolName}

import scala.language.postfixOps

object RegisterTools {
  def inc_addr(b : BusIf): BusIf = {
    val r = (b.getRegPtr() & 0xFFFFFF00L) + 0x100L
    b.regPtrReAnchorAt(r)
    b
  }

  def ReadOnly[T <: Data](busIf : BusIf, name : String, value: T): Unit = {
    val reg = busIf.newReg(name)(SymbolName(name))
    val field = reg.field(Bits(32 bits), RO)
    field := value.asBits.resized
  }

  def Register[T <: Data](busIf : BusIf, name : String, initValue: T): T = {
    val reg = busIf.newReg(name)(SymbolName(name))
    val field = reg.field(Bits(initValue.getBitsWidth bits), RW) init(initValue.asBits)

    val value = cloneOf(initValue)
    value.assignFromBits(field.asBits)
    value
  }

  def create_counter(reg: UInt, cond : Bool, clockDomain: ClockDomain): Unit = {
    if(reg.clockDomain != clockDomain) {
      val r = new ClockingArea(clockDomain) {
        val r = Reg(cloneOf(reg)) init(0)
        when(cond) {
          r := r + 1
        }
      }.r

      val cc = StreamCCByToggle(cloneOf(r), clockDomain, reg.clockDomain)
      cc.io.input.payload := r
      cc.io.input.valid := True
      cc.io.output.ready := True

      reg.setAsReg() init(0)
      when(cc.io.output.fire) {
        reg := cc.io.output.payload
      }
    } else {
      reg.setAsReg() init(0)
      when(cond) {
        reg := reg + 1
      }
    }
  }

  def Counter(busIf : BusIf, name : String, event : Bool, clockDomain: ClockDomain = ClockDomain.current) = {
    val reg = busIf.newReg(name)(SymbolName(name))
    val field = reg.field(UInt(32 bits), if (ClockDomain.current == clockDomain) RW else RO)
    create_counter(field, event, clockDomain)
    field
  }

  def newSection(b : BusIf) = {
    val r = (b.getRegPtr() & 0xFFFFFF00L) + 0x100L
    b.regPtrReAnchorAt(r)
    b
  }
}