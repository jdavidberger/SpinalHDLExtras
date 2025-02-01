package spinalextras.lib.soc

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory

import scala.collection.mutable

case class EventSource(name : String, description : String = "") {
  val status, pending, enable, irq = Bool()
}

class EventSourceProcess(trigger : Bool, description : String = "", edge : String = "rising") extends EventSource(trigger.name, description) {
  status := trigger
  pending.setAsReg() init(False)
  enable.setAsReg() init(False)

  val trigger_case = if(edge == "falling") trigger.fall(False) else trigger.rise(False)
  when(trigger_case) {
    pending := True
  }

  irq := enable & pending
}


class CSREventManager extends Area {
  val sources = new mutable.ArrayBuffer[EventSource]()
  def add(source : EventSource): Unit = {
    sources += source
  }

  def generateIRQ(busCtrl : BusSlaveFactory, addr : BigInt = 0) = new Composite(this) {
    val statusAddr = 0x00 + addr
    val pendingAddr = 0x04 + addr
    val enableAddr = 0x08 + addr
    val interrupt = Vec(sources.zipWithIndex.map{ case (s, idx) => {
      busCtrl.read(s.status.setName(s"${s.name}_status"), address = statusAddr, bitOffset = idx, documentation = s.description)
      busCtrl.readAndClearOnSet(s.pending.setName(s"${s.name}_pending"), address = pendingAddr, bitOffset = idx)
      busCtrl.readAndWrite(s.enable.setName(s"${s.name}_enable"), enableAddr, bitOffset = idx)
      s.irq
    }}).orR
  }.interrupt
}

object CSREventManager {
  def apply(busCtrl : BusSlaveFactory, addr : BigInt, sources : EventSource*): Bool = {
    val eventManager = new CSREventManager()
    sources.foreach(eventManager.add)
    eventManager.generateIRQ(busCtrl = busCtrl, addr = addr)
  }
  def apply(busCtrl : BusSlaveFactory, sources : EventSource*): Bool = {
    apply(busCtrl = busCtrl, addr = 0, sources:_*)
  }
}