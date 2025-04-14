package spinalextras.lib.bus.simple

import spinal.core._
import spinal.lib.bus.misc._
import spinal.lib.bus.regif.{BusIf, ClassName}
import spinal.lib.bus.simple.PipelinedMemoryBus

import scala.language.postfixOps


case class PipelinedMemoryBusInterface(bus: PipelinedMemoryBus, sizeMap: SizeMapping, regPre: String = "", withSecFireWall: Boolean = false)(implicit moduleName: ClassName) extends BusIf {

  override val askWrite: Bool = bus.cmd.write && bus.cmd.valid
  override val askRead: Bool = ~bus.cmd.write && bus.cmd.valid
  override val doWrite: Bool = askWrite && bus.cmd.fire
  override val doRead: Bool = askRead && bus.cmd.fire

  override val busDataWidth: Int = bus.config.dataWidth

  lazy val reg_wrerr: Bool = Reg(Bool(), init = False)
  val bus_rdata: Bits  = Bits(busDataWidth bits)
  val reg_rderr: Bool = False
  val reg_rdata: Bits = Bits(busDataWidth bits)

  override val writeData: Bits = bus.cmd.data

  override val withStrb: Boolean = bus.cmd.mask != null


  val wstrb: Bits  = bus.cmd.mask
  val wmask: Bits  = withStrb generate (Bits(busDataWidth bit))
  val wmaskn: Bits = withStrb generate (Bits(busDataWidth bit))
  initStrbMasks()

  override def readAddress(): UInt = bus.cmd.address

  override def writeAddress(): UInt = bus.cmd.address

  val halted = Bool()
  halted := False

  bus.cmd.ready := !halted && bus.cmd.valid
  bus.rsp.payload.data := bus_rdata
  bus.rsp.valid := RegNext(doRead) init(False)

  override def readHalt(): Unit = halted := True

  override def writeHalt(): Unit = halted := True

  //override def busDataWidth: Int = bus.config.dataWidth

//  override type B = this.type

  override def getModuleName = moduleName.name

  override  val busAddrWidth: Int = bus.config.addressWidth
  override lazy val readData: Bits = bus.rsp.data
  override lazy val readError: Bool = False
}