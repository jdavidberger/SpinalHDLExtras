package spinalextras.lib.bus.simple

import spinal.core._
import spinal.lib.bus.misc._
import spinal.lib.bus.regif.{BusIf, ClassName}
import spinal.lib.bus.simple.PipelinedMemoryBus


case class PipelinedMemoryBusInterface(bus: PipelinedMemoryBus, sizeMap: SizeMapping, regPre: String = "", withSecFireWall: Boolean = false)(implicit moduleName: ClassName) extends BusIf{

  override val askWrite: Bool = bus.cmd.write && bus.cmd.valid
  override val askRead: Bool = ~bus.cmd.write && bus.cmd.valid
  override val doWrite: Bool = askWrite && bus.cmd.fire
  override val doRead: Bool = askRead && bus.cmd.fire
  override val readData: Bits = Bits(bus.config.dataWidth bits)

  override val writeData: Bits = bus.cmd.data
  override val readError: Bool = False
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
  bus.rsp.payload.data := readData
  bus.rsp.valid := RegNext(doRead) init(False)

  override def readHalt(): Unit = halted := True

  override def writeHalt(): Unit = halted := True

  override def busDataWidth: Int = bus.config.dataWidth

//  override type B = this.type

  override def getModuleName = moduleName.name
}