package spinalextras.lib.bus.simple

import spinal.core.UInt
import spinal.lib.bus.misc.{BusSlaveFactoryDelayed, BusSlaveFactoryElement}

//
//class PipelineMemoryBusSlaveFactory(bus: PipelinedMemoryBus, reg_fedback: Boolean = true) extends BusSlaveFactoryDelayed{
//  bus.rsp.setIdle()
//
//  val busInterface = PipelinedMemoryBusInterface(bus)
//
//  override def build(): Unit = {
//    super.doNonStopWrite(bus.cmd.data)
//
//    def doMappedElements(jobs : Seq[BusSlaveFactoryElement]) = super.doMappedElements(
//      jobs = jobs,
//      askWrite = askWrite,
//      askRead = askRead,
//      doWrite = doWrite,
//      doRead = doRead,
//      writeData = bus.DAT_MOSI,
//      readData = bus.DAT_MISO
//    )
//
//    switch(byteAddress) {
//      for ((address, jobs) <- elementsPerAddress if address.isInstanceOf[SingleMapping]) {
//        is(address.asInstanceOf[SingleMapping].address) {
//          doMappedElements(jobs)
//        }
//      }
//    }
//
//    for ((address, jobs) <- elementsPerAddress if !address.isInstanceOf[SingleMapping]) {
//      when(address.hit(byteAddress)){
//        doMappedElements(jobs)
//      }
//    }
//  }
//
//  override def busDataWidth: Int = busInterface.busDataWidth
//
//  override def readHalt(): Unit = busInterface.readHalt()
//
//  override def writeHalt(): Unit = busInterface.writeHalt()
//
//  override def readAddress(): UInt = busInterface.readAddress()
//
//  override def writeAddress(): UInt = busInterface.writeAddress()
//}