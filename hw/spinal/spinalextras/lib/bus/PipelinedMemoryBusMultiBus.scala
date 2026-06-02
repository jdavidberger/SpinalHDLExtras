package spinalextras.lib.bus

import spinal.core.{Bool, Composite}
import spinal.lib.bus.misc.AddressMapping
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusArbiter, PipelinedMemoryBusConnectors, PipelinedMemoryBusDecoder}
import spinal.lib.traversableOnceBoolPimped
import spinalextras.lib.formal.fillins.PipelinedMemoryBusFormal.PipelinedMemoryBusFormalExt

case class PipelinedMemoryBusMultiBus(bus: PipelinedMemoryBus,
                                      pendingMax: Int = 1,
                                      pendingRspMax: Int = 1, rspRouteQueue: Boolean = false, transactionLock: Boolean = true) extends MultiBusInterface {
  override def address_width = bus.config.addressWidth

  override def create_decoder(mappings: Seq[AddressMapping]) = new Composite(bus, "") {
    val decoder = new PipelinedMemoryBusDecoder(bus.config, mappings, pendingMax)
    decoder.io.input <> bus
    val outputs = decoder.io.outputs.map(outputBus => copy(bus = outputBus))
  }.outputs

  override def create_arbiter(size: Int) = new Composite(bus, "") {
    val arbiter = new PipelinedMemoryBusArbiter(bus.config, size, pendingRspMax, rspRouteQueue, transactionLock)
    arbiter.io.output <> bus
    val inputs = arbiter.io.inputs.map(inputBus => copy(bus = inputBus))
  }.inputs

  override def isValidProducer: Bool = PipelinedMemoryBusFormalExt(bus).formalIsProducerValid().map(_.condition).andR

  override def isValidConsumer: Bool = PipelinedMemoryBusFormalExt(bus).formalIsConsumerValid().map(_.condition).andR
}

object PipelinedMemoryBusMultiBus {
  MultiInterconnectConnectFactory.AddHandler { case (m: PipelinedMemoryBusMultiBus, s: PipelinedMemoryBusMultiBus) => PipelinedMemoryBusConnectors.direct(m.bus, s.bus) }
}