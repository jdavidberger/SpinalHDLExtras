package spinalextras.lib.bus.general

import spinal.core.{Bool, Composite, MultiData}
import spinal.lib.IMasterSlave
import spinal.lib.bus.misc.AddressMapping
import spinalextras.lib.bus.MultiBusInterface

class GeneralBusMultiBus[T <: MultiData with IMasterSlave](bus_instance: T, val accessor: GeneralBusInterface[T]) extends MultiBusInterface {

  override def bus: T = bus_instance

  override def data_width: Int = accessor.data_width

  override def address_width: Int = accessor.bus_address(bus).getWidth

  override def create_decoder(mappings: Seq[AddressMapping]): Seq[MultiBusInterface] = new Composite(bus) {
    val decoder = new GeneralBusDecoder(accessor, mappings)
    decoder.setWeakName(s"${bus.name}_decoder")
    decoder.io.input <> bus
    val outputs = decoder.io.outputs.map(outputBus => new GeneralBusMultiBus(outputBus, accessor))
  }.outputs

  override def create_arbiter(size: Int): Seq[MultiBusInterface] = new Composite(bus) {
    val arbiter = new GeneralBusArbiter[T](accessor, size)
    arbiter.io.output <> bus
    val inputs = arbiter.io.inputs.map(inputBus => new GeneralBusMultiBus(inputBus, accessor))
  }.inputs

  override def isValidProducer: Bool = ???

  override def isValidConsumer: Bool = ???
}
