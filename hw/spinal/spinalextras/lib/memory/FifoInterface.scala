package spinalextras.lib.memory

import spinal.core._
import spinal.lib.{IMasterSlave, Stream, master, slave}
import spinalextras.lib.formal.{FormalMasterSlave, FormalProperties}
import spinalextras.lib.formal.StreamFormal.StreamExt

case class FifoInterface[T <: Data](dataType: HardType[T], depth: BigInt) extends Bundle with IMasterSlave with FormalMasterSlave {
  val push = Stream(dataType)
  val pop = Stream(dataType)

  val flush = Bool() default (False)
  val occupancy = UInt(log2Up(depth + 1) bits)
  val availability = UInt(log2Up(depth + 1) bits)

  override def asMaster(): Unit = {
    master(push)
    slave(pop)
    in(occupancy, availability)
    out(flush)
  }

  override def formalIsProducerValid() = push.formalIsProducerValid() ++ pop.formalIsConsumerValid() ++ new FormalProperties {
    when(flush) {
      addFormalProperty(pop.ready, "FIFO interface wants pop ready to be true when flushing")
    }
  }.implicitValue

  override def formalIsConsumerValid() = push.formalIsConsumerValid() ++ pop.formalIsProducerValid()
}
