package spinalextras.lib.memory

import spinal.core.{Bits, Bool, Bundle, Data, False, HardType, IntToBuilder, UInt, in, log2Up, out}
import spinal.lib.{IMasterSlave, Stream, master, slave}
import spinalextras.lib.formal.FormalMasterSlave
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

  override def formalIsProducerValid() = push.formalIsProducerValid() ++ pop.formalIsConsumerValid()
  override def formalIsConsumerValid() = push.formalIsConsumerValid() ++ pop.formalIsProducerValid()
}
