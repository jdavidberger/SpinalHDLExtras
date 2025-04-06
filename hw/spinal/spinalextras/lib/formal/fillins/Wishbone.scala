package spinalextras.lib.formal.fillins

import spinal.core._
import spinal.lib._
import spinal.lib.bus.wishbone.Wishbone
import spinalextras.lib.formal.{FormalMasterSlave, FormalProperty, fillins}

object Wishbone {
  implicit class WishboneFormalExt(val bus : Wishbone) extends FormalMasterSlave {

    /**
     * @return True if and only if the driving signals are valid
     */
    override def formalIsProducerValid(): Seq[FormalProperty] = ???

    override def asIMasterSlave: IMasterSlave = bus
  }
}