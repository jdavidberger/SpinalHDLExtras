package spinalextras.lib

import spinal.core._
import spinal.lib.bus.wishbone.Wishbone

package object bus {
  implicit class WishboneExt(bus: Wishbone) {
    def isCycle = bus.CYC
    def STB = bus.STB
    def STALL = bus.STALL
    def ACK = bus.ACK
    def WE = bus.WE

    def config = bus.config

    def masterHasRequest = isCycle && STB
    private def slaveRequestAck = if(config.isPipelined) !STALL else ACK
    def isAcceptingRequests = if(config.isPipelined) !STALL else True

    @deprecated("This status check doesn't map pipelined modes correctly, prefer isRequestStalled")
    def isStall : Bool    = if(config.isPipelined)  isCycle && STALL
    else                    False

    def isRequestStalled  = masterHasRequest && !slaveRequestAck

    @deprecated("This status check is ambiguous and may be removed in the future, prefer isRequestAck or isResponse")
    def isAck      = isRequestAck
    def isRequestAck      = masterHasRequest && slaveRequestAck
    def isResponse        = if(config.isPipelined) isCycle && ACK else masterHasRequest && ACK

    @deprecated("This status check doesn't map pipelined modes correctly, prefer masterHasRequest or isRequestAck " +
      "depending on whether you want to check if a request exists or if one was acknowledged")
    def isTransfer : Bool = if(config.isPipelined)  isCycle && STB && !STALL
    else                    isCycle && STB
    def isWrite : Bool    = masterHasRequest &&  WE
    def isRead : Bool     = masterHasRequest && !WE
    def doSend  : Bool    = masterHasRequest && isRequestAck
    def doWrite : Bool    = doSend &&  WE
    def doRead  : Bool    = doSend && !WE

    def wordAddress() : UInt = bus.ADR
    def assignWordAddress(addr : UInt) = bus.ADR := addr

  }
}
