package spinalextras.lib

import spinal.core._
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusCmd, PipelinedMemoryBusConfig}
import spinal.lib.bus.wishbone.{AddressGranularity, Wishbone}

package object bus {
  implicit class WishboneExt(bus: Wishbone) {
    def isCycle = bus.CYC

    def STB = bus.STB

    def STALL = bus.STALL

    def ACK = bus.ACK

    def WE = bus.WE

    def config = bus.config

    def masterHasRequest = isCycle && STB

    private def slaveRequestAck = if (config.isPipelined) !STALL else ACK

    def isAcceptingRequests = if (config.isPipelined) !STALL else True

    @deprecated("This status check doesn't map pipelined modes correctly, prefer isRequestStalled")
    def isStall: Bool = if (config.isPipelined) isCycle && STALL
    else False

    def isRequestStalled = masterHasRequest && !slaveRequestAck

    @deprecated("This status check is ambiguous and may be removed in the future, prefer isRequestAck or isResponse")
    def isAck = isRequestAck

    def isRequestAck = masterHasRequest && slaveRequestAck

    def isResponse = if (config.isPipelined) isCycle && ACK else masterHasRequest && ACK

    @deprecated("This status check doesn't map pipelined modes correctly, prefer masterHasRequest or isRequestAck " +
      "depending on whether you want to check if a request exists or if one was acknowledged")
    def isTransfer: Bool = if (config.isPipelined) isCycle && STB && !STALL
    else isCycle && STB

    def isWrite: Bool = masterHasRequest && WE

    def isRead: Bool = masterHasRequest && !WE

    def doSend: Bool = masterHasRequest && isRequestAck

    def doWrite: Bool = doSend && WE

    def doRead: Bool = doSend && !WE

    def wordAddress(addressGranularityIfUnspecified: AddressGranularity.AddressGranularity = AddressGranularity.UNSPECIFIED): UInt = {
      config.wordAddressInc(addressGranularityIfUnspecified) match {
        case 1 => bus.ADR
        case x: Int => (bus.ADR >> log2Up(x))
      }
    }

    def assignWordAddress(wordAddress: UInt, addressGranularityIfUnspecified: AddressGranularity.AddressGranularity = AddressGranularity.UNSPECIFIED, allowAddressResize: Boolean = false): Unit = {
      config.wordAddressInc(addressGranularityIfUnspecified) match {
        case 1 => {
          assert(allowAddressResize || bus.ADR.getBitsWidth == wordAddress.getWidth,
            s"allowAddressResize must be true to assign from an unlike address space for ${this} and ${wordAddress}")
          bus.ADR := wordAddress.resized
        }
        case x: Int => {
          val byteAddress = wordAddress << log2Up(x)
          assert(allowAddressResize || bus.ADR.getBitsWidth == byteAddress.getWidth || bus.ADR.getBitsWidth == wordAddress.getWidth,
            s"allowAddressResize must be true to assign from an unlike address space for ${this} and ${wordAddress}")
          bus.ADR := byteAddress.resized
        }
      }
    }

    def assignByteAddress(byteAddress: UInt, addressGranularityIfUnspecified: AddressGranularity.AddressGranularity = AddressGranularity.UNSPECIFIED, allowAddressResize: Boolean = false): Unit = {
      config.wordAddressInc(addressGranularityIfUnspecified) match {
        case 1 => {
          val wordAddress = byteAddress >> log2Up(config.dataWidth / 8)
          assert(allowAddressResize || bus.ADR.getBitsWidth == wordAddress.getWidth || bus.ADR.getBitsWidth == byteAddress.getWidth,
            s"allowAddressResize must be true to assign from an unlike address space for ${this} and ${wordAddress}")
          bus.ADR := wordAddress.resized
        }
        case x: Int => {
          assert(allowAddressResize || bus.ADR.getBitsWidth == byteAddress.getWidth,
            s"allowAddressResize must be true to assign from an unlike address space for ${this} and ${byteAddress}")
          bus.ADR := byteAddress.resized
        }
      }
    }

    def assignAddress(that : Wishbone, allowAddressResize: Boolean = false): Unit = {
      val byteAddress = that.byteAddress()

      config.wordAddressInc() match {
        case 1 => {
          val wordAddress = byteAddress >> log2Up(that.config.dataWidth / 8)
          assert(allowAddressResize || bus.ADR.getBitsWidth == wordAddress.getWidth || bus.ADR.getBitsWidth == byteAddress.getWidth,
            s"allowAddressResize must be true to assign from an unlike address space for ${this} and ${wordAddress}")
          bus.ADR := wordAddress.resized
        }
        case x: Int => {
          assert(allowAddressResize || bus.ADR.getBitsWidth == byteAddress.getWidth,
            s"allowAddressResize must be true to assign from an unlike address space for ${this} and ${byteAddress}")
          bus.ADR := byteAddress.resized
        }
      }
    }

    def connectToGranularity(that: Wishbone, allowDataResize: Boolean = false, allowAddressResize: Boolean = false, allowTagResize: Boolean = false): Unit = {
      bus.connectTo(that, allowDataResize, allowAddressResize, allowTagResize)
      that.ADR.removeDataAssignments()
      that.assignWordAddress(bus.wordAddress(), allowAddressResize = allowAddressResize)
    }
  }

  implicit class PipelinedMemoryBusConfigExt(config : PipelinedMemoryBusConfig) {
    def wordAddressShift = log2Up((config.dataWidth / 8.0).ceil.toInt)
  }

  implicit class PipelinedMemoryBusCmdExt(cmd: PipelinedMemoryBusCmd) {
    def wordAddress = cmd.address >> cmd.config.wordAddressShift

    def assignByteAddress(byteAddress : UInt): Unit = {
      cmd.address := byteAddress
    }
    def assignWordAddress(wordAddress : UInt): Unit = {
      var byteAddress = wordAddress << cmd.config.wordAddressShift
      if(wordAddress.hasTag(tagAutoResize)) {
        byteAddress = byteAddress.resized
      }
      assignByteAddress(byteAddress)
    }
  }

  implicit class PipelinedMemoryBusExt(bus: PipelinedMemoryBus) {
    def resizeAddress(addr : Int) : PipelinedMemoryBus = {
      val pmb = PipelinedMemoryBus(addr, bus.config.dataWidth)
      bus.cmd.payload.address := pmb.cmd.payload.address.resized
      bus.cmd.payload.data := pmb.cmd.payload.data
      bus.cmd.mask := pmb.cmd.payload.mask
      bus.cmd.write := pmb.cmd.payload.write

      bus.cmd.arbitrationFrom(pmb.cmd)
      pmb.rsp << bus.rsp
      pmb
    }
  }
}
