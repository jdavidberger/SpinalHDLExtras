package spinalextras.lib.noc.protocols

import spinal.core.{Bits, IntToBuilder}
import spinal.lib.{Fragment, Stream}
import spinalextras.lib.misc.{StreamFragmentWidthAdapterEncoding, StreamTools}
import spinalextras.lib.noc.{NoC, NocConfig}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

/** A NoC node address that may not be resolved yet -- see `NoCBuilder.SlotAllocator.createSlot`
  * for when it's safe to read `resolvedAddress`. */
class NodeSlot private[protocols](requested: Int) {
  private var resolved: Int = requested

  def resolvedAddress: Int = {
    require(resolved >= 0, "NoC node address read before NoCBuilder.build() resolved auto-assigned addresses")
    resolved
  }

  private[protocols] def resolve(value: Int): Unit = resolved = value
}

class NoCBuilder(val cfg: NocConfig) {
  val protocols = new mutable.ArrayBuffer[ProtocolSpecification]()

  val inputs = new ArrayBuffer[(Int, Stream[Fragment[Bits]])]()
  val outputs = new ArrayBuffer[(Int, Stream[Fragment[Bits]])]()

  /** NoC inputs (packet injection points) and outputs (packet delivery points) are separate port
    * arrays -- a node that injects at address A need not be the same node that packets are
    * delivered to at address B (e.g. a PMB master's own "return address", carried in its request
    * subheader, is its *output* slot, entirely independent of the *input* slot it injects that
    * request from). So each direction gets its own address space to allocate/auto-assign from. */
  private class SlotAllocator {
    private val usedAddresses = new mutable.HashSet[Int]()
    private var nextAutoAddress = 0
    private val pendingAutoClaims = new ArrayBuffer[NodeSlot]()

    /** Reserves a NoC node address for later use: pass a concrete address to pin a node in place,
      * or -1 (default) to auto-assign the next free node. Auto addresses are only resolved once
      * `build()` runs -- after every specification sharing this builder has had a chance to
      * register its own claims -- so that an auto-assignment can never collide with an explicit
      * claim made by another specification later. Because of this, `NodeSlot.resolvedAddress`
      * must only be read from within a specification's own `build()` method, never at
      * registration time. */
    def createSlot(address: Int = -1): NodeSlot = {
      if (address >= 0) {
        require(address < cfg.topology.nodes, s"NoC node address $address is out of range (0 until ${cfg.topology.nodes})")
        require(!usedAddresses.contains(address), s"NoC node address $address already assigned")
        usedAddresses += address
        new NodeSlot(address)
      } else {
        val handle = new NodeSlot(-1)
        pendingAutoClaims += handle
        handle
      }
    }

    def resolveAutoClaims(): Unit = {
      for (handle <- pendingAutoClaims) {
        while (nextAutoAddress < cfg.topology.nodes && usedAddresses.contains(nextAutoAddress)) nextAutoAddress += 1
        require(nextAutoAddress < cfg.topology.nodes, "No free NoC node addresses remain")
        usedAddresses += nextAutoAddress
        handle.resolve(nextAutoAddress)
        nextAutoAddress += 1
      }
      pendingAutoClaims.clear()
    }
  }

  private val inputSlots = new SlotAllocator()
  private val outputSlots = new SlotAllocator()

  def createInputSlot(address: Int = -1): NodeSlot = inputSlots.createSlot(address)
  def createOutputSlot(address: Int = -1): NodeSlot = outputSlots.createSlot(address)

  def addSpecification(protocolSpecification: ProtocolSpecification) = {
    protocols.append(protocolSpecification)
  }

  def addInput(input: Stream[Fragment[Bits]], address: Int = -1): Unit = {
    if (input.payload.fragment.getBitsWidth != cfg.dataWidth) {
      val (header, tail) = StreamTools.takeHead(input)
      inputs.append((address, StreamFragmentWidthAdapterEncoding.encode(tail, cfg.dataWidth).insertHeader(header.resize(cfg.dataWidth bits))))
    } else {
      inputs.append((address, input))
    }
  }

  def addOutput(output: Stream[Fragment[Bits]], address: Int = -1) = {
    val outputStream = new Stream(Fragment(Bits(cfg.dataWidth bits)))
    outputs.append((address, outputStream))
    StreamFragmentWidthAdapterEncoding.decode(outputStream, output.fragment.getBitsWidth) >> output
  }

  def build(): NoC = {
    inputSlots.resolveAutoClaims()
    outputSlots.resolveAutoClaims()
    protocols.foreach(_.build())

    val noc = new NoC(cfg)
    for (input <- inputs) {
      noc.io.inputs(input._1) <> input._2
    }
    for (output <- outputs) {
      noc.io.outputs(output._1) <> output._2
    }
    noc.sealUnusedPorts()

    noc
  }
}
