package spinalextras.lib.noc

import spinal.core.{Bits, IntToBuilder, RegNextWhen}
import spinal.lib.{Fragment, Stream}
import spinalextras.lib.misc.{StreamFragmentWidthAdapterEncoding, StreamTools}
import spinalextras.lib.noc.protocols.ProtocolSpecification

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

/** A NoC node address that may not be resolved yet -- see `NoCBuilder.SlotAllocator.createSlot`
  * for when it's safe to read `resolvedAddress`. */
class NodeSlot(requested: Int) {
  private var resolved: Int = requested

  def resolvedAddress: Int = {
    require(resolved >= 0, "NoC node address read before NoCBuilder.build() resolved auto-assigned addresses")
    resolved
  }

  def resolve(value: Int): Unit = resolved = value
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
    private val pendingAutoClaims = new ArrayBuffer[NodeSlot]()

    /** Reserves a NoC node address for later use: pass a concrete address to pin a node in place,
      * or -1 (default) to auto-assign the next free node. Auto addresses are only resolved once
      * `build()` runs -- after every specification sharing this builder has had a chance to
      * register its own claims -- so that an auto-assignment can never collide with an explicit
      * claim made by another specification later. Because of this, `NodeSlot.resolvedAddress`
      * must only be read from within a specification's own `build()` method, never at
      * registration time. */
    def createSlot(address: Int = -1): NodeSlot = {
      if (address >= 0 && !usedAddresses.contains(address)) {
        require(address < cfg.topology.nodes, s"NoC node address $address is out of range (0 until ${cfg.topology.nodes})")
        require(!usedAddresses.contains(address), s"NoC node address $address already assigned")
        usedAddresses += address
        new NodeSlot(address)
      } else {
        println(s"Warning: ${address} is already assigned; assigning random address")
        val handle = new NodeSlot(-1)
        pendingAutoClaims += handle
        handle
      }
    }

    /** Resolves every pending auto-assigned slot to the lowest-numbered free node, skipping any
      * node `forbiddenFor(handle)` rules out for that particular slot (used to steer auto
      * addressing away from routes `NoCBuilder.requireRoute` has flagged as required -- see
      * there for why some address combinations are unroutable). */
    def resolveAutoClaims(forbiddenFor: NodeSlot => Set[Int] = _ => Set.empty): Unit = {
      for (handle <- pendingAutoClaims) {
        val forbidden = forbiddenFor(handle)
        val candidate = (0 until cfg.topology.nodes).find(a => !usedAddresses.contains(a) && !forbidden.contains(a))
        require(candidate.isDefined, "No free NoC node address remains that satisfies this slot's routing constraints")
        usedAddresses += candidate.get
        handle.resolve(candidate.get)
      }
      pendingAutoClaims.clear()
    }
  }

  private val inputSlots = new SlotAllocator()
  private val outputSlots = new SlotAllocator()

  /** (input, output) pairs a packet must actually be able to travel between -- see `requireRoute`. */
  private val requiredRoutes = new ArrayBuffer[(NodeSlot, NodeSlot)]()

  def createInputSlot(address: Int = -1): NodeSlot = inputSlots.createSlot(address)
  def createOutputSlot(address: Int = -1): NodeSlot = outputSlots.createSlot(address)

  /** Declares that a packet injected at `input`'s node must be deliverable to `output`'s node.
    * The NoC fabric can never route a packet back out the very node it was injected from (its
    * local delivery port is excluded as a destination for anything arriving on its local
    * injection port), so `input` and `output` must not end up resolving to the same node --
    * registering the pair here lets auto-addressing steer away from that collision instead of
    * silently producing an unroutable pair. Must be called from `ProtocolSpecification.registerRoutes`,
    * before any address on either slot has been read, so auto-assignment can still take it into account. */
  def requireRoute(input: NodeSlot, output: NodeSlot): Unit = {
    requiredRoutes += ((input, output))
  }

  def addSpecification(protocolSpecification: ProtocolSpecification) = {
    protocols.append(protocolSpecification)
  }
  def verifyAddress(address : Int): Unit = {
    if(address != -1) {
      assert(address < cfg.topology.nodes)
    }
  }
  def addInput(input: Stream[Fragment[Bits]], address: Int = -1): Unit = {
    verifyAddress(address)
    if (input.payload.fragment.getBitsWidth != cfg.dataWidth) {
      val (headerFlow, tail) = StreamTools.takeHead(input)
      val header = headerFlow.toReg()
      val widthAdaptedEncoding = StreamFragmentWidthAdapterEncoding.encode(tail, cfg.dataWidth)
      val widthAdapterEncodingWithHeader = widthAdaptedEncoding.insertHeader(header.resize(cfg.dataWidth bits))
      inputs.append((address, widthAdapterEncodingWithHeader.setName(f"${input.name}Adapted")))
    } else {
      inputs.append((address, input))
    }
  }

  def addOutput(output: Stream[Fragment[Bits]], address: Int = -1) = {
    verifyAddress(address)
    val outputStreamWithHeader = new Stream(Fragment(Bits(cfg.dataWidth bits)))
    outputs.append((address, outputStreamWithHeader.setName(f"${output.name}")))
    val (hdrFlow, outputStream) = StreamTools.takeHead(outputStreamWithHeader)
    val hdr = hdrFlow.toReg()
    StreamFragmentWidthAdapterEncoding.decode(outputStream, output.fragment.getBitsWidth).insertHeader(hdr.resized).setName(f"${output.name}Adapted") >> output
  }

  def build(): NoC = {
    protocols.foreach(_.registerRoutes())

    // Outputs resolve first so that, by the time inputs resolve, every route an input slot needs
    // to avoid (its registered partners' node) is already a concrete address.
    outputSlots.resolveAutoClaims()
    inputSlots.resolveAutoClaims()

    protocols.foreach(_.build())

    val noc = new NoC(cfg)
    println("Input:")
    for (input <- inputs) {
      println(s"  - ${input._2.name}: ${cfg.topology.addressName(input._1)}")
      noc.io.inputs(input._1) << input._2
    }
    println("Output:")
    for (output <- outputs) {
      println(s"  - ${output._2.name}: ${cfg.topology.addressName(output._1)}")
      noc.io.outputs(output._1) >> output._2
    }
    noc.sealUnusedPorts()

    noc
  }
}
