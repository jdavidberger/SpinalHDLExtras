package spinalextras.lib.noc.protocols

import spinal.core.{Bits, IntToBuilder}
import spinal.lib.{Fragment, Stream}
import spinalextras.lib.misc.{StreamFragmentWidthAdapterEncoding, StreamTools}
import spinalextras.lib.noc.{NoC, NocConfig}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class NoCBuilder(val cfg: NocConfig) {
  val protocols = new mutable.ArrayBuffer[ProtocolSpecification]()

  val inputs = new ArrayBuffer[(Int, Stream[Fragment[Bits]])]()
  val outputs = new ArrayBuffer[(Int, Stream[Fragment[Bits]])]()

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
