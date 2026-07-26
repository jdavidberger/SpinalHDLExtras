package spinalextras.lib.misc

import spinal.core._
import spinal.lib._

/**
 * A pair of components that convert a Stream(Fragment(Bits)) between two bit widths, in either
 * direction, such that stacking the encoder and decoder reproduces the original stream exactly
 * (same beats, same payload bits, same `last` positions).
 *
 * If widthOut evenly divides widthIn, a packet's total bit length (an exact multiple of widthIn)
 * is automatically an exact multiple of widthOut too, so a plain demux/mux is sufficient and no
 * extra framing is required. Otherwise the last widthOut-wide beat of a packet may only be
 * partially filled with real bits; a small footer beat (carrying how many low bits of the
 * preceding beat were real) is appended so the decoder can discard the padding exactly.
 */
object StreamFragmentWidthAdapterEncoding {
  def needsFooter(widthIn: Int, widthOut: Int): Boolean = (widthIn % widthOut) != 0

  def encode(input: Stream[Fragment[Bits]], widthOut: Int, endianness: Endianness = LITTLE): Stream[Fragment[Bits]] = {
    if(widthOut == input.payload.fragment.getBitsWidth) {
      input
    } else {
      val enc = new StreamFragmentWidthAdapterEncoder(input.fragment.getWidth, widthOut, endianness)
      enc.io.input <> input
      enc.io.output
    }
  }

  def decode(input: Stream[Fragment[Bits]], widthIn: Int, endianness: Endianness = LITTLE): Stream[Fragment[Bits]] = {
    if(widthIn == input.payload.fragment.getBitsWidth) {
      input
    } else {
      val dec = new StreamFragmentWidthAdapterDecoder(widthIn, input.fragment.getWidth, endianness)
      dec.io.input <> input
      dec.io.output
    }
  }
}

/**
 * Converts Stream(Fragment(Bits(widthIn))) -> Stream(Fragment(Bits(widthOut))).
 * Inverse of StreamFragmentWidthAdapterDecoder(widthIn, widthOut, endianness).
 */
class StreamFragmentWidthAdapterEncoder(widthIn: Int, widthOut: Int, endianness: Endianness = LITTLE) extends Component {
  require(widthIn > 0 && widthOut > 0)

  val io = new Bundle {
    val input = slave(Stream(Fragment(Bits(widthIn bits))))
    val output = master(Stream(Fragment(Bits(widthOut bits))))
  }

  if (widthIn == widthOut) {
    io.output << io.input
  } else if (widthIn > widthOut && widthIn % widthOut == 0) new Composite(this, "split") {
    // widthOut evenly divides widthIn: every input beat splits into exactly (widthIn/widthOut)
    // output beats, for any packet length. No padding is ever possible, so no footer is needed.
    val factor = widthIn / widthOut
    val counter = Counter(factor, inc = io.output.fire)

    io.output.valid := io.input.valid
    endianness match {
      case `LITTLE` => io.output.fragment := io.input.fragment.subdivideIn(factor slices).read(counter)
      case `BIG`    => io.output.fragment := io.input.fragment.subdivideIn(factor slices).reverse.read(counter)
    }
    io.output.last := io.input.last && counter.willOverflowIfInc
    io.input.ready := io.output.ready && counter.willOverflowIfInc
  } else new Composite(this, "pack") {
    // widthOut does not evenly divide widthIn (this includes the "pack several narrow beats into
    // one wide beat" case, and any irregular/gcd pair). A packet's total bit length may or may not
    // land on a widthOut boundary depending on its length, so we run a generic bit-serial packer
    // and append a footer beat recording how many low bits of the final data beat were real.
    require(endianness == LITTLE, "BIG endianness is not supported for irregular width pairs")

    val footerBits = log2Up(widthOut)
    val bufWidth = widthIn + widthOut
    val validBits = log2Up(bufWidth + 1)

    val buffer = Reg(Bits(bufWidth bits)) init (0)
    val valid = Reg(UInt(validBits bits)) init (0)
    val pendingLast = Reg(Bool()) init (False)
    val footerArmed = Reg(Bool()) init (False)
    val footerValueCode = Reg(UInt(footerBits bits)) init (0)

    val wantDrain = valid >= widthOut
    val atTail = pendingLast && !footerArmed && !wantDrain
    val wantPartial = atTail && valid =/= 0
    val wantFooterOnly = atTail && valid === 0
    val wantFooter = footerArmed || wantFooterOnly

    val partialMask = Bits(widthOut bits)
    for ((bit, id) <- partialMask.asBools.zipWithIndex) bit := U(id) < valid
    val maskedPartial = buffer(widthOut - 1 downto 0) & partialMask

    val footerCode = Mux(footerArmed, footerValueCode, U(widthOut - 1, footerBits bits))
    val footerPayload = Bits(widthOut bits)
    footerPayload := 0
    footerPayload(footerBits - 1 downto 0) := footerCode.asBits

    io.output.valid := wantDrain || wantPartial || wantFooter
    io.output.last := wantFooter
    io.output.fragment := Mux(wantFooter, footerPayload, Mux(wantPartial, maskedPartial, buffer(widthOut - 1 downto 0)))

    val doDrain = wantDrain && io.output.fire
    val doPartial = wantPartial && io.output.fire
    val doFooterOnly = wantFooterOnly && io.output.fire
    val doFooterArmed = footerArmed && io.output.fire

    val validAfterDrainForRoom = valid - Mux(doDrain, U(widthOut, validBits bits), U(0, validBits bits))
    val hasRoom = (validAfterDrainForRoom.resize(validBits + 1 bits) + U(widthIn, validBits + 1 bits)) <= bufWidth
    io.input.ready := !pendingLast && hasRoom
    val inFire = io.input.fire

    val bufAfterDrain = Mux(doDrain, buffer |>> widthOut, buffer)
    val validAfterDrain = valid - Mux(doDrain, U(widthOut, validBits bits), U(0, validBits bits))
    val insertShift = validAfterDrain.resize(log2Up(bufWidth) bits)
    val bufAfterInsert = Mux(inFire,
      bufAfterDrain | (io.input.fragment.resize(bufWidth) << insertShift).resize(bufWidth),
      bufAfterDrain)

    buffer := bufAfterInsert
    valid := validAfterDrain + Mux(inFire, U(widthIn, validBits bits), U(0, validBits bits))

    when(doPartial) {
      buffer := 0
      valid := 0
      footerArmed := True
      footerValueCode := (valid - 1).resize(footerBits bits)
    }

    when(doFooterOnly || doFooterArmed) {
      pendingLast := False
      footerArmed := False
    }

    when(inFire && io.input.last) {
      pendingLast := True
    }
  }
}

/**
 * Converts Stream(Fragment(Bits(widthOut))) -> Stream(Fragment(Bits(widthIn))).
 * Inverse of StreamFragmentWidthAdapterEncoder(widthIn, widthOut, endianness).
 */
class StreamFragmentWidthAdapterDecoder(widthIn: Int, widthOut: Int, endianness: Endianness = LITTLE) extends Component {
  require(widthIn > 0 && widthOut > 0)

  val io = new Bundle {
    val input = slave(Stream(Fragment(Bits(widthOut bits))))
    val output = master(Stream(Fragment(Bits(widthIn bits))))
  }

  if (widthIn == widthOut) {
    io.output << io.input
  } else if (widthIn > widthOut && widthIn % widthOut == 0) new Composite(this, "join") {
    val factor = widthIn / widthOut
    val buffer = Reg(Bits(widthIn - widthOut bits))
    val counter = Counter(factor, inc = io.input.fire)
    val sendIt = CombInit(counter.willOverflowIfInc)

    io.output.valid := io.input.valid && sendIt
    io.output.last := io.input.last
    io.input.ready := io.output.ready || !sendIt

    val data = CombInit(io.input.fragment ## buffer)
    endianness match {
      case `LITTLE` => io.output.fragment.assignFromBits(data.resize(widthIn))
      case `BIG`    => io.output.fragment.assignFromBits(data.subdivideIn(factor slices).reverse.asBits().resize(widthIn))
    }

    when(io.input.fire) {
      buffer := io.input.fragment ## (buffer >> widthOut)
    }
  } else new Composite(this, "unpack") {
    require(endianness == LITTLE, "BIG endianness is not supported for irregular width pairs")

    val footerBits = log2Up(widthOut)
    val footerValueBits = footerBits + 1
    val bufWidth = widthIn + widthOut
    val validBits = log2Up(bufWidth + 1)

    val buffer = Reg(Bits(bufWidth bits)) init (0)
    val valid = Reg(UInt(validBits bits)) init (0)
    val stage = Reg(Bits(widthOut bits)) init (0)
    val stageValid = Reg(Bool()) init (False)
    val finalizing = Reg(Bool()) init (False)

    val wantDrain = valid >= widthIn
    io.output.valid := wantDrain
    io.output.last := finalizing && valid === widthIn
    io.output.fragment := buffer(widthIn - 1 downto 0)
    val doDrain = wantDrain && io.output.fire

    val isFooterArriving = io.input.last
    val footerValue = (io.input.fragment(footerBits - 1 downto 0).asUInt.resize(footerValueBits bits) + 1)

    val committingFull = stageValid && !isFooterArriving
    val committingFooter = stageValid && isFooterArriving
    val firstStage = !stageValid

    val validAfterDrain = valid - Mux(doDrain, U(widthIn, validBits bits), U(0, validBits bits))
    val commitAmount = Mux(committingFull, U(widthOut, validBits bits),
      Mux(committingFooter, footerValue.resize(validBits bits), U(0, validBits bits)))
    val hasRoom = (validAfterDrain.resize(validBits + 1 bits) + commitAmount.resize(validBits + 1 bits)) <= bufWidth
    io.input.ready := !finalizing && hasRoom
    val inFire = io.input.fire

    val doCommitFull = committingFull && inFire
    val doCommitFooter = committingFooter && inFire
    val doFirstStage = firstStage && inFire

    val footerMask = Bits(widthOut bits)
    for ((bit, id) <- footerMask.asBools.zipWithIndex) bit := U(id) < footerValue

    val bufAfterDrain = Mux(doDrain, buffer |>> widthIn, buffer)
    val insertBits = Mux(doCommitFull, stage, Mux(doCommitFooter, stage & footerMask, B(0, widthOut bits)))
    val insertShift = validAfterDrain.resize(log2Up(bufWidth) bits)
    val bufAfterInsert = Mux(doCommitFull || doCommitFooter,
      bufAfterDrain | (insertBits.resize(bufWidth) << insertShift).resize(bufWidth),
      bufAfterDrain)

    buffer := bufAfterInsert
    valid := validAfterDrain + Mux(inFire, commitAmount, U(0, validBits bits))

    when(doDrain && finalizing && valid === widthIn) {
      finalizing := False
    }
    when(doCommitFooter) {
      finalizing := True
    }

    when(doFirstStage) {
      stage := io.input.fragment
      stageValid := True
    } elsewhen (doCommitFull) {
      stage := io.input.fragment
      stageValid := True
    } elsewhen (doCommitFooter) {
      stageValid := False
    }
  }
}
