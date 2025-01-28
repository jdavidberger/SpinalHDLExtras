package spinalextras.lib.bus.general

import spinal.core._
import spinal.core.formal.HasFormalAsserts
import spinal.lib._
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping}
import spinalextras.lib.misc.StreamFifoExt

import scala.language.postfixOps

class ResponseCounter(sizeBits : Int, pendingMax : Int) extends Component with HasFormalAsserts {
  val io = new Bundle {
    val increaseBy = slave(Stream(UInt(sizeBits bits)))
    val decrease = master(Stream(Bits(0 bits)))
  }

  val q = StreamFifo(io.increaseBy.payload, pendingMax, latency = 1)
  io.increaseBy <> q.io.push

  val rsp_counter = Reg(io.increaseBy.payload) init (0)

  io.decrease.valid := q.io.pop.valid
  q.io.pop.ready := False

  when(io.decrease.fire) {
    rsp_counter := rsp_counter + 1

    when(rsp_counter === q.io.pop.payload) {
      q.io.pop.ready := True
      rsp_counter := 0
    }
  }

  def formalTotalPending() = {
    q.formalFold(U(0))((a, b, c) => Mux(c, b +^ a +^ 1, a)) -^ rsp_counter
  }

  override lazy val formalValidInputs = io.increaseBy.formalIsValid()

  override def formalChecks()(implicit useAssumes: Boolean) = new Composite(this, "formalChecks") {
    q.formalAssumes()
    val totalInQ = q.formalFold(U(0))((a, b, c) => Mux(c, b +^ a +^ 1, a))
    assertOrAssume(totalInQ >= rsp_counter)
    assertOrAssume(io.decrease.formalIsValid())

    when(!q.io.pop.valid) {
      assertOrAssume(rsp_counter === 0)
    } otherwise {
      assertOrAssume(rsp_counter <= q.io.pop.payload)
    }
  }
}

case class GeneralBusDecoder[T <: Data with IMasterSlave](val busAccesor: GeneralBusInterface[T], mappings: Seq[AddressMapping], pendingMax: Int = 3) extends Component with HasFormalAsserts {
  import busAccesor._

  val io = new Bundle {
    val input = slave(dataType())
    val outputs = Vec(master(dataType()), mappings.size)
  }
  val hasDefault = mappings.contains(DefaultMapping)
  val logic = if (hasDefault && mappings.size == 1) {
    io.outputs(0) <> io.input

    null
  } else new Area {
    val mappingsWithDefault = if(hasDefault) mappings else mappings ++ Seq(DefaultMapping)
    val outputsWithDefault = if(hasDefault) io.outputs else {
      Vec(io.outputs.toSeq ++ Seq(decodeMissTarget()))
    }
    require(mappingsWithDefault.size == outputsWithDefault.size)

    val input_cmd = io.input.cmd

    val hits = Vec(Bool(), mappingsWithDefault.size)
    for ((slaveBus, memorySpace, hit) <- (outputsWithDefault, mappingsWithDefault, hits).zipped) yield {
      hit := (memorySpace match {
        case DefaultMapping => !hits.filterNot(_ == hit).orR
        case _ => memorySpace.hit(io.input.byteaAddress)
      })

      slaveBus.cmd.valid := input_cmd.valid && hit
      slaveBus.cmd.payload := input_cmd.payload.resized
    }

    val noHit = if (!hasDefault) !hits.orR else False
    assert(!noHit)

    io.input.cmd.ready := (hits, outputsWithDefault).zipped.map(_ && _.cmd.ready).orR || noHit

    val rspRequired = io.input.rspsRequired
    val rspPendingCounter = new ResponseCounter(rspRequired.getWidth, pendingMax + 1)
    rspPendingCounter.io.increaseBy.payload := rspRequired - 1
    val latchFirstValid = RegInit(True) clearWhen(rspPendingCounter.io.increaseBy.fire && !io.input.readRequestFire) setWhen(io.input.readRequestFire)
    rspPendingCounter.io.increaseBy.valid := io.input.readRequestValid && latchFirstValid
    rspPendingCounter.io.decrease.ready := io.input.rspFired

    val rspHits = RegNextWhen(hits, io.input.cmd.fire)
    val rspPending = rspPendingCounter.io.decrease.valid
    val rspNoHit = if (!hasDefault) !rspHits.orR else False

    val output_rsp = Stream(io.input.rsp.payload)
    output_rsp.valid := outputsWithDefault.map(_.rsp.fire).orR
    output_rsp.payload := outputsWithDefault.map(_.rsp.payload).read(OHToUInt(rspHits))
    busAccesor.map_rsp(io.input, output_rsp, False)

    val cmdWait = (io.input.cmd.valid && rspPending && hits =/= rspHits) || (!rspPendingCounter.io.increaseBy.ready && latchFirstValid)
    when(cmdWait) {
      io.input.cmd.ready := False
      outputsWithDefault.foreach(_.cmd.valid := False)
    }

  }

  override lazy val formalValidInputs = Vec(io.outputs.map(_.isConsumerValid)).andR && io.input.isProducerValid

  override def formalChecks()(implicit useAssumes: Boolean) = new Composite(this, "formalChecks") {
    withAutoPull()
    import busAccesor._

    val isValidOutputConsumer = io.outputs.map(_.isConsumerValid)
    isValidOutputConsumer.foreach(assertOrAssume(_))

    val inputRspPending = io.input.formalRspPending

    val multi = if(logic != null) new Area {
      when(!io.input.readRequestValid) {
        assertOrAssume(logic.latchFirstValid)
      }

      val floating = Mux(logic.latchFirstValid, U(0), logic.rspRequired)
      val outstandingRspByOutput = logic.outputsWithDefault.map(_.formalRspPending)
      assertOrAssume(CountOne(outstandingRspByOutput.map(_ > 0).asBits()) <= 1)
      (logic.rspHits, outstandingRspByOutput).zipped.foreach( (hit, cnt) => {
        when(!hit) {
          assertOrAssume(cnt === 0)
        }
      })
      val totalOutputsRspRequired = outstandingRspByOutput.fold(U(0))((x, y) => x +^ y)

      val formalTotalPending = logic.rspPendingCounter.formalTotalPending()
      assertOrAssume(formalTotalPending === (inputRspPending +^ floating))

      //val rspStreamOutput = busAccesor.formalRspPending(logic.rspStream.io.output)
      //assertOrAssume(totalOutputsRspRequired === rspStreamOutput)
      assertOrAssume(totalOutputsRspRequired === inputRspPending)
    }

    assertOrAssume(io.input.isConsumerValid)

    var singleDefault = if(logic == null) new Area {
      assert(io.outputs.size == 1)
      assertOrAssume(inputRspPending === io.outputs.head.formalRspPending)
    }
  }

}
