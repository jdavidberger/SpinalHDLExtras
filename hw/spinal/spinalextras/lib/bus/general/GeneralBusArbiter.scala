package spinalextras.lib.bus.general

import spinal.core._

import spinal.lib._
import spinal.lib.com.spi.ddr.SpiXdrMasterCtrl.{XipBus, XipBusParameters, XipCmd}
import spinal.lib.formal.ComponentWithFormalAsserts
import spinalextras.lib.misc.StreamFifoExt
import vexriscv.ip._
import vexriscv.plugin.{DBusSimpleBus, DBusSimpleCmd, DBusSimpleRsp}

import scala.collection.mutable
import scala.language.{implicitConversions, postfixOps}

class GeneralBusArbiter[T <: Data with IMasterSlave](val busAccesor: GeneralBusInterface[T], portCount : Int, pendingRspMax : Int = 1,
                                                     transactionLock : Boolean = true) extends ComponentWithFormalAsserts {
  import busAccesor._
  def dataType = busAccesor.dataType

  val io = new Bundle{
    val inputs = Vec(slave(dataType), portCount)
    val output = master(dataType)
  }
  val logic = if(portCount == 1) {
    io.output <> io.inputs(0)

    null
  } else new Area {
    val arbiterFactory = StreamArbiterFactory().lowerFirst
    if(transactionLock) arbiterFactory.transactionLock else arbiterFactory.noLock
    val arbiter = arbiterFactory.build(cloneOf(io.output.cmd.payload), portCount)
    (arbiter.io.inputs, io.inputs).zipped.foreach(_ <> _.cmd)

    val rspRouteOh = Bits(portCount bits)

    val rspQueue = new Area {
      val (outputCmdFork, routeCmdFork) = StreamFork2(arbiter.io.output)
      io.output.cmd << outputCmdFork

      val rspRequired = io.output.rspsRequired
      val rspNeeded = routeCmdFork.translateWith(TupleBundle(arbiter.io.chosenOH, rspRequired - 1)).takeWhen(rspRequired =/= 0)

      val rspRouteFifo = StreamFifo(rspNeeded.payload, pendingRspMax, latency = 1)

      val rspRoute = rspRouteFifo.io.pop
      rspRoute.ready := False

      rspRouteFifo.io.push <> rspNeeded
      val rsp_counter = Reg(rspRequired) init(0)
      when(io.output.rspFired) {
        rsp_counter := rsp_counter + 1

        when(rsp_counter === rspRoute.payload._2) {
          rspRoute.ready := True
          rsp_counter := 0
        }
      }

      rspRouteOh := 0
      when(rspRoute.valid) {
        rspRouteOh := rspRoute.payload._1
      }
    }

    io.output.rsp.setBlocked()
    for ((input, id) <- io.inputs.zipWithIndex) {
      input.rsp.setIdle()
      when(rspRouteOh(id)) {
        input.rsp.connect(io.output.rsp)
      }
    }
  }

  override lazy val formalValidInputs = Vec(io.inputs.map(_.isProducerValid)).andR && io.output.isConsumerValid
  override def formalChecks()(implicit useAssumes: Boolean) = new Composite(this, "formalChecks") {
    withAutoPull()

    val isValidInputConsumer = io.inputs.map(_.isConsumerValid)
    isValidInputConsumer.foreach(assertOrAssume(_))
    val single = if(self.logic == null) new Area {
      assertOrAssume(busAccesor.formalRspPending(io.inputs.head) === busAccesor.formalRspPending(io.output))
    }

    val multi = if(self.logic != null) new Area {
      val rspsInQueue = logic.rspQueue.rspRouteFifo.formalFold(Vec(U(0), portCount)) {
        case (acc: Vec[UInt], c: TupleBundle2[Bits, UInt], isValid: Bool) => {
          val newAcc = acc
          assertOrAssume(!isValid || CountOne(c._1) <= 1)
          newAcc(OHToUInt(c._1.asUInt)) := Mux(isValid, c._2 +^ 1, U(0))
          newAcc
        }
      }

      import logic._
      import logic.rspQueue._

      val inFlightRsps = io.inputs.indices.map(i => Mux(
        OHToUInt(rspRouteOh) === i, rsp_counter /*+^ busAccesor.rsp_fire(io.inputs(i)).asUInt*/ , U(0)))

      val cmdStalled = io.inputs.indices.map(i => OHToUInt(logic.arbiter.io.chosenOH) === i &&
        logic.rspQueue.outputCmdFork.valid &&
        !logic.rspQueue.routeCmdFork.valid)

      val routeStalled = io.inputs.indices.map(i => OHToUInt(logic.arbiter.io.chosenOH) === i &&
        !logic.rspQueue.outputCmdFork.valid &&
        logic.rspQueue.routeCmdFork.valid)

      val stalledToOutput = cmdStalled.map(Mux(_, logic.rspQueue.rspRequired, U(0)))
      val stalledToRoute = routeStalled.map(Mux(_, logic.rspQueue.rspRequired, U(0)))
      val totalStalledToRoute = stalledToRoute.fold(U(0))((x, y) => x +^ y)

      when(!rspRouteFifo.io.pop.valid) {
        assertOrAssume(rsp_counter === 0)
      } otherwise {
        assertOrAssume(rsp_counter <= rspRouteFifo.io.pop.payload._2)
      }

      for (i <- io.inputs.indices) {
        assertOrAssume(
          (rspsInQueue(i) /*+^ inFlightRsps(i)*/) ===
            (io.inputs(i).formalRspPending +^ stalledToOutput(i) +^ inFlightRsps(i))
        )
      }

      val outputRspRequired = io.output.formalRspPending
      val inputRspRequired = io.inputs.map(_.formalRspPending).fold(U(0))((x, y) => x +^ y)
      val rspInRoute = rspsInQueue.fold(U(0))((x, y) => x +^ y)

      assertOrAssume((inputRspRequired +^ totalStalledToRoute) === (outputRspRequired))
      assertOrAssume(CountOne(logic.rspRouteOh) <= 1)
      assertOrAssume(io.output.isProducerValid)
    }

    formalCheckOutputsAndChildren()
  }
}
