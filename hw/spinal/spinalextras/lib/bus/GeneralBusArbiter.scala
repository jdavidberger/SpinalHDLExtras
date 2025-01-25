package spinalextras.lib.bus

import spinal.core._
import spinal.core.formal.HasFormalAsserts
import spinal.lib._
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping}
import spinal.lib.com.spi.ddr.SpiXdrMasterCtrl.{XipBus, XipCmd}
import vexriscv.ip._
import vexriscv.plugin.{DBusSimpleBus, DBusSimpleCmd, DBusSimpleRsp}

import scala.collection.mutable
import scala.language.postfixOps

trait IMemoryBusRsp[RSP <: Data] {
  def fire : Bool
  def payload : RSP
  def map(output : Stream[RSP])
  def map(output : IMemoryBusRsp[RSP], takeWhen : Bool)
}

trait IMemoryBusCmd[CMD <: Data] {
  def rsp_needed(cmd : Stream[CMD]) : Bool
  def map(input: Stream[CMD], output : Stream[CMD], takeWhen : Bool)
  def address(cmd: CMD) : UInt
}

trait IMemoryBus[BUS <: Data with IMasterSlave] {
  type CMD <: Data
  type RSP <: Data

  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I) = getOrElseUpdate(key, f(key))
  }

  val dataType : HardType[BUS]

  def address(cmd : CMD) : UInt
  def bus_address(bus : BUS) : UInt = address(cmd(bus).payload)

  def rsp_needed(bus : BUS) : Bool = rsp_needed(cmd(bus))
  def rsp_needed(cmd : Stream[CMD]) : Bool = cmd_requires_response(cmd.payload) && cmd.fire
  def bus_cmd_requires_response(bus : BUS) : Bool = cmd_requires_response(cmd(bus).payload)
  def cmd_requires_response(cmd : CMD) : Bool
  def rsp_required_count(bus : BUS) : UInt
  def rsp_fire(bus : BUS) : Bool

  def cmd(bus : BUS) : Stream[CMD]
//  def cmdTrait : IMemoryBusCmd[CMD]
//  def rsp : IMemoryBusRsp[RSP]

  def rsp_payload(input : BUS) : RSP
  def map_cmd(input: Stream[CMD], output : Stream[CMD], takeWhen : Bool):Stream[CMD] = input << output.takeWhen(takeWhen)
  def map_cmd(input: BUS, output : BUS, takeWhen : Bool) : Stream[CMD] = map_cmd(cmd(input), cmd(output), takeWhen)

  def map_rsp(input : BUS, output : Stream[RSP], decodeNoHit : Bool)
  def map_rsp(input : BUS, output : BUS)
  def set_rsp_idle(input : BUS) : Unit = {}
  def set_rsp_blocked(input : BUS) : Unit = {}

  def isProducerValid(input : BUS) : Bool
  def isConsumerValid(input : BUS) : Bool
}

class GeneralBusArbiter[T <: Data with IMasterSlave](val memoryBusAccess: IMemoryBus[T], portCount : Int, pendingRspMax : Int = 1, rspRouteQueue : Boolean = false, transactionLock : Boolean = true) extends Component with HasFormalAsserts {
  def dataType = memoryBusAccess.dataType

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
    val arbiter = arbiterFactory.build(cloneOf(memoryBusAccess.cmd(io.output).payload), portCount)
    (arbiter.io.inputs, io.inputs).zipped.foreach(_ <> memoryBusAccess.cmd(_))

    val rspRouteOh = Bits(portCount bits)

    val rspQueue = new Area {
      val (outputCmdFork, routeCmdFork) = StreamFork2(arbiter.io.output)
      memoryBusAccess.cmd(io.output) << outputCmdFork

      val rspRequired = memoryBusAccess.rsp_required_count(io.output)
      val rspNeeded = routeCmdFork.translateWith(TupleBundle(arbiter.io.chosenOH, rspRequired - 1)).takeWhen(rspRequired =/= 0)

      val rspRouteFifo = StreamFifo(rspNeeded.payload, pendingRspMax, latency = 1)
      val rspRoute = rspRouteFifo.io.pop
      rspRoute.ready := False

      rspRouteFifo.io.push <> rspNeeded
      val rsp_counter = Reg(rspRequired) init(0)
      when(memoryBusAccess.rsp_fire(io.output)) {
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

    memoryBusAccess.set_rsp_blocked(io.output)
    for ((input, id) <- io.inputs.zipWithIndex) {
      memoryBusAccess.set_rsp_idle(input)
      when(rspRouteOh(id)) {
        memoryBusAccess.map_rsp(input, io.output)
      }
      //input.rsp.map(io.output.rsp, rspRouteOh(id))
    }
  }

  formalAsserts()

  override lazy val formalValidInputs = Vec(io.inputs.map(memoryBusAccess.isProducerValid)).andR && memoryBusAccess.isConsumerValid(io.output)
  override def formalChecks()(implicit useAssumes: Boolean) = new Composite(this, "formalChecks") {
    withAutoPull()
    val isValidInputConsumer = io.inputs.map(memoryBusAccess.isConsumerValid)
    isValidInputConsumer.foreach(assertOrAssume(_))

    assertOrAssume(memoryBusAccess.isProducerValid(io.output))
  }
}

case class GeneralBusDecoder[T <: Data with IMasterSlave](val memoryBusAccess: IMemoryBus[T], mappings : Seq[AddressMapping], pendingMax : Int = 3) extends Component with HasFormalAsserts {
  val io = new Bundle {
    val input = slave(memoryBusAccess.dataType())
    val outputs = Vec(master(memoryBusAccess.dataType()), mappings.size)
  }
  val hasDefault = mappings.contains(DefaultMapping)
  val logic = if(hasDefault && mappings.size == 1){
    io.outputs(0) <> io.input

    null
  } else new Area {
    val hits = Vec(Bool(), mappings.size)
    for ((slaveBus, memorySpace, hit) <- (io.outputs, mappings, hits).zipped) yield {
      hit := (memorySpace match {
        case DefaultMapping => !hits.filterNot(_ == hit).orR
        case _ => memorySpace.hit(memoryBusAccess.bus_address(io.input))
      })

      memoryBusAccess.cmd(slaveBus).valid := memoryBusAccess.cmd(io.input).valid && hit
      memoryBusAccess.cmd(slaveBus).payload := memoryBusAccess.cmd(io.input).payload.resized
    }
    val noHit = if (!hasDefault) !hits.orR else False
    memoryBusAccess.cmd(io.input).ready := (hits, io.outputs).zipped.map(_ && memoryBusAccess.cmd(_).ready).orR || noHit

    val rspRequiredCount = memoryBusAccess.rsp_required_count(io.input)
    val rspRequiredCountQ = StreamFifo(cloneOf(rspRequiredCount), pendingMax, latency = 1)
    rspRequiredCountQ.io.push.setIdle()
    rspRequiredCountQ.io.pop.setBlocked()

    when(memoryBusAccess.cmd(io.input).fire && rspRequiredCount > 0) {
      rspRequiredCountQ.io.push.payload := rspRequiredCount - 1
      rspRequiredCountQ.io.push.valid := True
      assert(rspRequiredCountQ.io.push.ready)
    }

    val rsp_counter = Reg(rspRequiredCount) init(0)
    when(memoryBusAccess.rsp_fire(io.input)) {
      rsp_counter := rsp_counter + 1
      when(rsp_counter === rspRequiredCountQ.io.pop.payload) {
        rspRequiredCountQ.io.pop.ready := True
        rsp_counter := 0
      }
    }


    val rspHits = RegNextWhen(hits, memoryBusAccess.cmd(io.input).fire, init=Vec(mappings.map(_ => False)))
    val rspPending = rspRequiredCountQ.io.pop.valid
    val rspNoHit = if (!hasDefault) !rspHits.orR else False

    val output_rsp = Stream(memoryBusAccess.rsp_payload(io.input))
    output_rsp.valid := io.outputs.map(memoryBusAccess.rsp_fire).orR || (rspPending && rspNoHit)
    output_rsp.payload := io.outputs.map(memoryBusAccess.rsp_payload).read(OHToUInt(rspHits))
    memoryBusAccess.map_rsp(io.input, output_rsp, rspNoHit)

    val cmdWait = (memoryBusAccess.cmd(io.input).valid && rspPending && hits =/= rspHits) || rspRequiredCountQ.io.push.ready === False
    when(cmdWait) {
      memoryBusAccess.cmd(io.input).ready := False
      io.outputs.foreach(memoryBusAccess.cmd(_).valid := False)
    }
  }

  formalAsserts()
  override lazy val formalValidInputs = Vec(io.outputs.map(memoryBusAccess.isConsumerValid)).andR && memoryBusAccess.isProducerValid(io.input)
  override def formalChecks()(implicit useAssumes: Boolean) = new Composite(this, "formalChecks") {
    withAutoPull()
    val isValidOutputConsumer = io.outputs.map(memoryBusAccess.isConsumerValid)
    isValidOutputConsumer.foreach(assertOrAssume(_))

    assertOrAssume(memoryBusAccess.isConsumerValid(io.input))
  }
}

package object bus_traits {
//  implicit class InstructionCacheMemCmdExtImpl(_cmd: Stream[InstructionCacheMemCmd]) extends IMemoryBusCmd[InstructionCacheMemCmd] {
//    override def cmd: Stream[InstructionCacheMemCmd] = _cmd
//    override def rsp_needed(cmd:  Stream[InstructionCacheMemCmd]) = cmd.fire
//    override def map(output:  InstructionCacheMemCmdExtImpl.this.type, takeWhen:  Bool): Unit = ???
//
//    override def map(input:  Stream[InstructionCacheMemCmd], output:  Stream[InstructionCacheMemCmd], takeWhen:  Bool): Unit = ???
//    override def address(cmd: InstructionCacheMemCmd): UInt = ???
//
//}
//  implicit class InstructionCacheMemRspExtImpl(rsp: Flow[InstructionCacheMemRsp]) extends IMemoryBusRsp[InstructionCacheMemRsp] {
//
//    override def fire: Bool = rsp.fire
//    override def payload: InstructionCacheMemRsp = rsp.payload
//    override def map(output:  Stream[InstructionCacheMemRsp]): Unit = rsp << output.toFlow
//    override def map(output:  IMemoryBusRsp[InstructionCacheMemRsp], takeWhen: Bool): Unit = {
//      rsp.valid := output.fire && takeWhen
//      rsp.payload := output.payload
//    }
//}


  implicit class DSimpleBusExtImpl(bus: DBusSimpleBus) extends IMemoryBus[DBusSimpleBus] {
    override  type CMD = DBusSimpleCmd
    override  type RSP = DBusSimpleRsp
    type BUS = DBusSimpleBus
    override  val dataType: HardType[DBusSimpleBus] = bus
    override def cmd_requires_response(cmd:  CMD): Bool = !cmd.wr
    override def rsp_fire(bus:  DBusSimpleBus): Bool = bus.rsp.ready
    override def cmd(bus:  DBusSimpleBus): Stream[CMD] = bus.cmd
    override def map_rsp(input:  DBusSimpleBus, output:  Stream[RSP], decoderMiss : Bool): Unit = {
      output.ready := True
      input.rsp.ready := output.valid
      input.rsp.error := decoderMiss
      input.rsp.data := output.data
    }

    override def map_rsp(input:  DBusSimpleBus, output: DBusSimpleBus): Unit = {
      input.rsp := output.rsp
    }
    override def address(cmd:  DBusSimpleCmd): UInt = cmd.address
    override def rsp_payload(bus:  DBusSimpleBus): DSimpleBusExtImpl.this.RSP = bus.rsp

    override def rsp_required_count(bus : DBusSimpleBus) : UInt = Mux(bus.cmd.wr, 0, 1)
//
//
//    def formalContract = memoize { input: BUS =>
//      new Composite(input, "formalContract") {
//        val outstandingRsp = Reg(UInt(32 bits)) init (0)
//
//        val toAdd, toRemove = UInt(32 bits)
//        toAdd := 0
//        toRemove := 0
//        when(input.cmd.fire) {
//          toAdd := input.p.burstSize
//        }
//        when(input.rsp.fire) {
//          toRemove := 1
//        }
//        val delta = toAdd.intoSInt - toRemove.intoSInt
//        val nextOutstandingRsp = (outstandingRsp.intoSInt + delta).asUInt.resized
//        outstandingRsp := nextOutstandingRsp
//        val canRspBeValid = outstandingRsp =/= 0 || (toAdd > 0)
//        assume((outstandingRsp.intoSInt +^ delta) < outstandingRsp.maxValue)
//      }
//    }
//
    override def isProducerValid(bus: BUS): Bool = {
      bus.cmd.formalIsValid()
    }
//
    override def isConsumerValid(bus: BUS): Bool = True
  }

  implicit class InstructionCacheMemBusExtImpl(bus: InstructionCacheMemBus) extends IMemoryBus[InstructionCacheMemBus] {
    override  type CMD = InstructionCacheMemCmd
    override  type RSP = InstructionCacheMemRsp
    type BUS = InstructionCacheMemBus
    override  val dataType: HardType[InstructionCacheMemBus] = bus
    override def cmd_requires_response(cmd:  CMD): Bool = True
    override def rsp_fire(bus:  InstructionCacheMemBus): Bool = bus.rsp.fire
    override def cmd(bus:  InstructionCacheMemBus): Stream[CMD] = bus.cmd
    override def map_rsp(input:  InstructionCacheMemBus, output:  Stream[RSP], decoderMiss : Bool): Unit = {
      output.ready := True
      input.rsp.valid := output.valid
      input.rsp.payload.error := decoderMiss
      input.rsp.payload.data := output.data
    }

    override def map_rsp(input:  InstructionCacheMemBus, output: InstructionCacheMemBus): Unit = input.rsp << output.rsp
    override def address(cmd:  InstructionCacheMemCmd): UInt = cmd.address
    override def rsp_payload(bus:  InstructionCacheMemBus): InstructionCacheMemBusExtImpl.this.RSP = bus.rsp.payload

    override def rsp_required_count(bus : InstructionCacheMemBus) : UInt = bus.p.burstSize


    def formalContract = memoize { input: BUS =>
      new Composite(input, "formalContract") {
        val outstandingRsp = Reg(UInt(32 bits)) init (0)

        val toAdd, toRemove = UInt(32 bits)
        toAdd := 0
        toRemove := 0
        when(input.cmd.fire) {
          toAdd := input.p.burstSize
        }
        when(input.rsp.fire) {
          toRemove := 1
        }
        val delta = toAdd.intoSInt - toRemove.intoSInt
        val nextOutstandingRsp = (outstandingRsp.intoSInt + delta).asUInt.resized
        outstandingRsp := nextOutstandingRsp
        val canRspBeValid = outstandingRsp =/= 0 || (toAdd > 0)
        assume((outstandingRsp.intoSInt +^ delta) < outstandingRsp.maxValue)
      }
    }

    override def isProducerValid(bus: BUS): Bool = {
      bus.cmd.formalIsValid()
    }

    override def isConsumerValid(bus: BUS): Bool = memoize((bus: BUS) => new Composite(bus, "isConsumerValid") {
      val balancedRsp = formalContract(bus).canRspBeValid || !bus.rsp.valid
    }.balancedRsp)(bus)
  }

  implicit class XipBusMemBusExtImpl(bus: XipBus) extends IMemoryBus[XipBus] {
    override type CMD = XipCmd
    override type RSP = Fragment[Bits]
    type BUS = XipBus

    override val dataType: HardType[BUS] = bus

    override def cmd_requires_response(cmd: CMD): Bool = True

    override def rsp_fire(bus: BUS): Bool = bus.rsp.fire

    override def cmd(bus: BUS): Stream[CMD] = bus.cmd

    override def map_rsp(input: BUS, output: Stream[RSP], decoderMiss : Bool): Unit = {
      input.rsp << output
    }

    override def map_rsp(input: BUS, output: BUS): Unit = input.rsp << output.rsp

    override def set_rsp_idle(input: BUS) = input.rsp.setIdle()

    override def set_rsp_blocked(input: BUS): Unit = input.rsp.setBlocked()

    override def address(cmd: CMD): UInt = cmd.address

    override def rsp_payload(input: BUS) = input.rsp.payload

    override def rsp_required_count(bus: BUS): UInt = bus.cmd.length +^ 1

    def formalContract = memoize { input: BUS =>
      new Composite(input, "formalContract") {
        val outstandingRsp = Reg(UInt(32 bits)) init (0)

        val toAdd, toRemove = UInt(32 bits)
        toAdd := 0
        toRemove := 0
        when(input.cmd.fire) {
          toAdd := (input.cmd.length +^ 1).resized
        }
        when(input.rsp.fire) {
          toRemove := 1
        }
        val delta = toAdd.intoSInt - toRemove.intoSInt
        val nextOutstandingRsp = (outstandingRsp.intoSInt + delta).asUInt.resized
        outstandingRsp := nextOutstandingRsp
        val canRspBeValid = outstandingRsp =/= 0 || (toAdd > 0)
        assume((outstandingRsp.intoSInt +^ delta) < outstandingRsp.maxValue)
      }
    }

    override def isProducerValid(bus: BUS): Bool = {
      bus.cmd.formalIsValid()
    }

    override def isConsumerValid(bus: BUS): Bool = memoize((bus: BUS) => new Composite(bus, "isConsumerValid") {
      val balancedRsp = formalContract(bus).canRspBeValid || !bus.rsp.valid
      val valid = bus.rsp.formalIsValid() && balancedRsp
    }.valid)(bus)
  }
}
