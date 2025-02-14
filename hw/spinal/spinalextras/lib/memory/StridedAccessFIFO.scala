package spinalextras.lib.memory

import spinal.core._

import spinal.lib.bus.regif.BusIf
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusConfig}
import spinal.lib.fsm._
import spinal.lib._
import spinal.lib.formal.{ComponentWithFormalAsserts, HasFormalAsserts}
import spinalextras.lib.logging.{FlowLogger, GlobalLogger, PipelinedMemoryBusLogger, SignalLogger}
import spinalextras.lib.misc.RegisterTools
import spinalextras.lib.testing.test_funcs


case class StridedAccessFIFOAsync[T <: Data](
                                         pushDataType: HardType[T],
                                         popDataType: HardType[T],
                                         depth: Int,
                                         baseAddress: BigInt,
                                         outCnt: Int,
                                         bufferSize: Int = 32,
                                         busConfig: PipelinedMemoryBusConfig = PipelinedMemoryBusConfig(32, 32),
                                         rsp_latency : Int = 0, cmd_latency : Int = 0
                                       ) extends ComponentWithFormalAsserts {
  val depthInBits = depth * pushDataType.getBitsWidth
  val depthInOutput = depthInBits / popDataType.getBitsWidth
  val reader = StridedAccessFIFOReaderAsync(popDataType, depthInOutput, baseAddress, outCnt,
    busConfig, rsp_latency = rsp_latency)

  val io = new Bundle {
    val push = slave Stream (Fragment(pushDataType))
    val pop = master (cloneOf(reader.io.pop))

    val bus = master(PipelinedMemoryBus(busConfig))
  }

  val asyncFifoBusContract = io.bus.formalContract
  val writer = StreamToBuffer(pushDataType, depth, baseAddress, busConfig)

  reader.io.pop <> io.pop
  reader.io.bus.cmd.setBlocked()

  val writeMode = RegInit(True) setWhen (reader.io.lastFire) clearWhen (writer.io.last)
  io.push <> writer.io.push

  val cmd = cloneOf(io.bus.cmd)
  cmd <> io.bus.cmd
  //cmd.queue(cmd_latency, latency = 0) <> io.bus.cmd

  val needsFlush = RegNextWhen(True, io.push.lastFire) init(False)

  cmd.setIdle()
  writer.io.bus.cmd.setBlocked()
  withAutoPull()
  assert(asyncFifoBusContract.outstandingReads.value === reader.io.bus.formalContract.outstandingReads.value)

  val fsm = new StateMachine {
    val read, wait_push_fall = new State
    val write = new State with EntryPoint {
      whenIsActive {
        cmd <> writer.io.bus.cmd

        when(writer.io.last) {
          needsFlush := False
          goto(read)
        }
      }
    }

    read.whenIsActive {
      cmd <> reader.io.bus.cmd
      when(reader.io.lastFire) {
        goto(wait_push_fall)
      }
    }

    wait_push_fall.whenIsActive {
      when(~needsFlush || io.push.lastFire) {
        goto(write)
      }
    }
  }

  writer.io.bus.rsp.setIdle()
  io.bus.rsp <> reader.io.bus.rsp

  fsm.build()

  GlobalLogger(
    Set("profile"),
    SignalLogger.concat("StridedFifoState", fsm.stateReg),
    FlowLogger.profile_signal("pushes", io.push.fire, 1000 ms),
    FlowLogger.profile_signal("lastPop", reader.io.lastFire, 1000 ms),
  )

  def attach_bus(busSlaveFactory: BusIf): Unit = {
    RegisterTools.ReadOnly(busSlaveFactory, "writeMode", writeMode)
    writer.attach_bus(busSlaveFactory)
    reader.attach_bus(busSlaveFactory)
    PipelinedMemoryBusLogger.attach_debug_registers(busSlaveFactory, io.bus.setName("strided_bus"))
  }

  //override lazy val formalValidInputs = io.bus.formalIsConsumerValid() && io.push.formalIsValid() && io.pop.formalIsValid()
  override def formalChecks()(implicit useAssumes: Boolean) = new Composite(this, "formalAsserts"){
    assertOrAssume(reader.io.pop.formalContract.outstandingFlows === io.pop.formalContract.outstandingFlows)
    formalCheckOutputsAndChildren()
  }
}

case class StridedAccessFIFO[T <: Data](
                                         pushDataType: HardType[T],
                                         popDataType: HardType[T],
                                         depth: Int,
                                         baseAddress: BigInt,
                                         outCnt: Int,
                                         bufferSize: Int = 32,
                                         busConfig: PipelinedMemoryBusConfig = PipelinedMemoryBusConfig(32, 32),
                                         rsp_latency : Int = 0, cmd_latency : Int = 0
) extends Component {
  val io = new Bundle {
    val push = slave Stream (Fragment(pushDataType))
    val pop = master Stream (Vec(popDataType, outCnt))

    val bus = master(PipelinedMemoryBus(busConfig))
  }
  val depthInBits = depth * pushDataType.getBitsWidth
  val depthInOutput = depthInBits / popDataType.getBitsWidth
  val writer = StreamToBuffer(pushDataType, depth, baseAddress, busConfig)
  val reader = StridedAccessFIFOReader(popDataType, depthInOutput, baseAddress, outCnt,
    busConfig, rsp_latency = rsp_latency)

  reader.io.pop.map(_.fragment) <> io.pop
  reader.io.bus.cmd.setBlocked()

  val writeMode = RegInit(True) setWhen (reader.io.pop.lastFire) clearWhen (writer.io.last)
  io.push <> writer.io.push

  val cmd = cloneOf(io.bus.cmd)
  cmd.queue(cmd_latency, latency = 0) <> io.bus.cmd

  val needsFlush = RegNextWhen(True, io.push.lastFire)

  cmd.setIdle()
  writer.io.bus.cmd.setBlocked()

  val fsm = new StateMachine {
    val read, wait_push_fall = new State
    val write = new State with EntryPoint {
      whenIsActive {
        cmd <> writer.io.bus.cmd

        when(writer.io.last) {
          needsFlush := False
          goto(read)
        }
      }
    }

    read.whenIsActive {
      cmd <> reader.io.bus.cmd
      when(reader.io.pop.lastFire) {
        goto(wait_push_fall)
      }
    }

    wait_push_fall.whenIsActive {
      when(~needsFlush || io.push.lastFire) {
        goto(write)
      }
    }
  }

  writer.io.bus.rsp.setIdle()
  io.bus.rsp <> reader.io.bus.rsp

  fsm.build()

  GlobalLogger(
    Set("profile"),
    SignalLogger.concat("StridedFifoState", fsm.stateReg),
    FlowLogger.profile_signal("pushes", io.push.fire, 1000 ms),
    FlowLogger.profile_signal("pops", io.pop.fire, 1000 ms),
    FlowLogger.profile_signal("lastPop", reader.io.pop.lastFire, 1000 ms),
  )

  def attach_bus(busSlaveFactory: BusIf): Unit = {
    RegisterTools.ReadOnly(busSlaveFactory, "writeMode", writeMode)
    writer.attach_bus(busSlaveFactory)
    reader.attach_bus(busSlaveFactory)
    PipelinedMemoryBusLogger.attach_debug_registers(busSlaveFactory, io.bus.setName("strided_bus"))
  }
}


