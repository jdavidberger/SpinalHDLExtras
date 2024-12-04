package spinalextras.lib.memory

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim.{SimBaseTypePimper, SimBitVectorPimper, SimBoolPimper, SimClockDomainHandlePimper, SimTimeout, simRandom}
import spinal.core.{Bits, Bundle, Component, Data, False, HardType, IntToBuilder, RegInit, RegNext, RegNextWhen, True, Vec, cloneOf, when}
import spinal.lib.bus.regif.BusIf
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusConfig}
import spinal.lib.fsm._
import spinal.lib.sim.StreamMonitor
import spinal.lib.{Fragment, master, slave}
import spinalextras.lib.Config
import spinalextras.lib.bus.simple.SimpleMemoryProvider
import spinalextras.lib.logging.{FlowLogger, GlobalLogger, PipelinedMemoryBusLogger, SignalLogger}
import spinalextras.lib.misc.RegisterTools

import scala.collection.mutable

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

  val writeMode = RegInit(True) setWhen (reader.io.pop.lastFire) clearWhen (writer.io.last)
  io.push <> writer.io.push

  val cmd = cloneOf(io.bus.cmd)
  cmd.queue(cmd_latency, latency = 0) <> io.bus.cmd

  val needsFlush = RegNextWhen(True, io.push.lastFire)

  cmd.setIdle()
  reader.io.bus.cmd.setBlocked()
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


class StridedAccessFIFOTest extends AnyFunSuite {
  def doTest() {
    Config.sim.doSim(
      new Component {
        val fifo = StridedAccessFIFO(Bits(32 bits), Bits(32 bits), 2304, 0x00000000L, 9, 32)
        val io = new Bundle {
          val push = slave (cloneOf(fifo.io.push))
          val pop = master(cloneOf(fifo.io.pop))
        }
        val memory = SimpleMemoryProvider()
        memory.io.bus <> fifo.io.bus
        fifo.io.pop <> io.pop
        fifo.io.push <> io.push
      }.setDefinitionName("StridedAccessFIFOReaderTest")
    ) { dut =>
      SimTimeout(5000 us)
      dut.clockDomain.forkStimulus(100 MHz)
      dut.io.pop.ready #= false
      dut.io.push.valid #= false
      dut.clockDomain.waitSampling(10)
      val q = new mutable.ArrayBuffer[Seq[BigInt]]()

      StreamMonitor(dut.io.pop, dut.clockDomain) {
        data => {
          q.append(data.map(_.toBigInt))
        }
      }

      dut.io.pop.ready #= true
      for(i <- 0 until 3) {
        val init_seq = (0 until 9).flatMap(idx => Array.fill(2304 / 9)(idx)).map(BigInt(_))
        init_seq.foreach(d => {
          dut.clockDomain.waitSamplingWhere(dut.io.push.ready.toBoolean)
          dut.io.push.payload.fragment #= d
          dut.io.push.payload.last #= false
          dut.io.push.valid #= true
          dut.clockDomain.waitSampling()
          dut.io.push.valid #= false
        })
      }

      while(q.size < 256 * 3) {
        dut.clockDomain.waitSampling(10)
      }
      dut.clockDomain.waitSampling(1000)

      println(q)
      assert(q.size == 256 * 3)
    }
  }

  test("Basic") {
    doTest();
  }
}