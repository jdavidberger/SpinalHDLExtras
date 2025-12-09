package spinalextras.lib.tests.formal

import spinal.core._
import spinal.lib._
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusConfig}
import spinal.lib.fsm._
import spinalextras.lib.formal.fillins.PipelinedMemoryBusFormal.PipelinedMemoryBusFormalExt
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties, FormalProperty}
import spinalextras.lib.misc.Optional
import spinalextras.lib.testing.{FormalAnyTestSuite, FormalTestSuite, GeneralFormalDut}

class DMACommand extends Bundle{
  val addr_in, addr_out, copy_size = UInt(32 bits)
}

class SimpleDMA(buggedLocation : Boolean ) extends ComponentWithFormalProperties {
  val io = new Bundle {
    val bus = master (new PipelinedMemoryBus(PipelinedMemoryBusConfig(32, 32)))
    val dmaCmd = slave Stream((new DMACommand))
  }
  io.bus.cmd.setIdle()
  io.dmaCmd.setBlocked()

  val counter = Counter(32 bit)

  val overflow = Bool()
  val readStream = io.bus.rsp.map(_.data).toStream(overflow, 1, 1)
  readStream.setBlocked()

  val fsm = new StateMachine {
    val idle : State = new State with EntryPoint {
      whenIsActive {
        counter.clear()

        when(io.dmaCmd.valid) {
          if(buggedLocation) {
            io.dmaCmd.ready := True
          }
          goto(readOne)
        }
      }
    }

    val readOne : State = new State {
      whenIsActive {
        io.bus.cmd.address := io.dmaCmd.addr_in + counter
        io.bus.cmd.write := False
        io.bus.cmd.valid := True
        when(io.bus.cmd.fire) {
          goto(writeOne)
        }
      }
    }

    val writeOne : State = new State {
      whenIsActive {
        io.bus.cmd.address := io.dmaCmd.addr_out + counter
        io.bus.cmd.write := True
        io.bus.cmd.mask.setAll()
        io.bus.cmd.data := readStream.payload
        io.bus.cmd.valid := readStream.valid
        readStream.ready := io.bus.cmd.ready
        when(io.bus.cmd.fire) {
          counter.increment()
          when(counter === io.dmaCmd.copy_size) {
            if(!buggedLocation) {
              io.dmaCmd.ready := True
            }
            goto(idle)
          } otherwise {
            goto(readOne)
          }
        }
      }
    }
  }

  override def formalComponentProperties(): Seq[FormalProperty] = new FormalProperties {
    when(fsm.isActive(fsm.writeOne)) {
      addFormalProperty(io.bus.contract.outstandingReads +^ readStream.valid.asUInt <= 1)
    } otherwise {
      addFormalProperty(io.bus.contract.outstandingReads +^ readStream.valid.asUInt === 0, "There should be no outstanding reads outside of writeOne")
    }

    when(fsm.isActive(fsm.readOne) || fsm.isActive(fsm.writeOne)) {
      addFormalProperty(io.dmaCmd.valid, "Valid must be true to be in readOne or writeOne states")
    }
  }
}

object SimpleDMAFormalTeter extends FormalTestSuite {
  override def defaultDepth() = 2

  override def generateRtl() = {
    Seq(
      ("Bugged", () => GeneralFormalDut(() => new SimpleDMA(true))),
      ("Fixed", () => GeneralFormalDut(() => new SimpleDMA(false)))
    )
  }

  def main(args: Array[String]): Unit = {
    formalTests().foreach(t => {
      print(t._1)
      t._2()
    })
  }
}