package spinalextras.lib.tests

import spinal.core.{Bits, Bool, Bundle, ClockDomain, Component, Data, False, HardType, IntToBuilder, Reg, RegInit, True, U, UInt, assert, out, report, when}
import spinal.lib.{Counter, master, slave}

case class FifoTestBench[T <: Data](dataType: HardType[T], unique_name: Boolean = false) extends Component {
  val io = new Bundle {
    val push = master Stream (dataType)
    val pop = slave Stream (dataType)

    val valid = out(Bool())
    val valid_count = out(UInt(32 bits))
  }

  val cmd_stream = io.push
  val primed = Reg(Bool()) init (False) setWhen (cmd_stream.fire)
  val timeout_counter = Counter(32)

  when(~timeout_counter.willOverflowIfInc && ~cmd_stream.fire) {
    when(primed) {
      timeout_counter.increment()
    }
  } elsewhen (cmd_stream.fire) {
    timeout_counter.clear()
  }

  val pushAddress, popAddress = RegInit(U(0, 32 bits))

  val dataWidth = dataType.getBitsWidth
  val incPerOp = dataWidth / 8
  //cmd_stream.address := (0x000 + incPerOp * writeCnt.value).resized
  cmd_stream.payload.assignFromBits(AddressHash(pushAddress, dataWidth).resized)

  cmd_stream.valid := True

  when(cmd_stream.fire) {
    pushAddress := pushAddress + incPerOp
  }

  when(io.pop.fire) {
    popAddress := popAddress + incPerOp
  }

  io.valid_count.setAsReg() init (0)

  io.valid.setAsReg() init (True)

  val expected_value = Bits(dataWidth bits)
  expected_value := AddressHash(popAddress, dataWidth).resized

  //val outstanding_count = CounterUpDown((1L << 32), incWhen = io.bus.cmd.fire && ~io.bus.cmd.write, decWhen = io.bus.rsp.fire)
  //assert(~outstanding_count.msb)
  io.pop.ready := timeout_counter.value === 0
  when(io.pop.fire) {
    val valid_value = expected_value === io.pop.payload.asBits
    io.valid := (io.valid && valid_value) // && ~timeout_counter.willOverflowIfInc)
    when(~valid_value) {
      io.valid_count := 0
    }

    when(!valid_value) {
      report(Seq("Invalid value found given ", io.pop.payload, " vs expected ", expected_value, " at ", popAddress, " for ", ClockDomain.current.frequency.getValue.decomposeString))
      assert(False)
    } otherwise {
      io.valid_count := io.valid_count + 1
    }
  }


  if (unique_name) {
    io.valid_count.setName(s"valid_count${dataWidth}_${(ClockDomain.current.frequency.getValue / 1e6).toDouble.round.toInt}")
    io.valid.setName(s"valid${dataWidth}_${(ClockDomain.current.frequency.getValue / 1e6).toDouble.round.toInt}")
  }
}
