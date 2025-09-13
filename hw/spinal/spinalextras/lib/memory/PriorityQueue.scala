package spinalextras.lib.memory

import spinal.core._
import spinal.lib.bus.simple.{PipelinedMemoryBusCmd, PipelinedMemoryBusConfig}
import spinal.lib.fsm._
import spinal.lib.{CounterUpDown, Flow, master, slave}
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties, FormalProperty}

abstract class PriorityQueue[T <: Data](dataType : HardType[T],
                                    maxElements : Int) extends ComponentWithFormalProperties {

  val size = CounterUpDown(maxElements + 1)

  val io = new Bundle {
    val push = slave Stream (dataType)

    val top = master Flow (dataType)
    val pop = in(Bool())
  }

  io.push.setBlocked()
  io.top.setIdle()

  val formal_check_word = Reg(Bits(io.push.payload.getBitsWidth bits)) init(0)

  // Min heap is a < b; top is smallest
  def compare(a: T, b: T, or_equal : Boolean = false) = if(or_equal) a.asBits.asUInt <= b.asBits.asUInt else a.asBits.asUInt < b.asBits.asUInt

  val idx, minimum_idx = Reg(UInt(size.getBitsWidth bits)) init(0)
  val idxNext = cloneOf(idx)
  idxNext := idx

  val request_value_flow = Flow(PipelinedMemoryBusCmd(PipelinedMemoryBusConfig(size.getBitsWidth, dataType.getBitsWidth)))
  request_value_flow.setIdle()
  val read_value_flow = Flow(dataType())
  def request_value(idx : UInt): Unit = {
    request_value_flow.address := idx.resized
    request_value_flow.write := False
    request_value_flow.valid := True
  }

  def set_value(idx: UInt, data: T): Unit = {
    when(idx === 0) {
      top := data
    }
    request_value_flow.address := idx
    request_value_flow.data.assignFromBits(data.asBits)
    request_value_flow.write := True
    request_value_flow.valid := True
  }

  def formal_fetch_value(idx: UInt): T

  def set_idx(idx : UInt) {
    idxNext := idx
  }
  idx := idxNext

  val data_at_idx, top = Reg(dataType())
  val data_at_minimum = Reg(dataType())

  val left_child_idx = (idx << 1) | 1
  val right_child_idx = (idx << 1) + 2

  val left_child_idx_next = (idxNext << 1) | 1
  val right_child_idx_next = (idxNext << 1) + 2

  io.top.payload := top

  // Can be optimized?
  def parent(idx : UInt) = (idx - 1) / 2
  val idx_parent = (idx - 1) / 2
  val idx_parent_next = (idxNext - 1) / 2

  val pop_fire = io.top.valid && io.pop

  def pop_element_fix_down(minimum_idx : UInt, data_at_minimum : T): Unit = {
    when(minimum_idx =/= idx) {
      this.minimum_idx := minimum_idx
      set_value(idx, data_at_minimum)
      set_idx(minimum_idx)
      fsm.goto(fsm.pop_element_fix_down_start_stage)
    } otherwise {
      set_value(idx, data_at_idx)
      fsm.goto(fsm.idle)
    }
  }

  val fsm = new StateMachine {
    val idle = new State with EntryPoint
    val push_element_fix_up_fetch_parent_stage, push_element_fix_up_fetch_parent,
    pop_element, pop_element_fix_down_start_stage, pop_element_fix_down_start, pop_element_fix_down_comp_right = new State

    idle.whenIsActive {
      set_idx(0)

      io.top.valid := size =/= 0
      io.push.ready := !size.mayOverflow

      minimum_idx.setAll()

      data_at_idx := io.push.payload

      when(io.push.fire && pop_fire) {
        formal_check_word := formal_check_word ^ io.push.payload.asBits ^ io.top.payload.asBits
        goto(pop_element_fix_down_start)
      } elsewhen (io.push.fire) {
        formal_check_word := formal_check_word ^ io.push.payload.asBits

        set_idx(size.resized)
        goto(push_element_fix_up_fetch_parent)
      } elsewhen (pop_fire) {
        formal_check_word := formal_check_word ^ io.top.payload.asBits
        when(size =/= 1) {
          goto(pop_element)
        } otherwise {
          size.clearAll()
        }
      }
    }

    pop_element.onEntry {
      request_value(size - 1)
    }
    pop_element.whenIsActive {
      when(read_value_flow.fire) {
        data_at_idx := read_value_flow.payload
        size.decrement()
        set_idx(0)
        goto(pop_element_fix_down_start)
      }
    }

    pop_element_fix_down_start_stage.whenIsActive {
      goto(pop_element_fix_down_start)
    }

    pop_element_fix_down_start.onEntry {
      when(left_child_idx_next < size) {
        request_value(left_child_idx_next)
      }
    }

    pop_element_fix_down_start.whenIsActive {
      minimum_idx := idx
      data_at_minimum := data_at_idx

      when(left_child_idx < size) {
        when(read_value_flow.valid) {
          when(compare(read_value_flow.payload, data_at_idx)) {
            minimum_idx := left_child_idx.resized
            data_at_minimum := read_value_flow.payload
          }
          goto(pop_element_fix_down_comp_right)
        }
      } otherwise {
        set_value(idx, data_at_idx)
        goto(idle)
      }
    }

    pop_element_fix_down_comp_right.onEntry {
      when(right_child_idx_next < size) {
        request_value(right_child_idx_next)
      }
    }
    pop_element_fix_down_comp_right.whenIsActive {
      when(right_child_idx < size) {
        when(read_value_flow.valid) {
          when(compare(read_value_flow.payload, data_at_minimum)) {
            pop_element_fix_down(right_child_idx.resized, read_value_flow.payload)
          } otherwise {
            pop_element_fix_down(minimum_idx, data_at_minimum)
          }
        }
      } otherwise {
        pop_element_fix_down(minimum_idx, data_at_minimum)
      }
    }

    push_element_fix_up_fetch_parent_stage.whenIsActive {
      goto(push_element_fix_up_fetch_parent)
    }

    push_element_fix_up_fetch_parent.onEntry {
      when(idxNext =/= 0) {
        request_value(idx_parent_next)
      }
    }

    push_element_fix_up_fetch_parent.whenIsActive {
      when(idx === 0) {
        set_value(idx, data_at_idx)
        size.increment()
        goto(idle)
      } elsewhen(read_value_flow.valid) {
        when(compare(data_at_idx, read_value_flow.payload)) {
          set_value(idx, read_value_flow.payload)
          set_idx(idx_parent)

          goto(push_element_fix_up_fetch_parent_stage)
        } otherwise {
          set_value(idx, data_at_idx)
          size.increment()
          goto(idle)
        }
      }
    }

  }

  fsm.build()

  val state_should_be_valid = fsm.isActive(fsm.idle)

  val seen_conditions = Seq(
    RegInit(False) setWhen (io.push.fire && pop_fire),
    RegInit(False) setWhen (io.push.fire && !pop_fire),
    RegInit(False) setWhen (!io.push.fire && pop_fire),
    RegInit(False) setWhen (size === maxElements)
  )

  override def covers(): Seq[FormalProperty] = new FormalProperties {
    seen_conditions.foreach(c => addFormalProperty(c && state_should_be_valid))
  }

  override def formalComponentProperties(): Seq[FormalProperty] = new FormalProperties {
    when(request_value_flow.valid) {
      addFormalProperty(request_value_flow.address <= size)
    }
    addFormalProperty(size <= maxElements)
    addFormalProperty(idx <= size)
    addFormalProperty(minimum_idx >= idx)
  }

}

class PriorityQueueLocalMemory[T <: Data](dataType : HardType[T],
                                          maxElements : Int) extends PriorityQueue[T](dataType, maxElements) {

  lazy val mem = Mem(dataType, maxElements)
  val value = Reg(dataType())

  read_value_flow.valid := RegNext(request_value_flow.valid && !request_value_flow.write)
  read_value_flow.payload := value

  when(request_value_flow.write && request_value_flow.valid) {
    mem(request_value_flow.address).assignFromBits(request_value_flow.data.asBits)
  } otherwise {
    value := mem(request_value_flow.address)
  }

  override def formal_fetch_value(idx: UInt) = mem(idx)

  override def formalComponentProperties(): Seq[FormalProperty] = new FormalProperties {
    var contents_hash = B(0)

    for (i <- 0 until maxElements) {
      val element_is_valid = compare(mem((i-1) / 2), mem(i), or_equal = true) || i >= size.value
      element_is_valid.setPartialName(s"element_is_valid_${i}")

      addFormalProperty(element_is_valid, f"Heap verification element ${i}")

      contents_hash = contents_hash ^ ((i < size.value) ? mem(i).asBits | 0)
    }

    contents_hash.setWeakName("contents_hash")

    when(state_should_be_valid) {
      addFormalProperty(contents_hash === formal_check_word, "Ensure that the push/pop hash matches up")
    }

    when(io.top.valid) {
      addFormalProperty(io.top.payload === mem(0))
    }
  }.addFormalProperties(super.formalComponentProperties())
}
