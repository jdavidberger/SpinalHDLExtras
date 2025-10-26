package spinalextras.lib.formal.fillins

import spinal.core._
import spinal.lib._
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties, FormalProperty, HasFormalProperties}

class StreamFifoFormalProperties[T <: Data](val fifo: StreamFifo[T]) extends HasFormalProperties {
  override protected def formalProperties(): Seq[FormalProperty] = new FormalProperties(fifo){
    import fifo._
    val logic_option = Option(logic)

    // Occupancy as dictated soley by push/pop pointers
    val push_pop_occupancy =
      logic_option.map(logic => {
        val push_sub_pop = logic.ptr.push -^ logic.ptr.pop
        if (withExtraMsb)
          Mux(logic.ptr.push >= logic.ptr.pop,
            push_sub_pop,
            (depth << 1) -^ logic.ptr.pop +^ logic.ptr.push
          )
        else
          Mux(logic.ptr.push === logic.ptr.pop,
            Mux(logic.ptr.wentUp, U(depth), U(0)),
            Mux(logic.ptr.push > logic.ptr.pop,
              push_sub_pop,
              depth -^ logic.ptr.pop +^ logic.ptr.push
            )
          )
      }).getOrElse(U(0))

    val extraOccupancy = logic_option.flatMap(logic => Option(logic.pop.sync)).map(_.readArbitation.valid.asUInt).getOrElse(U(0))

    val calculate_occupancy = push_pop_occupancy +^ extraOccupancy

    // Fifo depth of 0 is just a direct connect
    // Fifo depth of 1 is just a staged stream; so it can't be in an invalid state
    if (depth > 1) {
      addFormalProperty(calculate_occupancy === io.occupancy)
      addFormalProperty(io.availability === depth -^ io.occupancy)

      when(io.occupancy === 0) {
        addFormalProperty(io.pop.valid === (io.push.fire && Bool(withBypass)), "Occupancy check didn't result in right pop valid")
      }

      if (logic != null) {
        when(logic.ptr.pop === 0 && Bool(!isPow2(depth))) {
          addFormalProperty(logic.ptr.popOnIo === ((depth - extraOccupancy) % depth))
        } otherwise {
          addFormalProperty(logic.ptr.popOnIo === (logic.ptr.pop - extraOccupancy))
        }

        when(io.availability === 0) {
          addFormalProperty(io.push.ready === False, "Stream should not be able to take more items when there is no availibility")
        }

        if (!withExtraMsb) {
          addFormalProperty(logic.ptr.pop <= (depth - 1))
          addFormalProperty(logic.ptr.push <= (depth - 1))
        } else {
          addFormalProperty(logic.ptr.pop <= ((depth << 1) - 1))
          addFormalProperty(logic.ptr.push <= ((depth << 1) - 1))
        }

        if (forFMax) {
          val counterWidth = log2Up(depth) + 1
          val emptyStart = 1 << (counterWidth - 1)
          val fullStart = (1 << (counterWidth - 1)) - depth

          addFormalProperty(logic.ptr.arb.fmax.fullTracker.value === (fullStart +^ io.occupancy))
          addFormalProperty(logic.ptr.arb.fmax.emptyTracker.value === (emptyStart -^ push_pop_occupancy))
        }
      }
    }
  }

}
