package spinalextras.lib

import spinal.core._
import spinal.core.formal.past
import spinal.lib._

package object misc {
  implicit class StreamExt[T <: Data](stream : Stream[T]) {
    def formalCopayload[S <: Data](s : Data) = new Composite(s, "formalCopayload") {
      import stream._

      val wasStall = past(isStall) init(False)
      val checkValidHandshake = Mux(wasStall, valid, True)

      val priorValidPayload = RegNextWhen(s, valid)
      val checkValidPayloadInvariance = Mux(wasStall, priorValidPayload === s, True)
    }.checkValidPayloadInvariance
  }
  implicit class StreamFifoExt[T <: Data](fifo : StreamFifo[T]) {

    def formalFold[B](z: B)(op: (B, T, Bool) => B) : B = {
      def f(acc : B, args : (T, Bool)) : B = {
        val (c, isValid) = args
        op(acc, c, isValid)
      }
      val lst : Seq[(T, Bool)] = formalRawContents().zip(formalMask())
      lst.foldLeft(z)(op = f)
    }

    def formalRawContents(): Seq[T] ={
      import fifo._
      if(logic != null) {
        (0 until depth).map(x => (if (useVec) logic.vec(x) else logic.ram(x)))
      } else if (oneStage != null) {
        Seq(oneStage.buffer.payload)
      } else {
        Seq()
      }
    }

    def formalMask(): Seq[Bool] = new Composite(fifo){
      import fifo._

      val vec = if(logic != null) {
        // create mask for all valid payloads in FIFO RAM
        // inclusive [popd_idx, push_idx) exclusive
        // assume FIFO RAM is full with valid payloads
        //           [ ...  push_idx ... ]
        //           [ ...  pop_idx  ... ]
        // mask      [ 1 1 1 1 1 1 1 1 1 ]
        val mask = Vec(True, depth)
        val push_idx = logic.ptr.push.resize(log2Up(depth))
        val pop_idx = logic.ptr.pop.resize(log2Up(depth))
        // pushMask(i)==0 indicates location i was popped
        val popMask = (~((U(1) << pop_idx) - 1)).asBits
        // pushMask(i)==1 indicates location i was pushed
        val pushMask = ((U(1) << push_idx) - 1).asBits
        // no wrap   [ ... popd_idx ... push_idx ... ]
        // popMask   [ 0 0 1 1 1 1  1 1 1 1 1 1 1 1 1]
        // pushpMask [ 1 1 1 1 1 1  1 1 0 0 0 0 0 0 0] &
        // mask      [ 0 0 1 1 1 1  1 1 0 0 0 0 0 0 0]
        when(pop_idx < push_idx) {
          mask.assignFromBits(pushMask & popMask)
          // wrapped   [ ... push_idx ... popd_idx ... ]
          // popMask   [ 0 0 0 0 0 0  0 0 1 1 1 1 1 1 1]
          // pushpMask [ 1 1 0 0 0 0  0 0 0 0 0 0 0 0 0] |
          // mask      [ 1 1 0 0 0 0  0 0 1 1 1 1 1 1 1]
        }.elsewhen(pop_idx > push_idx) {
          mask.assignFromBits(pushMask | popMask)
          // empty?
          //           [ ...  push_idx ... ]
          //           [ ...  pop_idx  ... ]
          // mask      [ 0 0 0 0 0 0 0 0 0 ]
        }.elsewhen(logic.ptr.empty) {
          mask := mask.getZero
        }
        mask.toSeq
      } else if (oneStage != null) {
        Seq(oneStage.buffer.valid)
      } else {
        Seq()
      }
    }.vec

  }
}