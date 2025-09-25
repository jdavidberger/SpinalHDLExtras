package spinalextras.lib.misc

import spinal.core.{Bool, Bundle, Data, False, HardType, True, cloneOf, when}
import spinal.lib.Stream
import spinalextras.lib.formal.{FormalData, FormalProperties, FormalProperty}


case class Optional[T <: Data](dataType : HardType[T]) extends Bundle with FormalData {
  val valid = Bool()
  val value = dataType()

  override def formalIsStateValid(): Seq[FormalProperty] = new FormalProperties(this) {
    val value_properties = FormalData.formalIsStateValid(value)
      for (v <- value_properties) {
        addFormalProperty(valid === False || v.condition, v.msg)(v.loc)
      }
  }
}

object Optional {
  def apply[T <: Data](valid : Bool, payload : T)= {
    val rtn = new Optional(cloneOf(payload))
    rtn.valid := valid
    rtn.value := payload
    rtn
  }

  def Empty[T <: Data](dataType : HardType[T])= {
    val rtn = new Optional(dataType)
    rtn.valid := False
    rtn.assignDontCareToUnasigned()
    rtn
  }
  def withValue[T <: Data](payload : T)= {
    val rtn = new Optional(cloneOf(payload))
    rtn.valid := True
    rtn.value := payload
    rtn
  }
  def OnlyValid[T <: Data](stream : Stream[Optional[T]]) = {
    val outStream = Stream(cloneOf(stream.value))
    outStream.payload := stream.payload.value
    outStream.valid := False
    stream.ready := outStream.ready

    when(stream.valid) {
      outStream.valid := stream.payload.valid
      stream.ready := outStream.ready || !stream.payload.valid
    }

    outStream
  }
}