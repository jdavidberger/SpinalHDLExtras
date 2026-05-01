package spinalextras.lib.misc

import spinal.core.{Bool, Bundle, Data, False, HardType, True, cloneOf, when}
import spinal.lib.Stream
import spinalextras.lib.formal.{FormalData, FormalProperties, FormalProperty}


case class Optional[T <: Data](dataType : HardType[T]) extends Bundle with FormalData {
  val has_value = Bool()
  val value = dataType()

  override def formalIsStateValid(): Seq[FormalProperty] = new FormalProperties(this) {
    val value_properties = FormalData.formalIsStateValid(value)
      for (v <- value_properties) {
        addFormalProperty(has_value === False || v.condition, v.msg)(v.loc)
      }
  }
  def formalAssertEquivalence(that : Optional[T]) = new FormalProperties {
    addFormalProperty(has_value === that.has_value, s"${this} valid signal does not match ${that}")
    when(has_value) {
      addFormalProperty(value === that.value, s"${this} value signal does not match ${that}")
    }
  }
}

object Optional {
  def apply[T <: Data](valid : Bool, payload : T)= {
    val rtn = new Optional(cloneOf(payload))
    rtn.has_value := valid
    rtn.value := payload
    rtn
  }

  def Empty[T <: Data](dataType : HardType[T])= {
    val rtn = new Optional(dataType)
    rtn.has_value := False
    rtn.assignDontCareToUnasigned()
    rtn
  }
  def withValue[T <: Data](payload : T)= {
    val rtn = new Optional(cloneOf(payload))
    rtn.has_value := True
    rtn.value := payload
    rtn
  }
  def OnlyValid[T <: Data](stream : Stream[Optional[T]]) = {
    val outStream = Stream(cloneOf(stream.value))
    outStream.payload := stream.payload.value
    outStream.valid := False
    stream.ready := outStream.ready

    when(stream.valid) {
      outStream.valid := stream.payload.has_value
      stream.ready := outStream.ready || !stream.payload.has_value
    }

    outStream
  }
}