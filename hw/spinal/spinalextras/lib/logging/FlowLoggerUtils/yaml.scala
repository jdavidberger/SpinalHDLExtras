package spinalextras.lib.logging.FlowLoggerUtils

import spinal.core.{Data, MultiData, SpinalEnumCraft, assert}
import spinalextras.lib.logging.FlowLogger

import java.io.PrintWriter
import java.nio.file.{Files, Paths}

object FlowLoggerYaml {
  def apply(flowLogger : FlowLogger, output_path : String) {
    import flowLogger._

    val output_file = s"${output_path}/${name}.${signature.toHexString}.logger_defs.yml"
    val file = new PrintWriter(output_file)

    def get_field_name(n: String): String = {
      assert(n.nonEmpty)

      if (n.matches("[0-9*].*"))
        s"_${n}"
      else n
    }

    def emit(s: String): Unit = {
      file.write(s)
      file.write("\n");
      file.flush()
    }

    def emit_type(d: Data, tabCount: Int = 1): Unit = {
      val tabs = "   ".repeat(tabCount)
      d match {
        case b: MultiData => {
          b.elements.foreach(x => {
            val prefix = get_field_name(x._1)
            emit(s"${tabs}- ${prefix}:")
            emit_type(x._2, tabCount + 1)
          })
        }
        case e: SpinalEnumCraft[_] => {
          e.spinalEnum.elements.map(_.getDisplayName()).foreach(n => {
            emit(s"${tabs}- ${n}")
          })
        }
        case _ => {
          emit(s"${tabs}${d.getClass.getSimpleName}: ${d.getBitsWidth}")
        }
      }
    }

    emit(s"clock_freq: ${clockDomain.get.frequency.getValue.toDouble}")
    emit(s"signature: ${signature}")
    emit(s"index_size: ${index_size}")
    emit(s"log_bits: ${logBits}")
    emit("event_definitions:")
    datas.zipWithIndex.foreach(d_clk => {
      val (d, cd) = d_clk._1
      val idx = d_clk._2

      emit(s"   - name: ${flows()(idx)._1.name}")
      emit(s"     time_bits: ${logBits - d.getBitsWidth - index_size}")
      emit(s"     type: ")
      emit_type(d, 2)
    })

    file.close()

    Files.deleteIfExists(Paths.get(s"${output_path}/${name}.logger_defs.yml"))
    Files.createSymbolicLink(Paths.get(s"${output_path}/${name}.logger_defs.yml"), Paths.get(f"${name}.${signature.toHexString}.logger_defs.yml"))

  }
}