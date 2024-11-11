package spinalextras.lib.lattice

import spinal.core._
import spinalextras.lib.Constraints

import java.io.PrintWriter

object IPX {
  def generate_ipx[T <: Component](report : SpinalReport[T]): Unit = {
    val file = new PrintWriter(s"${report.toplevelName}.ipx")

    file.write(
      s"""<?xml version="1.0" ?>
        |<RadiantModule generator="ipgen" module="custom" name="${report.toplevelName}" source_format="Verilog" >
        | <Package>
      """.stripMargin)

    for (elem <- report.generatedSourcesPaths) {
      file.write(s"""  <File name="${elem}" type="top_level_verilog"/>""")
    }

    val ldc_file = report.globalData.config.targetDirectory + "/" + report.toplevelName + ".ldc"
    Constraints.write_file(report, ldc_file)

    file.write(
      s"""
        |  <File name="${ldc_file}" type="timing_constraints"/>
        | </Package>
        |</RadiantModule>
        |""".stripMargin)

    file.close()
  }
}