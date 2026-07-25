package spinalextras.lib.noc

import spinal.core._
import spinalextras.lib.Config

import java.io.ByteArrayInputStream
import scala.sys.process._

/**
 * Elaborates a `NoC` for a given [[NocConfig]], hands the generated Verilog to
 * yosys for a generic `opt` pass, and reports the resulting `stat` (cell/gate
 * count) numbers. This is a rough relative-size comparison -- it deliberately
 * stops at generic-cell `opt`, not a full technology-mapped `synth`, so the
 * numbers are only meaningful compared against each other, not as real ASIC/FPGA
 * gate counts.
 */
object NocGateCount {

  case class GateCountResult(name: String, totalCells: Int, cellsByType: Map[String, Int], statOutput: String)

  private val cellTypeLine = """^\s*(\S+)\s+(\d+)\s*$""".r

  /** Parses the `chtype  count` lines out of a yosys `stat` cell breakdown. */
  private def parseCellsByType(statOutput: String): Map[String, Int] = {
    val lines = statOutput.linesIterator.dropWhile(!_.contains("Number of cells:")).drop(1)
    lines.takeWhile(_.trim.nonEmpty).flatMap {
      case cellTypeLine(name, count) => Some(name -> count.toInt)
      case _                         => None
    }.toMap
  }

  /**
   * Builds `new NoC(cfg)`, generates its Verilog, and runs it through yosys:
   * `read_verilog` -> `hierarchy -top` -> `proc` -> `flatten` -> `opt -full` -> `stat`.
   * `flatten` collapses the router/allocator sub-modules into the top level first,
   * so the reported cell count is the design's total rather than just the
   * top-level module's own cells.
   */
  def gateCount(name: String, cfg: NocConfig, yosysCmd: String = "yosys"): GateCountResult = {
    val targetDirectory = s"hw/gen/gatecount/$name"
    val report = Config.spinal.copy(targetDirectory = targetDirectory).generateVerilog(new NoC(cfg))
    val verilogFile = s"$targetDirectory/${report.toplevelName}.v"

    val yosysScript =
      s"""|read_verilog $verilogFile
          |hierarchy -top ${report.toplevelName}
          |proc
          |flatten
          |opt -full
          |stat
          |""".stripMargin

    val output = new StringBuilder
    val logger = ProcessLogger(line => output.append(line).append("\n"))
    val exitCode = (yosysCmd #< new ByteArrayInputStream(yosysScript.getBytes)) ! logger
    val statOutput = output.toString()

    if (exitCode != 0) {
      throw new RuntimeException(s"yosys failed (exit $exitCode) for $name:\n$statOutput")
    }

    val cellsByType = parseCellsByType(statOutput)
    val totalCells = "Number of cells:\\s+(\\d+)".r
      .findFirstMatchIn(statOutput)
      .map(_.group(1).toInt)
      .getOrElse(cellsByType.values.sum)

    GateCountResult(name, totalCells, cellsByType, statOutput)
  }

  /** Runs [[gateCount]] over every configuration and prints a summary table sorted by cell count. */
  def report(configurations: Seq[(String, NocConfig)] = NocConfig.testConfigurations(),
             yosysCmd: String = "yosys"): Seq[GateCountResult] = {
    val results = configurations.map { case (name, cfg) =>
      println(s"[NocGateCount] synthesizing $name ...")
      val result = gateCount(name, cfg, yosysCmd)
      println(s"[NocGateCount] $name: ${result.totalCells} cells")
      result
    }

    println()
    println(f"${"Configuration"}%-45s ${"Cells"}%10s")
    println("-" * 56)
    results.sortBy(_.totalCells).foreach { r =>
      println(f"${r.name}%-45s ${r.totalCells}%10d")
    }

    results
  }

  def main(args: Array[String]): Unit = {
    report()
  }
}
