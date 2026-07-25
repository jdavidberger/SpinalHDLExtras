package spinalextras.lib.noc

import spinal.core._
import spinalextras.lib.Config
import spinalextras.lib.noc.topology.{Mesh, Ring, Torus}
import spinalextras.lib.noc.virtualchannels.{Dynamic, Static}

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

  case class GateCountResult(name: String, totalCells: Int, cellsByType: Map[String, Int],
                              summaryStats: Seq[(String, Int)], statOutput: String)

  private val cellTypeLine = """^\s*(\S+)\s+(\d+)\s*$""".r
  private val summaryStatLine = """^\s*Number of ([^:]+):\s+(\d+)\s*$""".r

  /** Parses the `chtype  count` lines out of a yosys `stat` cell breakdown. */
  private def parseCellsByType(statOutput: String): Map[String, Int] = {
    val lines = statOutput.linesIterator.dropWhile(!_.contains("Number of cells:")).drop(1)
    lines.takeWhile(_.trim.nonEmpty).flatMap {
      case cellTypeLine(name, count) => Some(name -> count.toInt)
      case _                         => None
    }.toMap
  }

  /** Parses the top-level `Number of ...: N` lines out of a yosys `stat` report,
    * in the order yosys printed them (e.g. "wires", "wire bits", ..., "cells") --
    * excludes the per-celltype breakdown that follows "Number of cells:". */
  private def parseSummaryStats(statOutput: String): Seq[(String, Int)] = {
    statOutput.linesIterator.collect {
      case summaryStatLine(label, count) => label -> count.toInt
    }.toSeq
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
    val displayColumns = Set("wire bits", "memories", "memory bits", "cells")
    val summaryStats = parseSummaryStats(statOutput).filter(x => displayColumns.contains(x._1))
    val totalCells = "Number of cells:\\s+(\\d+)".r
      .findFirstMatchIn(statOutput)
      .map(_.group(1).toInt)
      .getOrElse(cellsByType.values.sum)

    GateCountResult(name, totalCells, cellsByType, summaryStats, statOutput)
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

  /** The NocConfig parameters that identify a configuration -- used both to
    * label yosys work directories and as the leftmost columns of [[markdownTable]]. */
  private def configParams(cfg: NocConfig): Seq[(String, String)] = Seq(
    "Topology"   -> cfg.topology.toString,
    "Nodes"      -> cfg.topology.nodes.toString,
    "Data Width" -> cfg.dataWidth.toString,
    "VCs"        -> cfg.virtualChannels.toString,
    "VC Depth"   -> cfg.vcDepth.toString,
    "VC Mode"    -> NocConfig.objectName(cfg.virtualChannelMode),
    "VC Policy"  -> NocConfig.objectName(cfg.virtualChannelArbitrationPolicy),
  )

  private def configLabel(cfg: NocConfig, idx: Int): String =
    (configParams(cfg).map(_._2) :+ idx.toString).mkString("_")

  /**
   * Runs [[gateCount]] over `configs` and renders a Markdown table: one row per
   * configuration, with the NoC parameters ([[configParams]]) as the leftmost
   * columns followed by every `Number of ...` line yosys' `stat` printed
   * (wires, wire bits, memories, cells, etc, in the order yosys reports them) --
   * the per-celltype ($mux, $adff, ...) breakdown is intentionally left out.
   */
  def markdownTable(configs: Seq[NocConfig], yosysCmd: String = "yosys"): String = {
    val rows = configs.zipWithIndex.map { case (cfg, idx) =>
      val label = configLabel(cfg, idx).replace(" ", "").replace("(", "").replace(")", "").replace(",","_")
      println(s"[NocGateCount] synthesizing $label ...")
      val result = gateCount(label, cfg, yosysCmd)
      (configParams(cfg), result.summaryStats)
    }

    // Union of stat labels across all rows, in first-seen order, in case some
    // configuration is missing a stat category another one has (e.g. no memories).
    val statHeaders = rows.flatMap(_._2.map(_._1)).distinct
    val paramHeaders = configParams(configs.headOption.getOrElse(NocConfig())).map(_._1)
    val headers = paramHeaders ++ statHeaders

    val sb = new StringBuilder
    sb.append("| ").append(headers.mkString(" | ")).append(" |\n")
    sb.append("|").append(Seq.fill(headers.size)("---").mkString("|")).append("|\n")

    def format(k : String, v : Int): String = {
      if(k.contains("bits")) {
        f"${v / 1000.0}%.2f kb"
      } else {
        v.toString
      }
    }

    for ((params, stats) <- rows) {
      val statValues = statHeaders.map(h => format(h, stats.toMap.getOrElse(h, 0)))
      val values = params.map(_._2) ++ statValues
      sb.append("| ").append(values.mkString(" | ")).append(" |\n")
    }

    sb.toString()
  }
}

/**
 * Standalone entry point: runs [[NocGateCount.markdownTable]] over a list of
 * configurations -- by default every configuration in
 * `NocConfig.testConfigurations()` -- and prints the resulting Markdown table.
 * Pass a file path as the first argument to also write the table there.
 */
object NocGateCountMarkdownApp {
  def main(args: Array[String]): Unit = {
    val configs = Seq(
      NocConfig(new Ring(4), dataWidth = 64, virtualChannels = 2, virtualChannelMode = Static, virtualChannelArbitrationPolicy = LowestFirst),
      NocConfig(new Mesh(2, 2), dataWidth = 64, virtualChannels = 1, virtualChannelMode = Static, virtualChannelArbitrationPolicy = LowestFirst),
      NocConfig(new Mesh(4, 4), dataWidth = 16, virtualChannels = 1, virtualChannelMode = Static, virtualChannelArbitrationPolicy = LowestFirst),
      NocConfig(new Mesh(4, 4), dataWidth = 16, virtualChannels = 1, virtualChannelMode = Static, virtualChannelArbitrationPolicy = LowestFirst),
      NocConfig(new Mesh(4, 4), dataWidth = 64, virtualChannels = 1, virtualChannelMode = Static, virtualChannelArbitrationPolicy = LowestFirst),
      NocConfig(new Mesh(4, 4), dataWidth = 16, virtualChannels = 1, virtualChannelMode = Static, virtualChannelArbitrationPolicy = RoundRobin),
      NocConfig(new Ring(16), dataWidth = 16, virtualChannels = 2, virtualChannelMode = Static, virtualChannelArbitrationPolicy = LowestFirst),
      NocConfig(new Mesh(4, 4), dataWidth = 16, virtualChannels = 2, virtualChannelMode = Static, virtualChannelArbitrationPolicy = LowestFirst),
      NocConfig(new Torus(4, 4), dataWidth = 16, virtualChannels = 2, virtualChannelMode = Static, virtualChannelArbitrationPolicy = LowestFirst),
      NocConfig(new Mesh(4, 4), dataWidth = 16, virtualChannels = 2, virtualChannelMode = Static, virtualChannelArbitrationPolicy = LowestFirst),
      NocConfig(new Mesh(4, 4), dataWidth = 16, virtualChannels = 4, virtualChannelMode = Static, virtualChannelArbitrationPolicy = LowestFirst),
      NocConfig(new Mesh(4, 4), dataWidth = 16, virtualChannels = 4, virtualChannelMode = Dynamic, virtualChannelArbitrationPolicy = LowestFirst),
    )
    val table = NocGateCount.markdownTable(configs)

    println(table)

    args.headOption.foreach { path =>
      java.nio.file.Files.write(java.nio.file.Paths.get(path), table.getBytes)
      println(s"[NocGateCountMarkdownApp] wrote table to $path")
    }
  }
}
