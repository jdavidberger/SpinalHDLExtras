package spinalextras.lib.mipi

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

/**
 * MIPI CSI-2 header sideband for camera stats (after CDC into pixel/stats domain).
 *
 * Pulses are one cycle in the destination domain:
 *   - sof / eof: short packet dt 0/1
 *   - line: long AV; word_count is CSI-2 payload bytes for that line
 */
case class MipiCameraStatsEvent() extends Bundle {
  val sof = Bool()
  val eof = Bool()
  val line = Bool()
  val word_count = UInt(16 bits)
}
