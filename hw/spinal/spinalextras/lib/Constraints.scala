package spinalextras.lib

import spinal.core._
import spinal.lib.{BufferCC, StreamCCByToggle, StreamFifoCC}

import java.io.PrintWriter
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Constraints {

  val clocks = new mutable.ArrayBuffer[(Data, HertzNumber)]()
  val max_skews = new mutable.ArrayBuffer[(Seq[Data], TimeNumber)]()
  val clock_groups = new mutable.ArrayBuffer[(Seq[Data], Boolean)]()
  val constraints = new mutable.ArrayBuffer[(Seq[Data], Map[String, String])]()
  val false_paths = new ArrayBuffer[Data]()

  def write_file[T <: Component](report: SpinalReport[T], path : String): Unit = {
    val file = new PrintWriter(path)
    file.println("# Trust the clock crossing checks in spinal")
    for ((data, freq) <- clocks) {
      file.println(s"# ${data.name} ${freq.decompose}")
      file.println(s"create_clock -name {${data.name}} -period ${freq.toTime.toDouble * 1e9} [get_ports ${data.getRtlPath()}]")
      //file.println(s"set_false_path -from [get_clocks ${data.name}]")
    }

//    for ((clks, async) <- clock_groups) {
//      file.println(s"set_clock_groups ${clks.map("-group [get_clocks {" + _.name +"}]").mkString(" ")} ${if(async) "-asynchronous" else ""}")
//    }

    for ((datas, skew) <- max_skews) {
      file.println(s"set_max_skew [get_nets {${datas.map(_.getRtlPath() + "*").mkString(" ")}}] ${skew.toDouble * 1e9}")
    }

    for(false_path <- false_paths) {
      file.println(s"set_false_path -through [get_nets ${false_path.getRtlPath()}/*]")
    }

    report.toplevel.walkComponents {
      case c: StreamFifoCC[_] => {
        file.println(s"set_false_path -through [get_nets ${c.getRtlPath()}/*]")
      }
      case c: BufferCC[_] => {
        file.println(s"set_false_path -through [get_nets ${c.getRtlPath()}/*]")
      }
      case c: StreamCCByToggle[_] => {
        file.println(s"set_false_path -through [get_nets ${c.getRtlPath()}/*]")
      }
      case c: Component => {}
    }

    for ((datas, tags) <- constraints) {
      for(data <- datas) {
        file.println(s"ldc_set_port -iobuf {${tags.map(x => s"${x._1}=${x._2}").mkString(" ")}} [get_ports {${data.getRtlPath()}}]")
      }
    }
    file.close()
  }

  def add_false_path(d: Data*): Unit = {
    false_paths.appendAll(d)
  }

  def create_clock(d: Data, f: HertzNumber) = {
    clocks.append((d, f))
  }
  def set_max_skew(max_skew: TimeNumber, d: Data*) = {
    max_skews.append((d, max_skew))
  }
  def add_clock_group(asynchronous: Boolean, clks: Data*): Unit = {
    clock_groups.append((clks, asynchronous))
  }
  def set_constraints(tags: Map[String, String], ports: Data*): Unit = {
    constraints.append((ports, tags))
  }
}