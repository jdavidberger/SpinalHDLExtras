package spinalextras.lib

import spinal.core._
import spinal.lib.{BufferCC, FlowCCByToggle, FlowCCUnsafeByToggle, KeepAttribute, StreamCCByToggle, StreamFifoCC}

import java.io.PrintWriter
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Constraints {
  val clocks = new mutable.ArrayBuffer[(Data, HertzNumber)]()
  val max_skews = new mutable.ArrayBuffer[(Seq[Data], TimeNumber)]()
  val clock_groups = new mutable.ArrayBuffer[(Seq[Data], Boolean)]()
  val constraints = new mutable.ArrayBuffer[(Seq[Data], Map[String, String])]()
  val false_paths = new ArrayBuffer[Data]()
  val min_delay = new ArrayBuffer[(Seq[Data], TimeNumber)]()

  var false_path_all_clocks = false
  val verbatim_constraints = new ArrayBuffer[() => String]()

  def SetFalsePathAllClocks(): Unit = {
    false_path_all_clocks = true
  }


  def write_file[T <: Component](report: SpinalReport[T], path : String): Unit = {
    val file = new PrintWriter(path)

    report.globalData.config.defaultClockDomainFrequency match {
      case f : FixedFrequency => {
        val defaultClock = f.getValue
        file.println(s"# clk ${defaultClock.decompose}")
        file.println(s"create_clock -name {clk} -period ${defaultClock.toTime.toDouble * 1e9} [get_nets clk]")
      }
      case _ => {}
    }

    for ((data, freq) <- clocks) {
      //if(data.getComponents().nonEmpty) {
        file.println(s"# ${data.name} ${freq.decompose}")
        file.println(s"create_clock -name {${data.name}} -period ${freq.toTime.toDouble * 1e9} [get_ports ${data.getRtlPath()}]")
        //file.println(s"set_false_path -from [get_clocks ${data.name}]")
      //}
    }

    //    for ((clks, async) <- clock_groups) {
    //      file.println(s"set_clock_groups ${clks.map("-group [get_clocks {" + _.name +"}]").mkString(" ")} ${if(async) "-asynchronous" else ""}")
    //    }

    for ((datas, skew) <- max_skews) {
      file.println(s"set_max_skew [get_nets {${datas.map(_.getRtlPath() + "*").mkString(" ")}}] ${skew.toDouble * 1e9}")
    }

    for ((datas, delay) <- min_delay) {
      file.println(s"set_min_delay -through [get_nets {${datas.map(_.getRtlPath() + "*").mkString(" ")}}] ${delay.toDouble * 1e9}")
    }

    def set_false_path(d : Data): Unit = {
      KeepAttribute(d)
      d.addAttribute("syn_keep", 1).addAttribute("nomerge", "")

      val name_is_global = false //d.name.startsWith("zzz_")
      val net_selector = if(name_is_global) s"-hierarchical -regexp .*${d.name}.*" else s"${d.getRtlPath()}/*"
      file.println(s"set_false_path -through [get_nets ${net_selector}]")
    }

    def set_false_path_component(d : Component): Unit = {
      val name_is_global = false //d.name.startsWith("zzz_")
      val net_selector = if(name_is_global) s"-hierarchical -regexp .*${d.name}.*" else s"${d.getRtlPath()}/*"
      file.println(s"set_false_path -through [get_nets ${net_selector}]")
    }

    for(false_path <- false_paths) {
      set_false_path(false_path)
    }

    report.toplevel.walkComponents {
      case c: StreamFifoCC[_] => {
        set_false_path_component(c)
      }
      case c: BufferCC[_] => {
        set_false_path_component(c)
      }
      case c: StreamCCByToggle[_] => {
        set_false_path_component(c)
      }
      case c: FlowCCUnsafeByToggle[_] => {
        set_false_path_component(c)
      }
      case c: Component => {}
    }

    for ((datas, tags) <- constraints) {
      for(data <- datas) {
        file.println(s"ldc_set_port -iobuf {${tags.map(x => s"${x._1}=${x._2}").mkString(" ")}} [get_ports {${data.getRtlPath()}*}]")
      }
    }

    for (c <- verbatim_constraints) {
      file.println(c())
    }

    file.close()
  }


}

object Constraints {
  var constraints = new Constraints

  var toplevel : Component = null

  def addAttributeIfNeeded(d : Component, n : String, v : String): Unit = {
    if (!d.getTagsOf[Attribute].exists(a => a.getName == n)) {
      d.addAttribute(n, v)
    }
  }
  def keep_chain(d : Component): Unit = {
    if (d != null) {
      addAttributeIfNeeded(d, "syn_keep", "1")
      addAttributeIfNeeded(d, "nomerge", "")
      addAttributeIfNeeded(d, "keep_hierarchy", "TRUE")
      keep_chain(d.parent)
    }
  }

  def keep_key_heirarchy(d : Component): Unit = {
    walk_cc_components(d, {
      case c: Component => {
        keep_chain(c)
      }
      case c : Data => {
        keep_chain(c.component)
      }
    })
  }


  def walk_cc_components(d : Component, fn : Any => Unit): Unit = {
    for(false_path <- constraints.false_paths) {
      fn(false_path)
    }

    d.walkComponents {
      case c: StreamFifoCC[_] => {
        fn(c)
      }
      case c: BufferCC[_] => {
        fn(c)
      }
      case c: StreamCCByToggle[_] => {
        fn(c)
      }
      case c: FlowCCUnsafeByToggle[_] => {
        fn(c)
      }
      case c: Component => {}
    }

  }

  def check(): Unit = {
    if(Component.toplevel != toplevel) {
      constraints = new Constraints()
      toplevel = Component.toplevel
    }
  }

  def write_file[T <: Component](report: SpinalReport[T], path : String): Unit = {
    check()
    constraints.write_file(report, path)
  }
  def add_verbatim(s: => String) : Unit = {
    check()
    constraints.verbatim_constraints.append(() => s)
  }
  def add_false_path(d: Data*): Unit = {
    check()
    constraints.false_paths.appendAll(d)
  }
  def create_clock(d: Data, f: HertzNumber) = {
    check()
    constraints.clocks.append((d, f))
  }
  def set_max_skew(max_skew: TimeNumber, d: Data*) = {
    check()
    constraints.max_skews.append((d, max_skew))
  }
  def add_clock_group(asynchronous: Boolean, clks: Data*): Unit = {
    check()
    constraints.clock_groups.append((clks, asynchronous))
  }
  def set_constraints(tags: Map[String, String], ports: Data*): Unit = {
    check()
    constraints.constraints.append((ports, tags))
  }
  def set_min_delay(delay: TimeNumber, d: Data*): Unit = {
    check()
    constraints.min_delay.append((d, delay))
  }
}