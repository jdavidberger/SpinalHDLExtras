package spinalextras.lib.logging

import spinal.core.{ASYNC, Bits, ClockDomain, ClockingArea, Component, Data, assert}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.{Flow, Stream, master}
import spinalextras.lib.bus.general.BusSlaveProvider
import spinalextras.lib.logging.FlowLoggerUtils.{FlowLoggerCCode, FlowLoggerSqlite, FlowLoggerYaml}
import spinalextras.lib.tests.WishboneGlobalBus.GlobalBus_t

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class GlobalLogger {
  val signals = new mutable.ArrayBuffer[(Data, Flow[Bits], ClockDomain, Set[String])]()
  val comments = new ArrayBuffer[String]()
  var built = false
  var topComponent = {
    val topComponent = Component.toplevel
    topComponent
  }

  def topify(signal: Flow[Bits]): Flow[Bits] = {
    if (Component.current == Component.toplevel) {
      return signal
    }
    var name = signal.name
    var intermediate_bus = signal
    var new_bus: Option[Flow[Bits]] = None

    var c = Component.current
    do {
      val ctx = Component.push(c)

      val next_bus = master(signal.clone()).setName(s"logger_${name}")

      if (new_bus.isEmpty) {
        new_bus = Some(next_bus)
      }

      intermediate_bus <> next_bus

      intermediate_bus = next_bus
      ctx.restore()
      c = c.parent
    } while (c != Component.toplevel && c != null)
    assert(c != null)

    intermediate_bus
  }

  def add(tags: Set[String], s: (Data, Flow[Bits])*): Unit = {
    if (built) {
      try {
        println(s"Warning: ${s} with tags ${tags} was added after the logger was created")
      } catch {
        case a : NullPointerException =>  println(s"Warning: Unnamed components with tags ${tags} was added after the logger was created")
      }

      return
    }
    assert(ClockDomain.current != null)
    for (elem <- s) {
      assert(elem._1.name != null && elem._1.name.nonEmpty)
      //assert(elem._2.name != null && elem._2.name.size > 0)
    }
    signals.appendAll(s.map(x => (x._1, topify(x._2), ClockDomain.current, tags)))
  }

  var output_path: String = null

  def set_output_path(fn: String): Unit = {
    output_path = fn
  }

  def build(sysBus: BusSlaveProvider, address: BigInt, depth: Int, name: String,
            outputStream: Option[Stream[Bits]] = None,
            tags: Set[String] = Set()): Unit = {
    val clockDomain = outputStream.map(_.valid.clockDomain).getOrElse(ClockDomain.current)

    if (built) {
      return
    }
    if (output_path == null) {
      output_path = spinal.core.GlobalData.get.config.targetDirectory
    }
    val signals = this.signals.filter(s => {
      s._4.intersect(tags).nonEmpty || tags.isEmpty
    }).map(s => (s._1, s._2, s._3))

    built = true;
    val loggerName = name
    new ClockingArea(clockDomain) {
      if (signals.nonEmpty) {
        val ctx = Component.push(Component.toplevel)
        val logger = FlowLogger(signals)
        logger.setName(loggerName)
        logger.add_comments(comments)
        FlowLoggerCCode(logger, output_path)
        FlowLoggerSqlite(logger, output_path)
        FlowLoggerYaml(logger, output_path)
        logger.create_logger_port(sysBus, address, depth, outputStream)

        ctx.restore()
      } else {
        outputStream.foreach(_.setIdle())
      }
    }
    output_path = null
  }

  def create_logger_stream(depth: Int, outputStream: Stream[Bits], tags: Set[String] = Set()): Unit = {
    Component.toplevel.addPrePopTask(() => {
      val sysBus: GlobalBus_t = null
      this.build(sysBus, 0, depth, "GlobalLogger", Some(outputStream), tags = tags)
    })
  }

  def create_logger_port(sysBus: BusSlaveProvider, address: BigInt, depth: Int, name: String, outputStream: Option[Stream[Bits]] = None, tags: Set[String] = Set()): Unit = {
    sysBus.addPreBuildTask(() =>
      build(sysBus, address, depth, name, outputStream, tags)
    )
    Component.toplevel.addPrePopTask(() => {
      this.build(sysBus, address, depth, name, outputStream, tags)
    })
  }


  def add_comment(str: String) = {
    comments += str
  }
}

object GlobalLogger {
  var sysLogger: Option[GlobalLogger] = None

  def get(): GlobalLogger = {
    if (sysLogger.isEmpty || sysLogger.get.topComponent != Component.toplevel) {
      sysLogger = Some(new GlobalLogger())
    }
    sysLogger.get
  }

  def add_comment(str: String): Unit = {
    get().add_comment(str)
  }

  def apply(tags: Set[String], signals: Seq[(Data, Flow[Bits])]*): Unit = {
    signals.foreach(x => get().add(tags = tags, x: _*))
  }

  def apply(signals: Seq[(Data, Flow[Bits])]*): Unit = {
    signals.foreach(x => get().add(tags = Set(), x: _*))
  }

  def set_output_path(fn: String): Unit = {
    get().set_output_path(fn)
  }

  def create_logger_stream(depth: Int, outputStream: Stream[Bits], tags: Set[String] = Set()): Unit = {
    get().create_logger_stream(depth, outputStream = outputStream, tags = tags)
  }

  def create_logger_port(sysBus: BusSlaveProvider, address: BigInt, depth: Int, name: String = Component.toplevel.name + "Logger",
                         outputStream: Option[Stream[Bits]] = None, tags: Set[String] = Set()): Unit = {
    get().create_logger_port(sysBus, address, depth, name = name, outputStream = outputStream, tags = tags)
  }
}