package spinalextras.lib.logging

import spinal.core._
import spinal.core.sim.SimPublic
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.{Flow, Stream, master}
import spinal.lib.sim.StreamMonitor
import spinalextras.lib.bus.general.BusSlaveProvider
import spinalextras.lib.logging.FlowLoggerUtils.{FlowLoggerCCode, FlowLoggerSqlite, FlowLoggerYaml}
import spinalextras.lib.tests.WishboneGlobalBus.GlobalBus_t

import java.io.{BufferedOutputStream, File, FileOutputStream}
import java.nio.{ByteBuffer, ByteOrder}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

case class SimulationLoggerHandle(stream: Stream[Bits], yamlPath: String, defaultDir: String) {
  def startCapture(clockDomain: ClockDomain, filename: String = null): Unit = {
    import spinal.core.sim._

    val resolvedFilename = if (filename != null) filename else s"$defaultDir/GlobalLogger.bin"
    val isSqlite = resolvedFilename.endsWith(".sqlite")
    val binPath = if (isSqlite) resolvedFilename.stripSuffix(".sqlite") + ".bin" else resolvedFilename

    val out = new BufferedOutputStream(new FileOutputStream(binPath))
    stream.ready #= true

    StreamMonitor(stream, clockDomain) { payload =>
      val v = payload.toBigInt
      val buf = ByteBuffer.allocate(12).order(ByteOrder.LITTLE_ENDIAN)
      buf.putInt((v & 0xFFFFFFFFL).toInt)
      buf.putInt(((v >> 32) & 0xFFFFFFFFL).toInt)
      buf.putInt(((v >> 64) & 0xFFFFFFFFL).toInt)
      out.write(buf.array())
      out.flush()
    }

    if (isSqlite) {
      onSimEnd {
        out.close()
        GlobalLoggerSim.convertToSqlite(yamlPath, binPath, resolvedFilename)
      }
    }
  }
}

object GlobalLoggerSim {
  def convertToSqlite(yamlPath: String, binPath: String, dbPath: String): Unit = {
    import scala.sys.process._
    import java.nio.file.{Files, Paths}

    val scriptDir = Paths.get("sw", "event_logger").toAbsolutePath
    val script = scriptDir.resolve("event_logger_db.py")

    if (!Files.exists(script)) {
      println(s"Warning: event_logger_db.py not found at $script, leaving $binPath unconverted")
      return
    }
    val python3 = if (Seq("sh", "-c", "command -v python3 > /dev/null").! == 0) "python3" else null
    if (python3 == null) {
      println(s"Warning: python3 not found on PATH, leaving $binPath unconverted")
      return
    }

    new File(dbPath).delete()
    val cmd = Seq(python3, script.toString,
      new File(yamlPath).getAbsolutePath,
      "--db", new File(dbPath).getAbsolutePath,
      "--input", new File(binPath).getAbsolutePath)

    val exitCode = Process(cmd, scriptDir.toFile).!
    if (exitCode != 0) {
      println(s"Warning: event_logger_db.py exited with code $exitCode; keeping $binPath for inspection")
    } else {
      new File(binPath).delete()
    }
  }
}

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
            ctrlStreams: Option[(Stream[Bits], Flow[Bits])] = None,
            tags: Set[String] = Set(), localDepth : Int = 0): Unit = {
    val clockDomain = ctrlStreams.map(_._1.valid.clockDomain).getOrElse(ClockDomain.current)

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
      if (signals.nonEmpty && depth > 0) {
        val ctx = Component.push(Component.toplevel)
        val logger = FlowLogger(FlowLoggerConfig(localDepth = localDepth), signals)
        logger.setName(loggerName)
        logger.add_comments(comments)
        FlowLoggerYaml(logger, output_path)
        logger.create_logger_port(sysBus, address, depth, ctrlStreams)

        ctx.restore()
      } else {
        val ctx = Component.push(Component.toplevel)
        ctrlStreams.foreach(_._1.setIdle())
        ctx.restore()
      }
    }
    output_path = null
  }

  def create_logger_stream(depth: Int, ctrlStreams: (Stream[Bits], Flow[Bits]), tags: Set[String] = Set()): Unit = {
    Component.toplevel.addPrePopTask(() => {
      val sysBus: GlobalBus_t = null
      this.build(sysBus, 0, depth, "GlobalLogger", Some(ctrlStreams), tags = tags)
    })
  }

  def create_simulation_logger(tags: Set[String] = Set(), depth: Int = 128): SimulationLoggerHandle = {
    val outStream = Stream(Bits(96 bits)).setName("simLoggerStream")
    val inFlow = Flow(Bits(32 bits)).setName("simLoggerCtrl")
    inFlow.setIdle()

    SimPublic(outStream.valid)
    SimPublic(outStream.ready)
    SimPublic(outStream.payload)

    create_logger_stream(depth, ctrlStreams = (outStream, inFlow), tags = tags)

    val dir = if (output_path != null) output_path else spinal.core.GlobalData.get.config.targetDirectory
    new File(dir).mkdirs()

    SimulationLoggerHandle(outStream, yamlPath = s"$dir/GlobalLogger.logger_defs.yml", defaultDir = dir)
  }

  def create_logger_port(sysBus: BusSlaveProvider, address: BigInt, depth: Int, name: String, ctrlStreams: Option[(Stream[Bits], Flow[Bits])] = None, tags: Set[String] = Set(), localDepth : Int = 0): Unit = {
    sysBus.addPreBuildTask(() =>
      build(sysBus, address, depth, name, ctrlStreams, tags, localDepth = localDepth)
    )
    Component.toplevel.addPrePopTask(() => {
      this.build(sysBus, address, depth, name, ctrlStreams, tags, localDepth = localDepth)
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

  def create_logger_stream(depth: Int, ctrlStreams: (Stream[Bits], Flow[Bits]), tags: Set[String] = Set()): Unit = {
    get().create_logger_stream(depth, ctrlStreams = ctrlStreams, tags = tags)
  }

  def create_simulation_logger(tags: Set[String] = Set(), depth: Int = 128): SimulationLoggerHandle = {
    get().create_simulation_logger(tags = tags, depth = depth)
  }

  def create_logger_port(sysBus: BusSlaveProvider, address: BigInt, depth: Int, name: String = Component.toplevel.name + "Logger",
                         ctrlStreams: Option[(Stream[Bits], Flow[Bits])] = None, tags: Set[String] = Set(), localDepth : Int = 0): Unit = {
    get().create_logger_port(sysBus, address, depth, name = name, ctrlStreams = ctrlStreams, tags = tags, localDepth = localDepth)
  }
}