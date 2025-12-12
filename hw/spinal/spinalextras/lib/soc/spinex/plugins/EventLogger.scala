package spinalextras.lib.soc.spinex.plugins

import spinal.core.{Bits, cloneOf}
import spinal.lib.{Stream, slave}
import spinal.lib.bus.misc.SizeMapping
import spinalextras.lib.debug.JtagLoggerTap
import spinalextras.lib.logging.GlobalLogger
import spinalextras.lib.soc.spinex.{Spinex, SpinexRegisterFilePlugin}

case class EventLoggerPlugin(address: BigInt = 0xe0006000L, depth: Int = 256, name: String = "EventLogger",
                             tags: Set[String] = Set(), localDepth : Int = 0) extends SpinexRegisterFilePlugin(name, SizeMapping(address, 32)) {
  override def apply(som: Spinex): Unit = {
    if (depth > 0) {
      val jtagPlugin = som.getPlugin[JTagPlugin]

      val ctrlStreams = jtagPlugin.map(j => {
        val jtag = new JtagLoggerTap(95)
        j.jtags.append(jtag.io.jtag)
        val log_stream = slave(cloneOf(jtag.io.outStream))
        log_stream >> jtag.io.outStream
        (log_stream, jtag.io.inFlow)
      })


      GlobalLogger.create_logger_port(som.interconnect, address, depth = depth, name = name,
        ctrlStreams = ctrlStreams, tags = tags, localDepth = localDepth)
    }
  }
}