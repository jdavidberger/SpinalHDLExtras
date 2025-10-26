package spinalextras.lib.soc.spinex.plugins

import spinal.core.{Bits, cloneOf}
import spinal.lib.{Stream, slave}
import spinal.lib.bus.misc.SizeMapping
import spinalextras.lib.debug.JtagLoggerTap
import spinalextras.lib.logging.GlobalLogger
import spinalextras.lib.soc.spinex.{Spinex, SpinexRegisterFilePlugin}

case class EventLoggerPlugin(address: BigInt = 0xe0006000L, depth: Int = 256, name: String = "EventLogger",
                             tags: Set[String] = Set()) extends SpinexRegisterFilePlugin(name, SizeMapping(address, 32)) {
  override def apply(som: Spinex): Unit = {
    val jtagPlugin = som.getPlugin[JTagPlugin]

    val outputStream = jtagPlugin.map(j => {
      val jtag = new JtagLoggerTap(95)
      j.jtags.append(jtag.io.jtag)
      val log_stream = slave (cloneOf(jtag.io.log_stream))
      log_stream >> jtag.io.log_stream
      log_stream
    })

    GlobalLogger.create_logger_port(som.interconnect, address, depth = depth, name = name,
      outputStream = outputStream, tags = tags)
  }
}