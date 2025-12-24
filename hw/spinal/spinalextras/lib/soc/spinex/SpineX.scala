package spinalextras.lib.soc.spinex

import spinal.core._
import spinal.core.sim.{SimClockDomainHandlePimper, SimClockDomainPimper, sleep}
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping, SizeMapping}
import spinal.lib.bus.simple._
import spinal.lib.bus.wishbone.Wishbone
import spinal.lib.com.jtag.JtagTapInstructionCtrl
import spinal.lib.com.spi.ddr.SpiXdrMasterCtrl.XipBus
import spinal.lib.cpu.riscv.debug._
import spinalextras.lib.Config
import spinalextras.lib.bus.bus.InstructionCacheMemBusExt
import spinalextras.lib.bus.{MultiBusInterface, MultiInterconnectByTag, PipelinedMemoryBusMultiBus}
import spinalextras.lib.clocking.ClockSelection
import spinalextras.lib.lattice.IPX
import spinalextras.lib.logging.{FlowLogger, GlobalLogger, SignalLogger}
import spinalextras.lib.misc.AutoInterconnect.buildInterconnect
import spinalextras.lib.misc.ClockSpecification
import spinalextras.lib.soc.DeviceTree
import spinalextras.lib.soc.spinex.plugins.{EventLoggerPlugin, JTagPlugin}
import vexriscv.ip.InstructionCacheMemBus
import vexriscv.plugin._
import vexriscv.{ExceptionCause, VexRiscv, VexRiscvConfig}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import scala.reflect.ClassTag

case class Spinex(config : SpinexConfig = SpinexConfig.default) extends Component{
  import config._
  val self = this

  val io = new Bundle {

  }
  noIoPrefix()

  val mainClockDomain = ClockDomain.current

  val resetCtrlClockDomain = mainClockDomain.copy(
    reset = null,
    config = ClockDomainConfig(resetKind = BOOT)
  )

  val resetCtrl = new ClockingArea(resetCtrlClockDomain) {
    val mainClkResetUnbuffered  = False

    val systemClkReset = Timeout(1 us)
    when(!systemClkReset) {
      mainClkResetUnbuffered := True
    }

    when(mainClockDomain.isResetActive) {
      systemClkReset.clear()
      mainClkResetUnbuffered := True
    }

    val systemReset  = RegNext(mainClkResetUnbuffered, init = True)
  }


  val systemClockDomain = mainClockDomain.copy(
    reset = resetCtrl.systemReset,
  )

  val debugClockDomain = systemClockDomain

  var interconnect, directInterconnect: MultiInterconnectByTag = null

  val system = new ClockingArea(systemClockDomain) {
    interconnect = new MultiInterconnectByTag("spinex_interconnect")
    directInterconnect = new MultiInterconnectByTag("spinex_directInterconnect")

    val pipelinedMemoryBusConfig = PipelinedMemoryBusConfig(
      addressWidth = 32,
      dataWidth = 32
    )

    if(config.withJtag) {
      cpuPlugins += //new DebugPlugin(debugClockDomain, hardwareBreakpointCount),
        new EmbeddedRiscvJtag(
          p = DebugTransportModuleParameter(
            addressWidth = 7,
            version      = 1,
            idle         = 7
          ),
          debugCd = debugClockDomain,
          withTunneling = false,
          withTap = true
        )
    }

    //Instanciate the CPU
    val cpu = new VexRiscv(
      config = VexRiscvConfig(
        plugins = cpuPlugins,
      )
    )

    //Checkout plugins used to instanciate the CPU to connect them to the SoC
    val timerInterrupt = False
    private val externalInterrupts = Bits(32 bits)
    externalInterrupts := 0

    var interruptInfos = new mutable.HashMap[Int, Data]()
    def addInterrupt(signal : Data, requestedIrq : Int = -1) = {
      val rtn = if (requestedIrq > -1) requestedIrq else {
        (0 until 32).filter(i => !interruptInfos.contains(i)).head
      }
      assert(!interruptInfos.contains(rtn))

      interruptInfos(rtn) = signal
      externalInterrupts(rtn, signal.getBitsWidth bits) := signal.asBits

      rtn
    }

    def addNamedInterrupt(name : String, signal : Data, requestedIrq : Int = -1) = {
      addInterrupt(CombInit(signal).setName(name, weak = true), requestedIrq)
    }

    for(plugin <- cpu.plugins) plugin match{
      case plugin : IBusCachedPlugin =>
        add_master(plugin.iBus)
      case plugin : IBusSimplePlugin =>
        add_master(plugin.iBus)
      case plugin : DBusSimplePlugin => {
        add_master(plugin.dBus.cmdHalfPipe().setName("dBus_staged"))
        //add_master(plugin.dBus.cmdS2mPipe().setName("dBus_staged"))
      }
      case plugin : CsrPlugin        => {
        plugin.timerInterrupt := timerInterrupt
        withAutoPull()

        if(plugin.config.withPrivilegedDebug) {
          GlobalLogger(Set("cpu"),
            FlowLogger.flows(plugin.debugBus.dmToHart),
            FlowLogger.flows(plugin.debugBus.hartToDm),
          )
        }

        GlobalLogger(Set("cpu"),
          FlowLogger.flows(plugin.exceptionPortsInfos.map({ ec =>
            class ExceptionCauseWithPC() extends ExceptionCause(ec.port.codeWidth) {
              val pc = UInt(32 bits)
            }

            val rtn = Flow(new ExceptionCauseWithPC())
            rtn.payload.pc := ec.stage.input(plugin.pipeline.config.PC)
            rtn.payload.badAddr := ec.port.badAddr
            rtn.payload.code := ec.port.code
            rtn.valid := ec.port.valid
            rtn.setName(ec.port.refOwner.toString + "_" + ec.port.name + "WithPC")
            rtn
          }):_*)
        )
      }
      case plugin : ExternalInterruptArrayPlugin => {
        plugin.externalInterruptArray := externalInterrupts
      }
      case plugin : DebugModule => {
        GlobalLogger(Set("cpu"),
          FlowLogger.flows(plugin.io.harts.map(_.hartToDm):_*),
          FlowLogger.flows(plugin.io.harts.map(_.dmToHart):_*)
        )
      }
      case plugin : DebugPlugin         => plugin.debugClockDomain{
        resetCtrl.systemReset setWhen(RegNext(plugin.io.resetOut))
        getPlugin[JTagPlugin].get.jtags.append(plugin.io.bus.fromJtag())
      }
      case plugin : EmbeddedRiscvJtag => {
        getPlugin[JTagPlugin].get.jtags.append(plugin.jtag)
      }
      case _ =>
    }

    //******** APB peripherals *********
    val apbMapping = ArrayBuffer[(Apb3, SizeMapping)]()
  }

  def getPlugins[T](implicit tag: ClassTag[T]) : Seq[T] = {
    config.plugins.filter(tag.runtimeClass.isInstance(_)).map(_.asInstanceOf[T])
  }
  def getPlugin[T](implicit tag: ClassTag[T]) = getPlugins[T](tag).headOption

  val clockingAreaBuild = new ClockingArea(systemClockDomain) {
    import system._

    config.plugins.foreach(x => x(self))

    val apbRegion = SizeMapping(0xe0000000L, 0x4000)
    apbMapping.foreach(m => {
      require(apbRegion.size >= m._2.end)
    })

    val largestAddressWidth = apbMapping.map(x => log2Up(x._2.end)).max
    val apbBridge = new PipelinedMemoryBusToApbBridge(
      apb3Config = Apb3Config(
        addressWidth = largestAddressWidth,
        dataWidth = 32
      ),
      pipelineBridge = pipelineApbBridge,
      pipelinedMemoryBusConfig = pipelinedMemoryBusConfig
    )
    add_slave(apbBridge.io.pipelinedMemoryBus, "apbBridge", apbRegion, "dBus")

    //******** Memory mappings *********
    val apbDecoder = Apb3Decoder(
      master = apbBridge.io.apb,
      slaves = apbMapping
    )

    val stagedBridge = PipelinedMemoryBus(32, 32).setName("interconnect")
    interconnect.addMaster(PipelinedMemoryBusMultiBus(stagedBridge.cmdM2sPipe().cmdS2mPipe().rspPipe().setName("interconnect_staged")), "dBus")
    directInterconnect.addSlave(PipelinedMemoryBusMultiBus(stagedBridge), DefaultMapping, "dBus")

    GlobalLogger(Set("soc"),
      SignalLogger.concat("interrupts", interruptInfos.values.toSeq)
    )

    Component.toplevel.addPrePopTask(() => {
      directInterconnect.build()
      interconnect.build()
    })
  }

  import spinalextras.lib.bus.bus._

  def add_master(bus: IBusSimpleBus): Unit = {
    interconnect.addMaster(bus, "iBus")

    assert(!bus.cmd.valid.isUnknown, "Invalid ibus valid")
    when(bus.cmd.valid) {
      assert(!bus.cmd.payload.asBits.isUnknown, "Invalid sbus address")
    }

    GlobalLogger(Set("iBus"),
      FlowLogger.streams(bus.cmd.setName("iBus_cmd")),
      FlowLogger.flows(bus.rsp.setName("iBus_rsp"))
    )
  }

  def add_master(bus: InstructionCacheMemBus): Unit = {
    directInterconnect.addMaster(bus, "iBus")

    GlobalLogger(Set("iBus"),
      FlowLogger.streams(bus.cmd.setName("iBus_cmd")),
      FlowLogger.flows(bus.rsp.setName("iBus_rsp"))
    )
  }

  def add_master(bus: DBusSimpleBus): Unit = new Composite(this, "add_master") {
    directInterconnect.addMaster(bus, "dBus")
    val dbusRspFlow = Flow(cloneOf(bus.rsp))
    dbusRspFlow.valid := bus.rsp.ready
    dbusRspFlow.payload := bus.rsp

    assert(!bus.cmd.valid.isUnknown, "Invalid dbus valid")
    when(bus.cmd.valid) {
      assert(!bus.cmd.payload.address.asBits.isUnknown, "Invalid dbus address")
      assert(!bus.cmd.payload.wr.isUnknown, "Invalid dbus wr")
      when(bus.cmd.payload.wr) {
        assert(!bus.cmd.payload.data.asBits.isUnknown, "Invalid dbus data")
      }
    }

    GlobalLogger(Set("dBus"),
      FlowLogger.streams(bus.cmd.setName("dBus")),
      FlowLogger.flows(dbusRspFlow.setName("dbusRspFlow"))
    )
  }

  def add_slave(bus: PipelinedMemoryBus, name : String, mapping : AddressMapping, direct : Boolean, tags : String*): Unit = {
    bus.setWeakName(name)
    if(direct) {
      directInterconnect.addSlave(PipelinedMemoryBusMultiBus(bus), mapping = mapping, tags: _*)
    } else {
      interconnect.addSlave(PipelinedMemoryBusMultiBus(bus), mapping = mapping, tags: _*)
    }
  }

  def add_slave(bus: PipelinedMemoryBus, name : String, mapping : AddressMapping, tags : String*): Unit = {
    add_slave(bus, name, mapping, false, tags:_*)
  }

  def add_slave(bus: XipBus, name : String, mapping : AddressMapping, tags : String*): Unit = {
    directInterconnect.addSlave(bus, mapping = mapping, tags:_*)
  }

  def add_slave(bus: MultiBusInterface, name : String, mapping : AddressMapping, direct : Boolean, tags : String*): Unit = {
    if(direct) {
      directInterconnect.addSlave(bus, mapping, tags = tags:_*)
    } else {
      interconnect.addSlave(bus, mapping, tags = tags:_*)
    }
  }

  def add_slave(bus: Wishbone, name : String, mapping : AddressMapping, tags : String*): Unit = {
    add_slave(bus, name, mapping, false, tags:_*)
  }

}


object Spinex{
  def main(args: Array[String]) {
    val report = Config.spinal.generateVerilog(Spinex(SpinexConfig.default))
    IPX.generate_ipx(report)
  }
}

class SpinexWithClock extends Component {
  val io = new Bundle {
    val led = out(Bool())
  }
  noIoPrefix()
  withAutoPull()

  val clocks = new ClockSelection(Seq(ClockSpecification(100 MHz)))
  val spinexClockDomain = clocks.ClockDomains.last
  //val spinexClockDomain = rst_sync(ClockDomain.current)

  var connectionArea = new ClockingArea(clockDomain = spinexClockDomain) {
    val som = Spinex(SpinexConfig.default.withPlugins(EventLoggerPlugin()))

    val counter = Timeout(1 sec)
    val led = RegInit(False)
    when(counter) {
      counter.clear()
      led := ~led
    }
  }

  io.led := connectionArea.led
  buildInterconnect(Seq(connectionArea.som), io)
}

object SpinexWithClock{
  def main(args: Array[String]) {
    val report = Config.spinal.copy(
      targetDirectory = s"hw/gen/SpinexWithClock",
      defaultClockDomainFrequency = FixedFrequency(60 MHz)
    ).generateVerilog(new SpinexWithClock())

    IPX.generate_ipx(report)
    DeviceTree.generate(report)
  }
}

object SpinexWithClockTestBench {
  def main(args: Array[String]) {
    val report = Config.spinal.copy(defaultClockDomainFrequency = FixedFrequency(60 MHz)).generateVerilog(new SpinexWithClock())

    Config.sim.addIncludeDir("hw/verilog/opencores_i2c/rtl/verilog/").withConfig(
      Config.spinal.includeSimulation.copy(
        defaultClockDomainFrequency = FixedFrequency(60 MHz),
        device = Device(vendor = "?", family = "?"),
      )
    ).doSim(new SpinexWithClock) { dut =>
      dut.clockDomain.forkStimulus(60 MHz)
      dut.clockDomain.waitSampling(1)
      sleep(3005 ns)
      dut.clockDomain.assertReset()
      sleep(150 ns)
      dut.clockDomain.deassertReset()
      dut.clockDomain.waitSampling(20)

      dut.clockDomain.waitSampling(300)

      val jtag_clk = dut.getAllIo.find(_.name == "jtag_tck").get.asInstanceOf[Bool]
      ClockDomain(jtag_clk).forkStimulus(9 MHz)

      dut.clockDomain.waitSampling(300)
      sleep(3005 ns)
      dut.clockDomain.assertReset()
      sleep(150 ns)
      dut.clockDomain.deassertReset()
      dut.clockDomain.waitSampling(20)


      dut.clockDomain.waitSampling(1500)
    }
  }
}
