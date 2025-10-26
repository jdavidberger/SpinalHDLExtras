package spinalextras.lib.soc.spinex

import spinal.core._
import spinal.core.sim.{SimClockDomainHandlePimper, SimClockDomainPimper, sleep}
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.misc.{AddressMapping, SizeMapping}
import spinal.lib.bus.simple._
import spinal.lib.bus.wishbone.Wishbone
import spinal.lib.com.jtag.JtagTapInstructionCtrl
import spinal.lib.com.spi.ddr.SpiXdrMasterCtrl.XipBus
import spinal.lib.cpu.riscv.debug._
import spinalextras.lib.Config
import spinalextras.lib.bus.{MultiInterconnectByTag, PipelinedMemoryBusMultiBus}
import spinalextras.lib.clocking.rst_sync
import spinalextras.lib.lattice.IPX
import spinalextras.lib.logging.{FlowLogger, GlobalLogger}
import spinalextras.lib.misc.AutoInterconnect.buildInterconnect
import spinalextras.lib.soc.DeviceTree
import spinalextras.lib.soc.spinex.plugins.{EventLoggerPlugin, JTagPlugin}
import vexriscv.ip.InstructionCacheMemBus
import vexriscv.plugin._
import vexriscv.{VexRiscv, VexRiscvConfig}

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

  val jtagNative = withNativeJtag generate new ClockingArea(debugClockDomain){
    val jtagCtrl = JtagTapInstructionCtrl()
    val tap = jtagCtrl.fromXilinxBscane2(userId = 2)
  }

  val mainClockDomain = ClockDomain.current

  val resetCtrlClockDomain = mainClockDomain.copy(
    reset = null,
    config = ClockDomainConfig(
      resetKind = BOOT
    )
  )

  val resetCtrl = new ClockingArea(resetCtrlClockDomain) {
    val mainClkResetUnbuffered  = False

    val systemClkResetCounter = Reg(UInt(6 bits)) init(0)
    when(systemClkResetCounter =/= U(systemClkResetCounter.range -> true)){
      systemClkResetCounter := systemClkResetCounter + 1
      mainClkResetUnbuffered := True
    }

    when(BufferCC(mainClockDomain.readResetWire)){
      systemClkResetCounter := 0
      mainClkResetUnbuffered := True
    }

    //Create all reset used later in the design
    val mainClkReset = RegNext(mainClkResetUnbuffered, init = True)
      val systemReset  = RegNext(mainClkResetUnbuffered, init = True)
  }


  val systemClockDomain = mainClockDomain.copy(
    reset = resetCtrl.systemReset,
  )

  val debugClockDomain = mainClockDomain.copy(
    reset = resetCtrl.mainClkReset,
  )

  var interconnect: MultiInterconnectByTag = null

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
    interconnect.addMaster(bus, "iBus")
    GlobalLogger(Set("iBus"),
      FlowLogger.streams(bus.cmd.setName("iBus_cmd")),
      FlowLogger.flows(bus.rsp.setName("iBus_rsp"))
    )
  }

  def add_master(bus: DBusSimpleBus): Unit = new Composite(this, "add_master") {
    interconnect.addMaster(bus, "dBus")
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

  def add_slave(bus: PipelinedMemoryBus, name : String, mapping : AddressMapping, tags : String*): Unit = {
    bus.setWeakName(name)
    interconnect.addSlave(PipelinedMemoryBusMultiBus(bus), mapping = mapping, tags:_*)
  }

  def add_slave(bus: XipBus, name : String, mapping : AddressMapping, tags : String*): Unit = {
    interconnect.addSlave(bus, mapping = mapping, tags:_*)
  }

  def add_slave(bus: Wishbone, name : String, mapping : AddressMapping, tags : String*): Unit = {
    interconnect.addSlave(bus, mapping = mapping, tags:_*)
  }

  val system = new ClockingArea(systemClockDomain) {
    interconnect = new MultiInterconnectByTag()

    val pipelinedMemoryBusConfig = PipelinedMemoryBusConfig(
      addressWidth = 32,
      dataWidth = 32
    )

    //Instanciate the CPU
    val cpu = new VexRiscv(
      config = VexRiscvConfig(
        plugins = cpuPlugins += //new DebugPlugin(debugClockDomain, hardwareBreakpointCount)
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

    for(plugin <- cpu.plugins) plugin match{
      case plugin : IBusCachedPlugin =>
        add_master(plugin.iBus)
      case plugin : IBusSimplePlugin =>
        add_master(plugin.iBus)
      case plugin : DBusSimplePlugin => {
        add_master(plugin.dBus.cmdHalfPipe().setName("dBus_staged"))
        //add_master(plugin.dBus)
      }
      case plugin : CsrPlugin        => {
        plugin.timerInterrupt := timerInterrupt
      }
      case plugin : ExternalInterruptArrayPlugin => {
        plugin.externalInterruptArray := externalInterrupts
      }
      case plugin : DebugPlugin         => plugin.debugClockDomain{
        resetCtrl.systemReset setWhen(RegNext(plugin.io.resetOut))
        if (withNativeJtag) {
          jtagNative.jtagCtrl <> plugin.io.bus.fromJtagInstructionCtrl(ClockDomain(jtagNative.tap.TCK),0)
        } else {
          getPlugin[JTagPlugin].get.jtags.append(plugin.io.bus.fromJtag())
        }
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

    Component.toplevel.addPrePopTask(() => {
      interconnect.build()
    })
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

  //val clocks = new ClockSelection(ClockDomain.current.frequency, Seq(ClockSpecification(80 MHz)))
  //val spinexClockDomain = clocks.ClockDomains.last
  val spinexClockDomain = rst_sync(ClockDomain.current)

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
    val report = Config.spinal.copy(defaultClockDomainFrequency = FixedFrequency(60 MHz)).generateVerilog(new SpinexWithClock())

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
