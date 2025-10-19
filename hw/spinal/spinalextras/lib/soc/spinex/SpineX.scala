package spinalextras.lib.soc.spinex

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.misc.{AddressMapping, SizeMapping}
import spinal.lib.bus.simple._
import spinal.lib.bus.wishbone.Wishbone
import spinal.lib.com.jtag.{Jtag, JtagTapInstructionCtrl}
import spinal.lib.com.spi.ddr.SpiXdrMasterCtrl.XipBus
import spinal.lib.cpu.riscv.debug._
import spinalextras.lib.Config
import spinalextras.lib.blackbox.lattice.lifcl.PLL
import spinalextras.lib.bus.{MultiInterconnectByTag, PipelinedMemoryBusMultiBus}
import spinalextras.lib.lattice.IPX
import spinalextras.lib.logging.{FlowLogger, GlobalLogger}
import spinalextras.lib.misc.AutoInterconnect.buildInterconnect
import spinalextras.lib.misc.ClockSpecification
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
    val jtag = ifGen(!config.withNativeJtag) (slave(Jtag()))
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

    //Implement an counter to keep the reset axiResetOrder high 64 cycles
    // Also this counter will automatically do a reset when the system boot.
    val systemClkResetCounter = Reg(UInt(6 bits)) init(0)
    when(systemClkResetCounter =/= U(systemClkResetCounter.range -> true)){
      systemClkResetCounter := systemClkResetCounter + 1
      mainClkResetUnbuffered := True
    }

    when(BufferCC(mainClockDomain.readResetWire)){
      systemClkResetCounter := 0
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

    var currentInterruptIdx = 0
    var interruptInfos = new mutable.ArrayBuffer[(Data, Int)]()
    def addInterrupt(signal : Data) = {
      interruptInfos.append((signal, currentInterruptIdx))
      val rtn = currentInterruptIdx
      externalInterrupts(currentInterruptIdx, signal.getBitsWidth bits) := signal.asBits
      currentInterruptIdx = currentInterruptIdx + signal.getBitsWidth
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
          io.jtag <> plugin.io.bus.fromJtag()
        }
      }
      case plugin : EmbeddedRiscvJtag => {
        plugin.jtag <> io.jtag
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

    interconnect.build()
  }
}


object Spinex{
  def main(args: Array[String]) {
    val report = Config.spinal.generateVerilog(Spinex(SpinexConfig.default.copy(withWishboneBus = true)))
    IPX.generate_ipx(report)
  }
}

class SpinexWithClock extends Component {
  val io = new Bundle {
    val led = out(Bool())
  }
  noIoPrefix()

  val ClockDomains = PLL(
    ClockSpecification(80 MHz),
  )


  var connectionArea = new ClockingArea(clockDomain = ClockDomains.head) {
    val som = Spinex(SpinexConfig.default.copy(withWishboneBus = false))

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
    val report = Config.spinal.generateVerilog(new SpinexWithClock())

    IPX.generate_ipx(report)
  }
}
