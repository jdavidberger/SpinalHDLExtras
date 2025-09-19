package spinalextras.lib.soc.spinex

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.misc.{AddressMapping, SizeMapping}
import spinal.lib.bus.simple._
import spinal.lib.bus.wishbone.{AddressGranularity, Wishbone, WishboneConfig}
import spinal.lib.com.jtag.{Jtag, JtagTapInstructionCtrl}
import spinal.lib.com.spi.ddr.SpiXdrMasterCtrl.XipBus
import spinal.lib.com.spi.ddr._
import spinal.lib.com.uart._
import spinal.lib.cpu.riscv.debug._
import spinalextras.lib.{Config, Memories, MemoryRequirement}
import spinalextras.lib.blackbox.lattice.lifcl.{OSCD, OSCDConfig, PLL}
import spinalextras.lib.blackbox.memories.W25Q128JVxIM_quad
import spinalextras.lib.blackbox.opencores.i2c_master_top
import spinalextras.lib.bus.{MultiInterconnectByTag, PipelinedMemoryBusExt, PipelinedMemoryBusMultiBus, WishboneToPipelinedMemoryBus}
import spinalextras.lib.io.TristateBuffer
import spinalextras.lib.lattice.IPX
import spinalextras.lib.logging.{FlowLogger, GlobalLogger}
import spinalextras.lib.misc.AutoInterconnect.buildInterconnect
import spinalextras.lib.misc.ClockSpecification
import spinalextras.lib.soc.peripherals.{SpinexApb3Timer, SpinexApb3UartCtrl}
import vexriscv.demo.MuraxPipelinedMemoryBusRam
import vexriscv.ip.{InstructionCacheConfig, InstructionCacheMemBus}
import vexriscv.plugin.CsrAccess.WRITE_ONLY
import vexriscv.plugin._
import vexriscv.{VexRiscv, VexRiscvConfig, plugin}

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

case class Spinex(config : SpinexConfig = SpinexConfig.default) extends Component{
  import config._

  val wbConfig = WishboneConfig(32, 32, useERR = true, useRTY = false, selWidth = 4, addressGranularity = AddressGranularity.BYTE).withBurstType
  val io = new Bundle {
    val jtag = ifGen(!config.withNativeJtag) (slave(Jtag()))

    //Peripherals IO
    val uart = master(Uart())

    val i2c0_scl = inout(Analog(Bool()))
    val i2c0_sda = inout(Analog(Bool()))

    val spiflash_clk = genXip generate out(Bool())
    val spiflash_cs_n = genXip generate out(Bool())
    val spiflash_dq = genXip generate inout(Analog(Bits(xipConfig.ctrl.spi.dataWidth bits)))

    val wb = config.withWishboneBus generate master(Wishbone(wbConfig))
    val externalInterrupts = in(Bits(config.externalInterrupts bits)) default(0)
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

    val bigEndianDBus = config.cpuPlugins.exists(_ match{ case plugin : DBusSimplePlugin => plugin.bigEndian case _ => false})

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
    val externalInterrupts = Bits(32 bits)
    externalInterrupts := 0

    externalInterrupts(0, config.externalInterrupts bits) := io.externalInterrupts

    val usb32_irq_loc = 0
    val timer0_irq_loc = 1
    val uart_irq_loc = 2
    val framectrl = 4
    val i2s_rx = 6
    val i2s_tx = 7

    if(config.withWishboneBus) {
      add_slave(io.wb, "wb0", SizeMapping(0xb0000000L, 0x10000000L), "dBus")
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



    //****** MainBus slaves ********
//    val mainBusMapping = ArrayBuffer[(PipelinedMemoryBus,SizeMapping)]()
//    val ram = new MuraxPipelinedMemoryBusRam(
//      onChipRamSize = onChipRamSize,
//      onChipRamHexFile = onChipRamHexFile,
//      pipelinedMemoryBusConfig = pipelinedMemoryBusConfig,
//      bigEndian = bigEndianDBus
//    )
    val mem = Memories(MemoryRequirement(Bits(32 bits), onChipRamSize / 4,
      numReadWritePorts = 2,
      needsMask = true))
    val mem_pmbs = mem.pmbs()
    add_slave(mem_pmbs(1).resizeAddress(32), "ram", SizeMapping(0x40000000l, onChipRamSize), "dBus")
    add_slave(mem_pmbs(0).resizeAddress(32), "ram", SizeMapping(0x40000000l, onChipRamSize), "iBus")
//    mainBusMapping += ram.io.bus -> (0x80000000l, onChipRamSize)

    val apbBridge = new PipelinedMemoryBusToApbBridge(
      apb3Config = Apb3Config(
        addressWidth = 20,
        dataWidth = 32
      ),
      pipelineBridge = pipelineApbBridge,
      pipelinedMemoryBusConfig = pipelinedMemoryBusConfig
    )
    add_slave(apbBridge.io.pipelinedMemoryBus, "apbBridge", SizeMapping(0xe0000000L, 0x5000), "dBus")


    //******** APB peripherals *********
    val apbMapping = ArrayBuffer[(Apb3, SizeMapping)]()

    val uartCtrl = SpinexApb3UartCtrl(uartCtrlConfig)
    uartCtrl.io.uart <> io.uart
    externalInterrupts(uart_irq_loc) := (uartCtrl.io.interrupt)
    apbMapping += uartCtrl.io.apb  -> (0x1800, 1 kB)

    val timer = new SpinexApb3Timer(0x2800)
    timerInterrupt setWhen(timer.io.interrupt)
    externalInterrupts(timer0_irq_loc) := (timer.io.interrupt)
    apbMapping += timer.io.apb     -> (timer.baseAddress, 1 kB)

    val i2cCtrl = new i2c_master_top()
    i2cCtrl.attachi2c(io.i2c0_scl, io.i2c0_sda)

    add_slave(i2cCtrl.io.wb, "i2c", SizeMapping(0xe0005000L, 32 Bytes), "dBus")

    val xip = ifGen(genXip)(new Area{
      val ctrl = Apb3SpiXdrMasterCtrl(xipConfig)

      apbMapping += ctrl.io.apb     -> (0x0, 1 kB)

      add_slave(ctrl.io.xip, "xip", SizeMapping(0x20000000L, 0x01000000), "iBus", "dBus")

      val buffers = ctrl.io.spi.data.map(_ => TristateBuffer())

      for(i <- io.spiflash_dq.bitsRange) {
        val (phy, tristate, xdr) = (io.spiflash_dq(i), buffers(i), ctrl.io.spi.data(i))
        tristate.io.output_enable := xdr.writeEnable
        tristate.io.input := xdr.write(0)
        xdr.read(0) := RegNext(RegNext(tristate.io.output))
        tristate.io.phy <> phy
      }

      io.spiflash_clk := ctrl.io.spi.sclk.write(0)
      io.spiflash_cs_n := ctrl.io.spi.ss(0)
    })

    print(apbMapping.toSeq)
    //******** Memory mappings *********
    val apbDecoder = Apb3Decoder(
      master = apbBridge.io.apb,
      slaves = apbMapping.toSeq
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
