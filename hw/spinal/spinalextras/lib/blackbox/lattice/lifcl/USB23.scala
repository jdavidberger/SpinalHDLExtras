package spinalextras.lib.blackbox.lattice.lifcl

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinalextras.lib.Config

import scala.language.postfixOps

case class USB23_IO() extends Bundle with IMasterSlave {
  // Passthrough ports from the USB core to the outside world:
  val VBUS          = Analog(Bits(1 bits))
  val REFINCLKEXTP  = Bits(1 bits)
  val REFINCLKEXTM  = Bits(1 bits)
  val RESEXTUSB2    = Analog(Bits(1 bits))
  val DP            = Analog(Bits(1 bits))
  val DM            = Analog(Bits(1 bits))
  val RXM           = Bits(1 bits)
  val RXP           = Bits(1 bits)
  val TXM           = Bits(1 bits)
  val TXP           = Bits(1 bits)

  override def asMaster() : Unit = {
    in(REFINCLKEXTM, REFINCLKEXTP, RXM, RXP)
    out (TXM, TXP)
    inout(VBUS, DM, DP,  RESEXTUSB2)
  }
}

object USB23 extends Enumeration {
  type Modes = Value
  val USB3, USB2, USB23 = Value
}

case class USB23(GSR: Boolean = false) extends BlackBox {
  val usb_mode : USB23.Modes = USB23.USB3
  val generic = new Generic {
    val USB_MODE = USB23.this.usb_mode
    val GSR = if(USB23.this.GSR) "ENABLED" else "DISABLED"
  }

  val io = new Bundle {
    val PHY    = master(USB23_IO())
    PHY.setPartialName("")

    // Following are assigned to constants
    val XOIN18              = in Bool() default(False)
    val XOOUT18             = out Bool()
    val USBPHY_REFCLK_ALT   = in Bool() default(ClockDomain.current.readClockWire)
    val XMCSYSREQ           = in Bool() default(False)
    val XMCSYSACK           = out Bool()
    val XMCACTIVE           = out Bool()
    val ID                  = in Bool() default(True)
    val STARTRXDETU3RXDET   = in Bool() default(False)
    val DISRXDETU3RXDET     = in Bool() default(False)
    val SS_RX_ACJT_EN       = in Bool() default(False)
    val SS_RX_ACJT_ININ     = in Bool() default(False)
    val SS_RX_ACJT_INIP     = in Bool() default(False)
    val SS_RX_ACJT_INIT_EN  = in Bool() default(False)
    val SS_RX_ACJT_MODE     = in Bool() default(False)
    val SS_TX_ACJT_DRVEN    = in Bool() default(False)
    val SS_TX_ACJT_DATAIN   = in Bool() default(False)
    val SS_TX_ACJT_HIGHZ    = in Bool() default(False)
    val SCANEN_CTRL         = in Bool() default(False)
    val SCANEN_USB3PHY      = in Bool() default(False)
    val SCANEN_CGUSB3PHY    = in Bool() default(False)
    val SCANEN_USB2PHY      = in Bool() default(False)
    val RESEXTUSB3          = out Bool() default(False)
    val DISRXDETU3RXDETACK  = out Bool() default(False)
    val SS_RX_ACJT_OUTN     = out Bool() default(False)
    val SS_RX_ACJT_OUTP     = out Bool() default(False)

    // Clocks, resets
    val USB3_MCUCLK         = in Bool() default(ClockDomain.current.readClockWire)
    val USB_SUSPENDCLK      = in Bool() default(ClockDomain.current.readClockWire)
    val USB3_SYSRSTN        = in Bool() default(~ClockDomain.current.isResetActive)
    val USB_RESETN          = in Bool() default(~ClockDomain.current.isResetActive)
    val USB2_RESET          = in Bool() default(ClockDomain.current.isResetActive)
    val INTERRUPT           = out Bool()

    // LMMI
    val LMMICLK             = in Bool() default(ClockDomain.current.readClockWire)
    val LMMIRESETN          = in Bool() default(~ClockDomain.current.isResetActive)
    val LMMIREQUEST         = in Bool() default(False)
    val LMMIWRRD_N          = in Bool() default(False)
    val LMMIOFFSET          = in Bits(15 bits)
    val LMMIWDATA           = in Bits(32 bits)
    val LMMIRDATAVALID      = out Bool()
    val LMMIREADY           = out Bool()
    val LMMIRDATA           = out Bits(32 bits)

    val axiMConfig = Axi4Config(
      addressWidth    = 32,
      dataWidth       = 64,
      idWidth         = 8,
      useRegion       = false,
      useId           = true,
      useLock         = true,
      useQos          = false,
      useResp         = true, //false,
      useProt         = true,
      useStrb         = true
    )

    val XM         = master(Axi4(axiMConfig))
    for((name, elements) <- XM.elements) {
      val bundle = elements.asInstanceOf[Stream[Bundle]]
      for((signal_name, signal) <- bundle.payload.elements) {
        signal.setName(s"XM${name}${signal_name}".toUpperCase)
      }
      bundle.valid.setName(s"XM${name}VALID".toUpperCase)
      bundle.ready.setName(s"XM${name}READY".toUpperCase)
    }

    val XMAWMISC_INFO       = out Bits(4 bits)
    val XMBMISC_INFO        = in Bits(4 bits) default(0)
  }

  noIoPrefix()
}

object USB23App extends App {
  Config.spinal.generateVerilog(
    new Component {
      val PHY = master(USB23_IO())

      val dut = new USB23()
      for(io <- dut.getGroupedIO(true)) {
        if(io.isInput) {
          io.assignDontCare()
        }
      }
      dut.io.XM.setBlocked()
      dut.io.PHY <> PHY

    }.setDefinitionName("USB23_ex")
  )
}