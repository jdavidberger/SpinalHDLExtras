package spinalextras.lib.formal.fillins

import spinal.core._
import spinal.lib._
import spinal.lib.bus.wishbone.Wishbone
import spinalextras.lib.formal.{FormalDataWithEquivalnce, FormalMasterSlave, FormalProperties, FormalProperty, fillins}

import scala.reflect.{ClassTag, classTag}

object Wishbone {
  implicit class WishboneFormalExt(val bus : Wishbone) extends FormalMasterSlave with FormalDataWithEquivalnce[WishboneFormalExt] {

    /**
     * @return True if and only if the driving signals are valid
     */
    override def formalIsProducerValid(): Seq[FormalProperty] = new FormalProperties {
      import spinal.core.formal._
      import bus._

      private val slaveRequestAck = if (config.isPipelined) !STALL else ACK
      private val slackRequestErr = if (config.useERR) ERR else False
      private val masterHasRequest = isCycle && STB
      private val isRequestStalled = masterHasRequest && !slaveRequestAck && !slackRequestErr

      private val wasStalledRequest = past(isRequestStalled) init (False)
      private val invalidRequestDrop = wasStalledRequest && !masterHasRequest
      addFormalProperty(!invalidRequestDrop, s"${bus} dropped its request too soon")

      private val dataStableValid = Bool()
      dataStableValid := True
      val effective_mosi = Mux(WE, DAT_MOSI, B(0))
      val effective_sel = if (SEL == null) null else Mux(WE, SEL, B(0))
      when(wasStalledRequest) {
        dataStableValid := Seq(effective_mosi, ADR, WE, effective_sel)
          .filter(_ != null)
          .map(x => stable(x))
          .fold(True)((a, b) => a & b)
      }
      addFormalProperty(dataStableValid, s"${bus} wishbone payload should be stable")

      // From here, https://github.com/ZipCPU/zipcpu/blob/master/rtl/ex/fwb_master.v#L220, the author assumes that an
      // ERR should force CYC to drop for the next cycle. I couldn't quite be sure this was a real constraint though --
      //
      //    val dropsOnError = if(ERR != null) {
      //      pastValid() && (!past(ERR) || !CYC)
      //    } else True
    }

    lazy val alwaysAck = new Composite(bus, "alwaysAck") { val v = RegInit(True) clearWhen(!bus.ACK) }.v
    /**
     * @return True if and only if the response signals are valid
     */
    override def formalIsConsumerValid(): Seq[FormalProperty] = new FormalProperties {
      import bus._

      if(config.useSTALL) {
        ???
      }
      // Slaves are allowed to keep ACK high forever; rule 3.55. We track this and allow exemptions to other rules for it.
      //val alwaysAck = RegInit(True) clearWhen(!ACK)

      val masterHasRequest = isCycle && STB
      val ackStateIsValid = ACK === False || masterHasRequest || alwaysAck
      addFormalProperty(ackStateIsValid, s"${bus} ack state not valid")

      val errorStateIsValid = if(ERR == null) True else {
        val isValid = Bool()
        isValid := True
        when(ERR) {
          isValid := !ACK && masterHasRequest
        }
        isValid
      }
      addFormalProperty(errorStateIsValid, s"${bus} error state valid")

      val cycleTerminationSignals = ACK ##
        (if(ERR != null) ERR else False) ##
        (if(RTY != null) RTY else False)

      val rule_315 = masterHasRequest || !cycleTerminationSignals.orR || alwaysAck
      addFormalProperty(rule_315, s"${bus} rule 3.15 violation")

      val rule_345 = CountOne(cycleTerminationSignals) <= 1
      addFormalProperty(rule_345, s"${bus} rule 3.45 violation")
    }

    override def asIMasterSlave: IMasterSlave = bus

    override def selfClassTag: ClassTag[WishboneFormalExt] = classTag[WishboneFormalExt]

    type Self = WishboneFormalExt
    override def formalAssertEquivalence(that: Self): Unit = {
      assert(this.alwaysAck === that.alwaysAck)
    }
  }
}