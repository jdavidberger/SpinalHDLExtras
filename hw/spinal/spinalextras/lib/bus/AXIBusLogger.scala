package spinalextras.lib.bus

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.misc._
import spinalextras.lib.logging.{FlowLogger, SignalLogger}

import scala.language.postfixOps

object AXIBusLogger {
  def decompose(axi : Axi4Bus) = {
    axi match {
      case axi: Axi4 => (axi.name, axi.aw, axi.ar, axi.r, axi.w, axi.b) //.map(x => FlowLogger.asFlow(x))
      case axi: Axi4Shared => {
        val (aw, ar) = (Stream(Axi4Aw(axi.config)), Stream(Axi4Ar(axi.config)))
        aw.payload.assignAllByName(axi.arw.payload)
        ar.payload.assignAllByName(axi.arw.payload)
        aw.valid := axi.arw.valid && axi.arw.write
        ar.valid := axi.arw.valid && ~axi.arw.write
        aw.ready := axi.arw.ready
        ar.ready := axi.arw.ready

        (axi.name, aw, ar, axi.r, axi.w, axi.b) //.map(x => FlowLogger.asFlow(x))
      }
    }

  }

  def stalls(axis: Axi4Bus*): Seq[(Data, Flow[Bits])] = {
    axis.flatMap(axi => {
      val (name, aw, ar, r, w, b) = decompose(axi)
      SignalLogger.concat(s"${name}_signals",
        aw.isStall.setName("aw_stall"),
        ar.isStall.setName("ar_stall"),
        r.isStall.setName("r_stall"),
        w.isStall.setName("w_stall"),
        b.isStall.setName("b_stall"),
        (w.last && w.fire).setName("w_lastFire")
      )
    }
    )
  }

  def flows(addressMapping: AddressMapping, axis: Axi4Bus*): Seq[(Data, Flow[Bits])] = {
    val flows_meta = axis.map(axi => {
      val (_, aw, ar, r, w, b) = decompose(axi)

      val (allowWrite, allowRead) =
        if(addressMapping == AllMapping) {
          (True, True)
        } else {
          (RegNextWhen(addressMapping.hit(aw.payload.addr), aw.fire, False),
            RegNextWhen(addressMapping.hit(ar.payload.addr), ar.fire, False))
        }

      (Seq(
        aw.toFlowFire.stage().takeWhen(addressMapping.hit(aw.payload.addr)).setName(aw.getName()),
        ar.toFlowFire.stage().takeWhen(addressMapping.hit(ar.payload.addr)).setName(ar.getName()),
        r.toFlowFire.stage().takeWhen(allowRead).setName(r.getName()),
        w.toFlowFire.stage().takeWhen(allowWrite).setName(w.getName()),
        b.toFlowFire.stage().takeWhen(allowWrite).setName(b.getName()),
      ).map(x => FlowLogger.asFlow(x)),
        Seq(
          aw.valid,
          ar.valid,
          r.valid,
          w.valid,
          b.valid,
          aw.ready,
          ar.ready,
          r.ready,
          w.ready,
          b.ready
        )
      )

    })

    flows_meta.flatMap(_._1)
  }
  def flows(axis: Axi4Bus*): Seq[(Data, Flow[Bits])] = {
    flows(AllMapping, axis:_*)
  }
}
