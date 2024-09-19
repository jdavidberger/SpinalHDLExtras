package spinalextras.lib.bus

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.misc._
import spinalextras.lib.logging.FlowLogger

import scala.language.postfixOps

object AXIBusLogger {
  def flows(addressMapping: AddressMapping, axis: Axi4Bus*): Seq[(Data, Flow[Bits])] = {
    val flows_meta = axis.map(axi => {
      val (aw, ar, r, w, b) = axi match {
        case axi: Axi4 => (axi.aw, axi.ar, axi.r, axi.w, axi.b) //.map(x => FlowLogger.asFlow(x))
        case axi: Axi4Shared => {
          val (aw, ar) = (Stream(Axi4Aw(axi.config)), Stream(Axi4Ar(axi.config)))
          aw.payload.assignAllByName(axi.arw.payload)
          ar.payload.assignAllByName(axi.arw.payload)
          aw.valid := axi.arw.valid && axi.arw.write
          ar.valid := axi.arw.valid && ~axi.arw.write
          aw.ready := axi.arw.ready
          ar.ready := axi.arw.ready

          (aw, ar, axi.r, axi.w, axi.b) //.map(x => FlowLogger.asFlow(x))
        }
      }

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
