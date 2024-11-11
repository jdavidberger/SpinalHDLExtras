package spinalextras.mains


import com.fasterxml.jackson.annotation._
import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.databind._
import com.fasterxml.jackson.databind.deser.std.StdDeserializer
import com.fasterxml.jackson.databind.module.{SimpleDeserializers, SimpleModule}
import com.fasterxml.jackson.databind.node.ObjectNode
import com.fasterxml.jackson.dataformat.yaml._
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import spinal.core._
import spinalextras.lib
import spinalextras.lib.Config
import spinalextras.lib.blackbox.lattice.lifcl.{PLL, PLLConfig}
import spinalextras.lib.misc.ClockSpecification

import java.io.FileReader

class PLLSpecification(val name : String,
                        val inputClock : ClockSpecification,
                        val outputClocks : ClockSpecification*
                      ) {

}


case class HertzDeserializer() extends StdDeserializer[HertzNumber](classOf[HertzNumber]) {
  override def deserialize(p: JsonParser, ctxt: DeserializationContext) = {
    val t = p.readValueAsTree[ObjectNode]()
    HertzNumber(t.get("value").asDouble())
  }
}

case class LatticePLL(config: PLLSpecification) extends Component {
  val io = new Bundle {
    val output_clks = config.outputClocks.map(x => out Bool())
    val clki_i, rstn_i = in Bool()
    val lock_o = out Bool()
  }
  new ClockingArea(new ClockDomain(io.clki_i, ~io.rstn_i, frequency = FixedFrequency(config.inputClock.freq))) {
    val pllConfig = PLLConfig.create(
      config.inputClock, config.outputClocks:_*
    )

    val pll = new PLL(pllConfig)
    io.lock_o := pll.io.LOCK
    pll.io.CLKS.zip(io.output_clks).foreach(cds => {
      cds._2 := cds._1._2
      cds._2.setName(cds._1._2.name.toLowerCase() + "_o")
    })
  }
  noIoPrefix()
}


object GenerateLatticePLL extends App {
  val mapper = new ObjectMapper(new YAMLFactory())
  mapper.registerModule(DefaultScalaModule)
  val module = new SimpleModule()
  module.addDeserializer(classOf[HertzNumber], HertzDeserializer())
  mapper.registerModule(module)

  if(args.size > 0) {
    val reader = new FileReader(args(0))
    val config: PLLSpecification = mapper.readValue(reader, classOf[PLLSpecification])

    Config.spinal.copy(rtlHeader  = mapper.writeValueAsString(config)).generateVerilog(
      LatticePLL(config).setDefinitionName(config.name + "_pll")
    )
  } else {
    val exampleYaml = mapper.writeValueAsString(new PLLSpecification(
      "Example",
      lib.misc.ClockSpecification(24 MHz),

      lib.misc.ClockSpecification(60 MHz),
      lib.misc.ClockSpecification(100 MHz, tolerance = 0),
      lib.misc.ClockSpecification(125 MHz, 90),
      lib.misc.ClockSpecification(125 MHz, 25),
      lib.misc.ClockSpecification(32 MHz, 135, .05),
    ))

    println(
      s"""
        |GenerateLatticePLL <spec-file.yml>
        |
        |Spec file example:
        |```
        |${exampleYaml}
        |```
        |""".stripMargin)
  }
}