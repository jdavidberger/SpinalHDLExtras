package spinalextras.lib.ipgen

import com.fasterxml.jackson.core.JsonFactory
import com.fasterxml.jackson.databind.module.SimpleModule
import com.fasterxml.jackson.databind.node.ObjectNode
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.kjetland.jackson.jsonSchema.{JsonSchemaConfig, JsonSchemaGenerator}
import spinal.core
import spinal.core._
import spinalextras.lib.Config
import spinalextras.lib.mipi.GenerateByte2Pixel
import spinalextras.lib.misc.{HertzDeserializer, Obfuscater, TimeNumberDeserializer}
import spinalextras.lib.soc.spinex.Spinex

import java.io.{FileReader, FileWriter}
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.reflect.{ClassTag, classTag}

case class IPGeneratorOptions(device: Device = Device(vendor = "lattice", family = "lifcl"),
                              obfuscate: Boolean = false,
                              name: String = "",
                              output_dir: String = "hw/gen",
                              generate_sim: Boolean = false) {

}

abstract class IPGenerator {
  def Schema() : JsonNode
  def Name : String
  def SaveSchema(fn : String)
  def SaveDefaultJson(fn : String)
  def ProcessFile(options : IPGeneratorOptions, filePath : String) : Unit
  def Description : String = ""
  def Labels: Seq[String] = Seq()
  def Provider: String = ""
}

object IPGeneratorOptions {
  def Load(fn : String): IPGeneratorOptions = {
    val reader = new FileReader(fn)
    val mapper = new ObjectMapper(new YAMLFactory())
    mapper.registerModule(DefaultScalaModule)
    mapper.readValue(reader, classOf[IPGeneratorOptions])
  }
}

abstract class IPGenerator_[CFG : ClassTag] extends IPGenerator {
  def DefaultConfig : Option[CFG] = None
  def ConfigExample : CFG

  def customMappings(module: SimpleModule): Unit = {
    module.addDeserializer(classOf[HertzNumber], HertzDeserializer())
    module.addDeserializer(classOf[TimeNumber], TimeNumberDeserializer())
  }

  def SaveDefaultJson(fn : String): Unit = {
    val cfg = DefaultConfig.getOrElse(ConfigExample)
    val schema = json_mapper.writerWithDefaultPrettyPrinter.writeValueAsString(cfg)
    val writer = new FileWriter(fn)
    writer.write(schema)
    writer.close()
  }

  override def Labels: Seq[String] = Seq()
  override def Provider: String = this.getClass.getPackage.getName.split('.')(0)

  override def Schema(): JsonNode = {
    val schemaGen = new JsonSchemaGenerator(json_mapper, JsonSchemaConfig.html5EnabledSchema)
    val clazz = classTag[CFG].runtimeClass.asInstanceOf[Class[CFG]]
    val schema = schemaGen.generateJsonSchema(clazz)
    schema.asInstanceOf[ObjectNode].put("title", Name)
    schema.asInstanceOf[ObjectNode].put("description", Description.replace("\n", ""))
    schema.asInstanceOf[ObjectNode].put("provider", Provider)
    val labelsArray = json_mapper.createArrayNode()
    Labels.foreach(labelsArray.add)
    schema.asInstanceOf[ObjectNode].set("labels", labelsArray)
    schema
  }

  def SaveSchema(fn : String) {
    val schema = json_mapper.writerWithDefaultPrettyPrinter.writeValueAsString(Schema())
    val writer = new FileWriter(fn)
    writer.write(schema)
    writer.close()
  }

  lazy val yaml_mapper: ObjectMapper = {
    val mapper = new ObjectMapper(new YAMLFactory())
    mapper.registerModule(DefaultScalaModule)
    val module = new SimpleModule()
    customMappings(module)
    mapper.registerModule(module)
    mapper
  }

  lazy val json_mapper: ObjectMapper = {
    val mapper = new ObjectMapper(new JsonFactory())
    mapper.registerModule(DefaultScalaModule)
    val module = new SimpleModule()
    customMappings(module)
    mapper.registerModule(module)
    mapper
  }

  def defaultClockDomainFrequency(cfg : CFG): IClockDomainFrequency = {
    UnknownFrequency()
  }

  def OutputDirectory(cfg : CFG): String = {
    Name
  }

  def SpinalConfig(options: IPGeneratorOptions, cfg : CFG): SpinalConfig = {
    Config.spinal.copy(
      device = options.device,
      targetDirectory = options.output_dir,
      defaultClockDomainFrequency = defaultClockDomainFrequency(cfg),
      defaultConfigForClockDomains = ClockDomainConfig(
        resetActiveLevel = LOW,
        resetKind = ASYNC
      ))
  }

  def processConfig(options : IPGeneratorOptions, config : CFG)

  override def ProcessFile(options : IPGeneratorOptions, filePath : String) : Unit = {
    val reader = new FileReader(filePath)
    val mapper = if (filePath.endsWith(".yml")) yaml_mapper else json_mapper
    val config: CFG = mapper.readValue(reader, classTag[CFG].runtimeClass.asInstanceOf[Class[CFG]])
    processConfig(options, config)
  }

  def ProcessFile(filePath : String) : Unit = {
    val reader = new FileReader(filePath)
    val mapper = if (filePath.endsWith(".yml")) yaml_mapper else json_mapper
    val config: CFG = mapper.readValue(reader, classTag[CFG].runtimeClass.asInstanceOf[Class[CFG]])

    val options = IPGeneratorOptions(
      device = Device(vendor = "lattice", family = "lifcl"),
      obfuscate = false,
      name = Name,
      output_dir = f"hw/gen/${OutputDirectory(config)}",
      generate_sim = false
    )

    processConfig(options, config)
  }

  def processRtl(options : IPGeneratorOptions, cfg: CFG, dut : () => Component, simDut : () => Component = null): Unit = {
    val config = this.SpinalConfig(options, cfg)
    val report = config.generateVerilog(
      {
        val top = dut()
        top.noIoPrefix()
        if (options.obfuscate) {
          Obfuscater(top)
        }
        top
      }
    )

    Spinex.generate_ipx(report)

    if(simDut != null && options.generate_sim) {
      config.generateVerilog({
        val top = simDut()
        top.noIoPrefix()
        if (options.obfuscate) {
          Obfuscater(top)
        }
        top
      })
    }
  }

  def cli_main(args: Array[String]): Unit = {
    if (args.length > 0) {
      ProcessFile(args(0))
    } else {
      val exampleYaml = yaml_mapper.writeValueAsString(ConfigExample)
      val exampleJson = json_mapper.writeValueAsString(ConfigExample)
      val jsonSchema = json_mapper.writeValueAsString(Schema())
      println(
        s"""
           |${this.getClass.getSimpleName} <spec-file.yml>
           |
           |Spec file example (yaml):
           |```
           |${exampleYaml}
           |```
           |Spec file example (json):
           |```
           |${exampleJson}
           |```
           |JSON Schema:
           |```
           |${jsonSchema}
           |```
           |
           |""".stripMargin)
    }
  }
}

object IPGenerator {
  val KnownGenerators = new mutable.HashMap[String, () => IPGenerator]

  def main(args: Array[String]): Unit = {
    if (args.length > 0) {
      if (args(0) == "--schemas") {
        val dir = args(1)
        val path = Paths.get(dir)
        Files.createDirectories(path)

        for ((name, gen_factory) <- KnownGenerators) {
          val gen = gen_factory()
          gen.SaveSchema(f"${dir}/${name}.schema.json")
          gen.SaveDefaultJson(f"${dir}/${name}.example.json")
        }
      } else {
        KnownGenerators.get(args(0)) match {
          case Some(g) => {
            val gen = g()

            val options = IPGeneratorOptions.Load(args(1))
            gen.ProcessFile(options = options, filePath = args(2))
          }
          case None => {
            System.err.println(f"Could not find generator with name ${args(0)}")
            for ((name, gen_factory) <- KnownGenerators) {
              System.err.println(f"- ${name}")
            }
          }
        }
      }
    } else {
      for ((name, gen_factory) <- KnownGenerators) {
        println(f"- ${name}")
      }
    }
  }
}