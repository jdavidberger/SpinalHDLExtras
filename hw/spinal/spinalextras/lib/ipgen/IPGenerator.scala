package spinalextras.lib.ipgen

import com.fasterxml.jackson.annotation.{JsonAutoDetect, PropertyAccessor}
import com.fasterxml.jackson.core.JsonFactory
import com.fasterxml.jackson.databind.exc.UnrecognizedPropertyException
import com.fasterxml.jackson.databind.module.SimpleModule
import com.fasterxml.jackson.databind.node.ObjectNode
import com.fasterxml.jackson.databind.{JsonNode, MapperFeature, ObjectMapper}
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.kjetland.jackson.jsonSchema.{JsonSchemaConfig, JsonSchemaGenerator}
import spinal.core
import spinal.core._
import spinal.lib.KeepAttribute
import spinalextras.lib.{Config, Constraints}
import spinalextras.lib.mipi.GenerateByte2Pixel
import spinalextras.lib.misc.{HertzDeserializer, Obfuscater, TimeNumberDeserializer}
import spinalextras.lib.soc.spinex.Spinex

import java.io.{ByteArrayInputStream, FileInputStream, FileReader, FileWriter, SequenceInputStream}
import java.nio.file.{Files, OpenOption, Paths, StandardCopyOption}
import scala.collection.mutable
import scala.reflect.{ClassTag, classTag}

case class IPGeneratorOptions(device: Device = Device(vendor = "lattice", family = "lifcl"),
                              obfuscate: Boolean = false,
                              schema_name: String = "",
                              instance_name: String = "",
                              output_dir: String = "hw/gen",
                              yosys_cmd : String = "yosys",
                              yosys_opt : Boolean = true,
                              generate_sim: Boolean = false) {
  def sanitized_instance_name = instance_name.replace(" ", "_")

  def with_defaults(instance_name_hint : String) : IPGeneratorOptions = {
    val this_instance_name = if (instance_name.isEmpty) instance_name_hint else instance_name
    val sanitized_nanme = this_instance_name.replace(" ", "_")

    copy(
      instance_name = sanitized_nanme,
      output_dir = f"hw/gen/${sanitized_nanme}",
    )
  }
}

abstract class IPGenerator {
  def Schema() : JsonNode
  def Name : String
  def SaveSchema(fn : String)
  def SaveDefaultJson(fn : String)
  def ProcessFile(filePath : String) : Unit
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

    val version = getClass.getPackage.getImplementationVersion
    if (version != null) {
      schema.asInstanceOf[ObjectNode].put("version", version)
    } else {
      schema.asInstanceOf[ObjectNode].put("version", "dev")
    }

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

  def customObjectMapper(mapper : ObjectMapper): Unit = {

  }

  lazy val yaml_mapper: ObjectMapper = {
    val mapper = new ObjectMapper(new YAMLFactory())
    customObjectMapper(mapper)
    mapper.registerModule(DefaultScalaModule)
    val module = new SimpleModule()
    customMappings(module)
    mapper.registerModule(module)
    mapper
  }

  lazy val json_mapper: ObjectMapper = {
    val mapper = new ObjectMapper(new JsonFactory())
    customObjectMapper(mapper)
    mapper.registerModule(DefaultScalaModule)
    val module = new SimpleModule()
    customMappings(module)
    mapper.registerModule(module)
    mapper
  }

  def defaultClockDomainFrequency(cfg : CFG): IClockDomainFrequency = {
    UnknownFrequency()
  }

  def Header(options: IPGeneratorOptions, cfg : CFG): String = {
    s"""
      |Generator Options:
      |${yaml_mapper.writeValueAsString(options)}
      |Config:
      |${yaml_mapper.writeValueAsString(cfg)}
      |""".stripMargin
  }

  def SpinalConfig(options: IPGeneratorOptions, cfg : CFG): SpinalConfig = {
    Config.spinal.copy(
      device = options.device,
      targetDirectory = options.output_dir,
      defaultClockDomainFrequency = defaultClockDomainFrequency(cfg),
      defaultConfigForClockDomains = ClockDomainConfig(
        resetActiveLevel = LOW,
        resetKind = ASYNC
      ),
      rtlHeader = Header(options, cfg)
    )
  }

  def processConfig(options : IPGeneratorOptions, config : CFG)

  override def ProcessFile(options : IPGeneratorOptions, filePath : String) : Unit = {
    val reader = new FileReader(filePath)
    val mapper = if (filePath.endsWith(".yml")) yaml_mapper else json_mapper
    val config: CFG = mapper.readValue(reader, classTag[CFG].runtimeClass.asInstanceOf[Class[CFG]])

    val fileName = Paths.get(filePath).getFileName.toString.split('.').head
    processConfig(options.copy(instance_name = if (options.instance_name.isEmpty) fileName else options.instance_name), config)
  }

  def processConfig(instance_name : String, config : CFG) : Unit = {
    val sanitized_nanme = instance_name.replace(" ", "_")
    val options = IPGeneratorOptions(
      device = Device(vendor = "lattice", family = "lifcl"),
      obfuscate = true,
      instance_name = sanitized_nanme,
      output_dir = f"hw/gen/${sanitized_nanme}",
      generate_sim = false,
      yosys_opt = true
    )

    processConfig(options, config)
  }

  override def ProcessFile(filePath : String) : Unit = {
    var reader = new FileReader(filePath)
    val mapper = if (filePath.endsWith(".yml")) yaml_mapper else json_mapper
    val fileName = Paths.get(filePath).getFileName.toString.split('.').head

    val root = mapper.readTree(reader)
    val cfgClassTag = classTag[CFG].runtimeClass.asInstanceOf[Class[CFG]]
    try {
      val options = mapper.treeToValue(
        root.get("options"),
        classOf[IPGeneratorOptions]
      ).with_defaults(fileName)

      val config = mapper.treeToValue(
        root.get("config"),
        cfgClassTag
      )

      processConfig(options, config)
    } catch {
      case ex: UnrecognizedPropertyException => {
        reader = new FileReader(filePath)
        val config: CFG = mapper.treeToValue(
          root,
          cfgClassTag
        )
        processConfig(Paths.get(filePath).getFileName.toString.split('.').head, config)
      }
    }
  }

  def processRtl(options : IPGeneratorOptions, cfg: CFG, dut : () => Component, simDut : () => Component = null): Unit = {
    val config = this.SpinalConfig(options, cfg)
    val top_signals = new mutable.ArrayBuffer[String]()

    val report = config.generateVerilog(
      {
        val top = dut().noIoPrefix()
        if (options.instance_name.nonEmpty)
          top.setDefinitionName(options.sanitized_instance_name)
        top.noIoPrefix()
        if (options.obfuscate) {
          Obfuscater(top)
          Constraints.keep_key_heirarchy(top)
        }

        top.getAllIo.foreach(w => {
          val dir = w.getDirection match {
            case `in`    => "input"
            case `out`   => "output"
            case `inout` => "inout"
          }
          val size = if (w.getBitsWidth == 1) "" else {
            s"[${w.getBitsWidth - 1}:0]"
          }
          top_signals.append(f"${dir} wire ${size} ${w.getName()}")
        })

        top
      }
    )

    val top_file =
      s"""
         |`timescale 1ns/1ps
         |
         |module ${report.toplevelName}_top (
         |    ${top_signals.mkString(",\n    ")}
         |);
         |    ${report.toplevelName} top(.*);
         |endmodule
         |""".stripMargin
    Files.write(Paths.get(f"${report.globalData.config.targetDirectory}/${report.toplevelName}_top.sv"), top_file.getBytes)

    if(options.yosys_opt || options.obfuscate) {
      import scala.sys.process._

      val yosys_in_file =
        s"""
          |read_verilog ${report.globalData.config.targetDirectory}/${report.toplevelName}.v
          |hierarchy -top ${report.toplevelName}
          |tribuf
          |proc
          |${if (options.obfuscate) "flatten" else ""}
          |peepopt
          |wreduce
          |opt -full
          |check
          |write_verilog -decimal ${if (options.obfuscate) "-noattr" else ""} ${report.globalData.config.targetDirectory}/${report.toplevelName}.v
          |""".stripMargin

      val input = new ByteArrayInputStream(yosys_in_file.getBytes)
      (options.yosys_cmd #< input).!

      if (config.rtlHeader != null) {
        val f = f"${report.globalData.config.targetDirectory}/${report.toplevelName}.v"
        val stringToPrepend = new ByteArrayInputStream((f"/***\n${config.rtlHeader}\n***/\n").getBytes)
        val existingFile = new FileInputStream(f)
        val combinedStream = new SequenceInputStream(stringToPrepend, existingFile)

        // Writes the combined stream directly to a new (or same) path
        Files.copy(combinedStream, Paths.get(f), StandardCopyOption.REPLACE_EXISTING)
      }
    }

    Spinex.generate_ipx(report)

    if(simDut != null && options.generate_sim) {
      config.generateVerilog({
        val top = simDut().setDefinitionName(options.sanitized_instance_name).noIoPrefix()
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
      if (args(0) == "--build-default") {
        processConfig(Name, ConfigExample)
      } else {
        ProcessFile(args(0))
      }
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
        val reader = new FileReader(args(0))
        val mapper = new ObjectMapper(new YAMLFactory())
        mapper.registerModule(DefaultScalaModule)
        val loaded_file = mapper.readValue(reader, classOf[Map[String, Any]])


        val (options, schema_name) = loaded_file.get("options").map {
          case s : Map[String, Any] => {
            (None, s.get("schema_name").asInstanceOf[Option[String]].get)
          }
        }.getOrElse({
          val options = IPGeneratorOptions.Load(args(0))
          (Some(options), options.schema_name)
        })


        KnownGenerators.get(schema_name) match {
          case Some(g) =>
            val gen = g()
            options match {
              case Some(o) =>
                gen.ProcessFile(options = o, filePath = args(1))
              case None =>
                gen.ProcessFile(filePath = args(0))
            }
          case None => {
            System.err.println(f"Could not find generator with name ${schema_name}")
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