import org.scalatest.funsuite.AnyFunSuite
import spinal.core.{Bundle, Component, IntToBuilder, cover}
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config, Axi4CrossbarFactory, Axi4ReadOnly, Axi4ReadOnlyArbiter, Axi4ReadOnlyDecoder, Axi4Shared, Axi4SharedDecoder, Axi4SharedOnChipRam, Axi4WriteOnly, Axi4WriteOnlyArbiter, Axi4WriteOnlyDecoder}
import spinal.lib.bus.misc.{DefaultMapping, SizeMapping}
import spinal.lib.{StreamPipe, master, slave}
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties, FormalProperty}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

class AxiCrossbarTester extends ComponentWithFormalProperties {
  val axiCrossbar = Axi4CrossbarFactory()

  val axiMConfig = Axi4Config(
    addressWidth    = 32,
    dataWidth       = 64,
    idWidth         = 2,
    useRegion       = false,
    useId           = true,
    useLock         = true,
    useQos          = false,
    useResp         = true, //false,
    useProt         = true,
    useStrb         = true
  )

  val io = new Bundle {
    val masterA, masterB = slave (Axi4(axiMConfig))
    val slaveA, slaveB = master (Axi4(axiMConfig.copy(idWidth = axiMConfig.idWidth + 1)))
  }

  axiCrossbar.addSlaves(
    io.slaveA    -> SizeMapping(1024, (1 + 0xFFFFFFFFL) - 1024),
    io.slaveB -> SizeMapping(0x0, 1024)
  )

  axiCrossbar.addConnections(
    io.masterA -> (Seq(io.slaveA, io.slaveB)),
    io.masterB -> (Seq(io.slaveA, io.slaveB))
  )

  axiCrossbar.build()
}

class AxiReadOnlyCrossbarTester extends ComponentWithFormalProperties {
  val axiCrossbar = Axi4CrossbarFactory()

  val axiMConfig = Axi4Config(
    addressWidth    = 32,
    dataWidth       = 64,
    idWidth         = 2,
    useRegion       = false,
    useId           = true,
    useLock         = true,
    useQos          = false,
    useResp         = true, //false,
    useProt         = true,
    useStrb         = true
  )

  val io = new Bundle {
    val masterA, masterB = slave (Axi4ReadOnly(axiMConfig))
    val slaveA, slaveB = master (Axi4ReadOnly(axiMConfig.copy(idWidth = axiMConfig.idWidth + 1)))
  }

  axiCrossbar.addSlaves(
    io.slaveA    -> SizeMapping(1024, (1 + 0xFFFFFFFFL) - 1024),
    io.slaveB -> SizeMapping(0x0, 1024)
  )

  axiCrossbar.addConnections(
    io.masterA -> (Seq(io.slaveA, io.slaveB)),
    io.masterB -> (Seq(io.slaveA, io.slaveB))
  )

  axiCrossbar.build()
}

class AxiWriteOnlyCrossbarTester extends ComponentWithFormalProperties {
  val axiCrossbar = Axi4CrossbarFactory()

  val axiMConfig = Axi4Config(
    addressWidth    = 32,
    dataWidth       = 64,
    idWidth         = 1,
    useRegion       = false,
    useId           = true,
    useLock         = true,
    useQos          = false,
    useResp         = true, //false,
    useProt         = true,
    useStrb         = true,
  )

  val io = new Bundle {
    val masterA , masterB = slave (Axi4WriteOnly(axiMConfig))
    val slaveA, slaveB = master (Axi4WriteOnly(axiMConfig.copy(idWidth = 2)))
  }

  axiCrossbar.addSlaves(
    //io.slaveA    -> SizeMapping(0, (1 + 0xFFFFFFFFL) - 0),
    io.slaveA    -> SizeMapping(1024, (1 + 0xFFFFFFFFL) - 1024),
    io.slaveB -> SizeMapping(0x0, 1024)
  )

  axiCrossbar.addConnections(
    io.masterA -> (Seq(io.slaveA, io.slaveB)),
    io.masterB -> (Seq(io.slaveA, io.slaveB))
  )

  axiCrossbar.build()
}


class AxiPipelineTester extends ComponentWithFormalProperties {
  val axiMConfig = Axi4Config(
    addressWidth    = 32,
    dataWidth       = 64,
    idWidth         = 2,
    useRegion       = false,
    useId           = true,
    useLock         = true,
    useQos          = false,
    useResp         = true, //false,
    useProt         = true,
    useStrb         = true
  )

  val io = new Bundle {
    val masterA /* masterB */ = slave (Axi4(axiMConfig))
    val slaveA /*slaveB*/ = master (Axi4(axiMConfig))
  }

  io.masterA.pipelined(aw=StreamPipe.FULL, ar=StreamPipe.FULL, r=StreamPipe.FULL, w=StreamPipe.FULL, b=StreamPipe.FULL) <> io.slaveA

  }

class AxiDirectTester extends ComponentWithFormalProperties {
  val axiMConfig = Axi4Config(
    addressWidth    = 32,
    dataWidth       = 64,
    idWidth         = 2,
    useRegion       = false,
    useId           = true,
    useLock         = true,
    useQos          = false,
    useResp         = true, //false,
    useProt         = true,
    useStrb         = true
  )

  val io = new Bundle {
    val masterA /* masterB */ = slave (Axi4(axiMConfig))
    val slaveA /*slaveB*/ = master (Axi4(axiMConfig))
  }

  io.masterA <> io.slaveA

  override def covers(): Seq[FormalProperty] = new FormalProperties(this) {
    addFormalProperty(io.masterA.ar.fire)
    addFormalProperty(io.masterA.r.fire)
    addFormalProperty(io.masterA.aw.fire)
    addFormalProperty(io.masterA.w.fire)
    addFormalProperty(io.masterA.b.fire)

    addFormalProperty(io.masterA.aw.valid)
    addFormalProperty(io.masterA.ar.valid)

    addFormalProperty(io.slaveA.r.valid)
  }
}


class AxiReadOnlyDecoderTester extends ComponentWithFormalProperties {
  val axiMConfig = Axi4Config(
    addressWidth    = 32,
    dataWidth       = 64,
    idWidth         = 2,
    useRegion       = false,
    useId           = true,
    useLock         = true,
    useQos          = false,
    useResp         = true, //false,
    useProt         = true,
    useStrb         = true
  )

  val io = new Bundle {
    val masterA /* masterB */ = slave (Axi4ReadOnly(axiMConfig))
    val slaveA, slaveB = master (Axi4ReadOnly(axiMConfig))
  }

  val decoder = Axi4ReadOnlyDecoder(axiMConfig, Seq(SizeMapping(0, 1024), SizeMapping(1024, 1 + 0xFFFFFFFFL - 1024)))
  decoder.io.input <> io.masterA
  decoder.io.outputs(0) <> io.slaveA
  decoder.io.outputs(1) <> io.slaveB
}


class Axi4ReadOnlyArbiterTester extends ComponentWithFormalProperties {
  val axiMConfig = Axi4Config(
    addressWidth    = 32,
    dataWidth       = 64,
    idWidth         = 0,
    useRegion       = false,
    useId           = true,
    useLock         = true,
    useQos          = false,
    useResp         = true, //false,
    useProt         = true,
    useStrb         = true
  )

  val io = new Bundle {
    val masterA, masterB = slave (Axi4ReadOnly(axiMConfig))
    val slaveA = master (Axi4ReadOnly(axiMConfig.copy(idWidth = axiMConfig.idWidth + 1)))
  }

  val arbiter = Axi4ReadOnlyArbiter(axiMConfig.copy(idWidth = axiMConfig.idWidth + 1), 2)
  arbiter.io.inputs(0) <> io.masterA
  arbiter.io.inputs(1) <> io.masterB
  arbiter.io.output <> io.slaveA
}

class Axi4WriteOnlyArbiterTester(routeBufferSize : Int = 4) extends ComponentWithFormalProperties {
  val axiMConfig = Axi4Config(
    addressWidth    = 32,
    dataWidth       = 64,
    idWidth         = 2,
    useRegion       = false,
    useId           = true,
    useLock         = true,
    useQos          = false,
    useResp         = true, //false,
    useProt         = true,
    useStrb         = true
  )

  val io = new Bundle {
    val masterA, masterB = slave (Axi4WriteOnly(axiMConfig))
    val slaveA = master (Axi4WriteOnly(axiMConfig.copy(idWidth = axiMConfig.idWidth + 1)))
  }

  val arbiter = Axi4WriteOnlyArbiter(axiMConfig.copy(idWidth = axiMConfig.idWidth + 1), 2, routeBufferSize)
  arbiter.io.inputs(0) <> io.masterA
  arbiter.io.inputs(1) <> io.masterB
  arbiter.io.output <> io.slaveA
}


class AxiWriteOnlyDecoderTester extends ComponentWithFormalProperties {
  val axiMConfig = Axi4Config(
    addressWidth    = 32,
    dataWidth       = 64,
    idWidth         = 2,
    useRegion       = false,
    useId           = true,
    useLock         = true,
    useQos          = false,
    useResp         = true, //false,
    useProt         = true,
    useStrb         = true
  )

  val io = new Bundle {
    val masterA /* masterB */ = slave (Axi4WriteOnly(axiMConfig))
    val slaveA, slaveB = master (Axi4WriteOnly(axiMConfig))
  }

  val decoder = Axi4WriteOnlyDecoder(axiMConfig, Seq(SizeMapping(0, 1024), SizeMapping(1024, 1 + 0xFFFFFFFFL - 1024)))
  decoder.io.input <> io.masterA
  decoder.io.outputs(0) <> io.slaveA
  decoder.io.outputs(1) <> io.slaveB
}

class AxiCrossbarTesterFormalTest extends AnyFunSuite with FormalTestSuite {
  formalTests().foreach(t => test(t._1) { t._2() })

  override def ProveConfig() = formalConfig.withProve(5)
  override def defaultDepth(): Int = 5

  override def generateRtlCover() = Seq(
    ("Direct", () => GeneralFormalDut( () => new AxiDirectTester())),
  )

  override def generateRtlProve() = Seq(
    ("Direct", () => GeneralFormalDut( () => new AxiDirectTester())),
  )

  override def generateRtlBMC() = Seq(
    ("ReadDecoder", () => GeneralFormalDut( () => new AxiReadOnlyDecoderTester())),
    ("ReadArbiter", () => GeneralFormalDut( () => new Axi4ReadOnlyArbiterTester())),
    ("writeCrossbar", () => GeneralFormalDut( () => new AxiWriteOnlyCrossbarTester())),
    ("WriteArbiter", () => GeneralFormalDut( () => new Axi4WriteOnlyArbiterTester())),
    ("Pipelined", () => GeneralFormalDut( () => new AxiPipelineTester())),
    ("WriteDecoder", () => GeneralFormalDut( () => new AxiWriteOnlyDecoderTester())),
    ("readCrossbar", () => GeneralFormalDut( () => new AxiReadOnlyCrossbarTester())),
    ("crossbar", () => GeneralFormalDut( () => new AxiCrossbarTester())),
  )

  override def generateRtl() = Seq()
}
