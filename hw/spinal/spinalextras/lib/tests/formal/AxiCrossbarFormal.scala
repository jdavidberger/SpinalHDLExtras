import org.scalatest.funsuite.AnyFunSuite
import spinal.core.{Bundle, Component, IntToBuilder}
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config, Axi4CrossbarFactory, Axi4ReadOnly, Axi4ReadOnlyArbiter, Axi4ReadOnlyDecoder, Axi4SharedOnChipRam, Axi4WriteOnly, Axi4WriteOnlyArbiter, Axi4WriteOnlyDecoder}
import spinal.lib.bus.misc.{DefaultMapping, SizeMapping}
import spinal.lib.{StreamPipe, master, slave}
import spinalextras.lib.formal.ComponentWithFormalProperties
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

class AxiCrossbarTester extends ComponentWithFormalProperties {
  val axiCrossbar = Axi4CrossbarFactory()

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

  val io = new Bundle {
    val masterA, masterB = slave (Axi4(axiMConfig))
    val slaveA, slaveB = master (Axi4(axiMConfig.copy(idWidth = 9)))
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
    idWidth         = 8,
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
    val slaveA, slaveB = master (Axi4ReadOnly(axiMConfig.copy(idWidth = 9)))
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
    idWidth         = 8,
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
    val slaveA, slaveB = master (Axi4WriteOnly(axiMConfig.copy(idWidth = 9)))
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
    idWidth         = 8,
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
    idWidth         = 8,
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

}


class AxiReadOnlyDecoderTester extends ComponentWithFormalProperties {
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
    idWidth         = 8,
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
    val slaveA = master (Axi4ReadOnly(axiMConfig.copy(idWidth = 9)))
  }

  val arbiter = Axi4ReadOnlyArbiter(axiMConfig.copy(idWidth = 9), 2)
  arbiter.io.inputs(0) <> io.masterA
  arbiter.io.inputs(1) <> io.masterB
  arbiter.io.output <> io.slaveA
}

class Axi4WriteOnlyArbiterTester(routeBufferSize : Int = 4) extends ComponentWithFormalProperties {
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

  val io = new Bundle {
    val masterA, masterB = slave (Axi4WriteOnly(axiMConfig))
    val slaveA = master (Axi4WriteOnly(axiMConfig.copy(idWidth = 9)))
  }

  val arbiter = Axi4WriteOnlyArbiter(axiMConfig.copy(idWidth = 9), 2, routeBufferSize)
  arbiter.io.inputs(0) <> io.masterA
  arbiter.io.inputs(1) <> io.masterB
  arbiter.io.output <> io.slaveA
}


class AxiWriteOnlyDecoderTester extends ComponentWithFormalProperties {
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

  override def defaultDepth(): Int = 10

  override def generateRtlBMC() = Seq(
    ("writeCrossbar", () => GeneralFormalDut( () => new AxiWriteOnlyCrossbarTester())),
    ("WriteArbiter", () => GeneralFormalDut( () => new Axi4WriteOnlyArbiterTester())),
    ("Direct", () => GeneralFormalDut( () => new AxiDirectTester())),
    ("Pipelined", () => GeneralFormalDut( () => new AxiPipelineTester())),
    ("ReadDecoder", () => GeneralFormalDut( () => new AxiReadOnlyDecoderTester())),
    ("ReadArbiter", () => GeneralFormalDut( () => new Axi4ReadOnlyArbiterTester())),
    ("WriteDecoder", () => GeneralFormalDut( () => new AxiWriteOnlyDecoderTester())),
    ("readCrossbar", () => GeneralFormalDut( () => new AxiReadOnlyCrossbarTester())),
    ("crossbar", () => GeneralFormalDut( () => new AxiCrossbarTester())),
  )

  override def generateRtl() = Seq()
}
