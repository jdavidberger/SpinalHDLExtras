ThisBuild / version := "1.0"
ThisBuild / scalaVersion := "2.12.18"
ThisBuild / organization := "io.constructiverealities"

val spinalVersion = "dev"

val spinalCore = "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion
val spinalLib = "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion
val spinalIdslPlugin = compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)

val vexRiscv = if(file("../VexRiscv/").exists())
  ProjectRef(file("../VexRiscv/"), "root") else
  ProjectRef(uri("ssh://git@github.com/jdavidberger/VexRiscv.git"), "root")

lazy val SpinalHDLExtras = (project in file("."))
  .settings(
    Compile / scalaSource := baseDirectory.value / "hw" / "spinal",
    Test / scalaSource := baseDirectory.value / "hw" / "spinal",
    libraryDependencies ++= Seq(
      spinalCore, spinalLib, spinalIdslPlugin,
      "org.scalatest" %% "scalatest" % "3.2.15",

      "com.fasterxml.jackson.core" % "jackson-core" % "2.17.2",
      "com.fasterxml.jackson.core" % "jackson-annotations" % "2.17.2",
      "com.fasterxml.jackson.core" % "jackson-databind" % "2.17.2",
      "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.17.2",

      "com.fasterxml.jackson.dataformat" % "jackson-dataformat-yaml" % "2.17.2"
    )
  ).dependsOn(vexRiscv)

fork := true
