ThisBuild / version := "1.0"
ThisBuild / scalaVersion := "2.12.18"
ThisBuild / organization := "io.constructiverealities"

val spinalVersion = "dev"
//val spinalVersion = "1.11.0"
val spinalCore = "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion
val spinalLib = "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion

val spinalIdslPlugin = compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)
//
//lazy val spinalCore = ProjectRef(uri("https://github.com/jdavidberger/SpinalHDL.git#feature/pmb_formal_testing"), "core")
//lazy val spinalLib = ProjectRef(uri("https://github.com/jdavidberger/SpinalHDL.git#feature/pmb_formal_testing"), "lib")
val vexRisc = ProjectRef(file("third_party/VexRiscv/"), "root")

lazy val SpinalHDLExtras = (project in file("."))
  .settings(
    Compile / scalaSource := baseDirectory.value / "hw" / "spinal",
    Test / scalaSource := baseDirectory.value / "hw" / "spinal",
    libraryDependencies ++= Seq(spinalCore, spinalLib,
      spinalIdslPlugin,
      "org.scalatest" %% "scalatest" % "3.2.15",

      "com.fasterxml.jackson.core" % "jackson-core" % "2.17.2",
      "com.fasterxml.jackson.core" % "jackson-annotations" % "2.17.2",
      "com.fasterxml.jackson.core" % "jackson-databind" % "2.17.2",
      "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.17.2",

      "com.fasterxml.jackson.dataformat" % "jackson-dataformat-yaml" % "2.17.2"
    )
  ).dependsOn(vexRisc)//.dependsOn(spinalCore, spinalLib)

fork := true
