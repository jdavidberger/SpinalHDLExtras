package spinalextras.lib.soc

import spinal.core.{Component, SpinalReport, SpinalTag}
import spinal.lib.bus.misc.SizeMapping

import java.io.{FileWriter, OutputStreamWriter}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class DeviceTree {
  def indent(str : String, tabs : Int) = {
    str.split("\n").map((" " * (tabs*3) + _)).mkString("\n")
  }
  case class TreeNode(children: mutable.HashMap[String, TreeNode] = new mutable.HashMap[String, TreeNode](),
                      values: mutable.ArrayBuffer[String] = new ArrayBuffer[String]()) {
    def addValue(path: Seq[String], value: String): Unit = {
      if (path.isEmpty) {
        values.append(value)
      } else {
        children.getOrElseUpdate(path.head, new TreeNode()).addValue(path.drop(1), value)
      }
    }

    def str(tabs: Int = 0): String = {
      indent((values.map(x => {
        x
      }) ++
        children.map(x => {
          f"""|${x._1} {
              |${x._2.str(tabs + 1)}
              |}
              |""".stripMargin
        })).mkString("\n"), tabs)
    }
  }

  val root = new TreeNode()

  def addEntry(value: String, section: String*): Unit = {
    root.addValue(section, value)
  }

  def save(filePath: String) = {
    val name = if (Component.toplevel.definitionName == null) "spinex" else Component.toplevel.definitionName
    val writer = new FileWriter(s"${filePath}/${name}.overlay")
    writer.write(root.str())
    writer.close()
  }
}

case class DeviceTreeProviderTag(provider : DeviceTreeProvider) extends SpinalTag {

}
abstract class DeviceTreeProvider(val regBase : BigInt = 0, val regSize : Int = 4) {
  Component.toplevel.addTag(new DeviceTreeProviderTag(this))

  def regs : Seq[(String, SizeMapping)] = Seq(("base" -> SizeMapping(0, regSize)))
  def entryName : String = getClass.getSimpleName.toLowerCase
  def baseEntryPath = Seq("/", f"${entryName}@${regBase.toString(16)}")
  def compatible : Seq[String] = Seq(s"spinex,${entryName}")

  def interrupts : Seq[(Int, Int)] = Seq()

  def appendDeviceTree(dt : DeviceTree): Unit = {
    if(compatible.nonEmpty) {
      dt.addEntry(
        f"compatible = ${compatible.map('"' + _ + '"').mkString(", ")};",
        baseEntryPath:_*)
    }
    dt.addEntry("""status = "okay";""", baseEntryPath:_*)
    dt.addEntry(s"reg = <${regs.map(_._2).map(m => s"0x${(m.base + regBase).toString(16)} 0x${(1 + m.highestBound - m.lowerBound).toString(16)}").mkString("\n       ")}>;", baseEntryPath:_*)
    dt.addEntry(s"reg-names = ${regs.map(_._1).map(m => '"' + m + '"').mkString(",\n            ")};", baseEntryPath:_*)
    if(interrupts.nonEmpty) {
      dt.addEntry("interrupt-parent = <&intc0>;", baseEntryPath:_*)
      dt.addEntry(
        f"interrupts = < ${interrupts.map(x => f"${x._1} ${x._2}").mkString(" ")} >;",
        baseEntryPath:_*)
    }
  }
}

object DeviceTree {
  def generate[T <: Component](report: SpinalReport[T]) = {
    val deviceTree = new DeviceTree()
    Component.toplevel.getTagsOf[DeviceTreeProviderTag]().map(_.provider).foreach(x => x.appendDeviceTree(deviceTree))
    deviceTree.save(s"${report.globalData.config.targetDirectory}")
  }

}