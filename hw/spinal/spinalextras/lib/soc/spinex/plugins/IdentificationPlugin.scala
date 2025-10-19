package spinalextras.lib.soc.spinex.plugins

import spinal.core.{B, IntToBuilder}
import spinal.lib.bus.misc.SizeMapping
import spinalextras.lib.soc.DeviceTree
import spinalextras.lib.soc.spinex.plugins.IdentificationPlugin.{getBuildTimestamp, getGitHash}
import spinalextras.lib.soc.spinex.{Spinex, SpinexRegisterFileApbPlugin}

import scala.language.postfixOps

case class IdentificationPlugin(deviceId: String = "SPNX",
                                registerLocation: BigInt = 0x0,
                                gitHashValue: BigInt = getGitHash(), // Default value, will be overridden at build time
                                buildTimestamp: BigInt = getBuildTimestamp(), // Unix timestamp of build
                                versionMajor: Int = 1,
                                versionMinor: Int = 0,
                                versionPatch: Int = 0) extends SpinexRegisterFileApbPlugin("id", SizeMapping(registerLocation, 0x10 Bytes)) {
  val deviceIdInt = deviceId.map(_.toInt).reduce(_ << 8 | _)
  val version = (versionMajor << 16) | (versionMinor << 8) | versionPatch

  override def apply(som: Spinex): Unit = {
    val deviceIdBits = B(deviceIdInt, 32 bits)
    busCtrl.read(deviceIdBits, 0x00, documentation = "deviceId")

    // Version information
    busCtrl.read(B(version, 32 bits), 0x04, documentation = "version") // Version
    busCtrl.read(B(gitHashValue, 32 bits), 0x08, documentation = "hash") // Git hash
    busCtrl.read(B(buildTimestamp, 32 bits), 0x0C, documentation = "timestamp") // Build timestamp

    super.apply(som)
  }

  override def appendDeviceTree(dt: DeviceTree): Unit = {
    super.appendDeviceTree(dt)

    dt.addEntry(f"deviceId = < 0x${deviceIdInt.toHexString} >; /* ${deviceId} */ ", baseEntryPath:_*)
    dt.addEntry(f"version = < 0x${version.toHexString} >;", baseEntryPath:_*)
    dt.addEntry(f"gitHash = < 0x${gitHashValue.toString(16)} >;", baseEntryPath:_*)
    dt.addEntry(f"buildTimestamp = < 0x${buildTimestamp.toString(16)} >;", baseEntryPath:_*)
  }
}

object IdentificationPlugin {
  // Helper to get git hash at compile time
  def getGitHash(): BigInt = {
    import scala.sys.process._
    try {
      val gitHash = "git rev-parse --short=8 HEAD".!!.trim
      BigInt(gitHash, 16)
    } catch {
      case _: Exception => BigInt("deadbeef", 16)
    }
  }

  // Helper to get build timestamp
  def getBuildTimestamp(): BigInt = {
    BigInt(System.currentTimeMillis() / 1000) // Unix timestamp
  }
}