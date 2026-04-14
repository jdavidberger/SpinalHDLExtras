package spinalextras.mains

import spinalextras.lib.ipgen.IPGenerator
import spinalextras.lib.mipi.GenerateByte2Pixel

object SpinalHDLExtrasIPGen {
  GenerateByte2Pixel;
  LatticePLLGenerator;
  def main(args: Array[String]): Unit = {
    IPGenerator.main(args)
  }
}