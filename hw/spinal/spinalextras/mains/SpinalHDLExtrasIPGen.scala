package spinalextras.mains

import spinalextras.lib.ipgen.IPGenerator
import spinalextras.lib.mipi.GenerateByte2Pixel
import spinalextras.lib.soc.spinex.GenerateSpinex

object SpinalHDLExtrasIPGen {
  GenerateByte2Pixel;
  LatticePLLGenerator;
  GenerateSpinex;
  def main(args: Array[String]): Unit = {
    IPGenerator.main(args)
  }
}