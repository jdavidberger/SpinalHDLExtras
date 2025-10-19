package spinalextras.lib.misc

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import scala.language.postfixOps

// LFSR-based Random Number Generator
// Uses maximal-length 64-bit LFSR (Galois) with taps at [64,63,61,60]
// Enhanced with non-linear mixing for better statistical properties
// Period: 2^64 - 1 (18,446,744,073,709,551,615)
class RandomNumberGenerator(withSeed: Boolean = false) extends Component {
  val io = new Bundle {
    val random = master Flow(UInt(32 bits))

    val seed = withSeed generate slave(Flow(UInt(64 bits)))
    val lfsrState = withSeed generate out(UInt(64 bits))
  }

  // 64-bit LFSR state - polynomial: x^64 + x^63 + x^61 + x^60 + 1
  val lfsr = Reg(UInt(64 bits)) init(BigInt("ACE1CAFEBABE1234", 16))
  lfsr.simPublic()
  
  // Galois LFSR implementation - taps at [64,63,61,60]
  val feedback = lfsr(0)
  val taps = U(BigInt("D800000000000000", 16), 64 bits)  // Bits 63, 62, 60, 59
  lfsr := (feedback ? taps | U(0)) ^ (lfsr |>> 1)
  
  // Optional seeding interface
  if(withSeed) {
    when(io.seed.fire) {
      lfsr := io.seed.payload | 1  // Ensure non-zero
    }
    io.lfsrState := lfsr
  }

  // Non-linear mixing for output:
  // 1. XOR upper and lower halves
  // 2. Rotate and XOR again for avalanche effect
  // 3. Mix with rotated version of itself
  val mixed1 = lfsr(63 downto 32) ^ lfsr(31 downto 0)
  val rotated = mixed1.rotateLeft(13)
  val mixed2 = mixed1 ^ rotated
  val final_mix = mixed2 ^ mixed2.rotateLeft(7)
  
  io.random.push(final_mix)
}

// RNG with APB3 register interface
case class RandomNumberGeneratorApb3(withSeed : Boolean = true) extends Component {
  val io = new Bundle {
    val apb = slave(Apb3(Apb3Config(addressWidth = 4, dataWidth = 32)))
  }

  val rng = new RandomNumberGenerator(withSeed = withSeed)
  val busCtrl = Apb3SlaveFactory(io.apb)

  // 0x00: Random data register (read-only, new value on each read)
  busCtrl.read(rng.io.random.payload, 0x00, documentation = "random number")
  
  // 0x04: LFSR state control lower 32 bits (read/write for seeding)
  val lfsrControlLow = busCtrl.createReadAndWrite(UInt(32 bits), 0x04, 0, documentation = "lfsr_low") init(0xBABE1234L)
  
  // 0x08: LFSR state control upper 32 bits (read/write for seeding)
  val lfsrControlHigh = busCtrl.createReadAndWrite(UInt(32 bits), 0x08, 0, documentation = "lfsr_high") init(0xACE1CAFEL)
  
  rng.io.seed.payload := lfsrControlHigh @@ lfsrControlLow
  rng.io.seed.valid := busCtrl.isWriting(0x04) || busCtrl.isWriting(0x08)
  lfsrControlLow := rng.io.lfsrState(31 downto 0)
  lfsrControlHigh := rng.io.lfsrState(63 downto 32)
}

object RandomNumberGenerator {
  def apply(): RandomNumberGenerator = new RandomNumberGenerator
  
  def main(args: Array[String]): Unit = {
    import spinalextras.lib.Config
    
    // Generate basic RNG
    Config.spinal.generateVerilog(new RandomNumberGenerator)
    
    // Generate RNG with APB3 interface
    Config.spinal.generateVerilog(new RandomNumberGeneratorApb3())
  }
}
