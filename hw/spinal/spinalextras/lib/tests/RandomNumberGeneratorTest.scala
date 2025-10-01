package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinalextras.lib.Config
import spinalextras.lib.misc.RandomNumberGenerator
import scala.collection.mutable

class RandomNumberGeneratorTest extends AnyFunSuite {
  test("RNG produces varying random numbers") {
    Config.sim.withVerilator.doSim(new RandomNumberGenerator) { dut =>
      dut.clockDomain.forkStimulus(100 MHz)
      dut.clockDomain.waitSampling(10)

      val samples = mutable.Set[BigInt]()
      val sequence = mutable.ArrayBuffer[BigInt]()
      
      // Collect 100 samples
      for (i <- 0 until 100) {
        dut.clockDomain.waitSampling()
        val value = dut.io.random.toBigInt
        samples.add(value)
        sequence.append(value)
        assert(dut.io.valid.toBoolean, "Valid should always be true")
      }

      println(s"Unique values in 100 samples: ${samples.size}")
      println(s"First 10 samples: ${sequence.take(10).mkString(", ")}")
      
      // Check for randomness quality
      assert(samples.size > 95, s"Expected > 95 unique values, got ${samples.size}")
      
      // Check no consecutive identical values
      var consecutiveCount = 0
      for (i <- 1 until sequence.length) {
        if (sequence(i) == sequence(i-1)) consecutiveCount += 1
      }
      assert(consecutiveCount == 0, s"Found $consecutiveCount consecutive identical values")
      
      // Simple chi-square test for bit distribution
      val bitCounts = Array.fill(32)(0)
      for (value <- sequence) {
        for (bit <- 0 until 32) {
          if ((value & (BigInt(1) << bit)) != 0) {
            bitCounts(bit) += 1
          }
        }
      }
      
      println(s"Bit balance: ${bitCounts.mkString(", ")}")
      
      // Each bit should be set roughly 50% of the time (allow 30-70% range)
      for (bit <- 0 until 32) {
        val percentage = (bitCounts(bit) * 100.0) / sequence.length
        assert(percentage > 30 && percentage < 70, 
          s"Bit $bit balance: $percentage% (expected 30-70%)")
      }
      
      println("RNG test passed - good randomness properties observed")
    }
  }

  test("RNG seeding produces different sequences") {
    Config.sim.withVerilator.doSim(new RandomNumberGenerator) { dut =>
      dut.clockDomain.forkStimulus(100 MHz)
      
      // Get sequence from default seed
      dut.clockDomain.waitSampling(5)
      val seq1 = mutable.ArrayBuffer[BigInt]()
      for (_ <- 0 until 20) {
        dut.clockDomain.waitSampling()
        seq1.append(dut.io.random.toBigInt)
      }
      
      // Re-seed with same value and check we get same sequence
      dut.lfsr #= BigInt("ACE1CAFEBABE1234", 16)
      dut.clockDomain.waitSampling(5)
      val seq2 = mutable.ArrayBuffer[BigInt]()
      for (_ <- 0 until 20) {
        dut.clockDomain.waitSampling()
        seq2.append(dut.io.random.toBigInt)
      }
      
      // Different seeds should give different sequences
      dut.lfsr #= BigInt("1234567890ABCDEF", 16)
      dut.clockDomain.waitSampling(5)
      val seq3 = mutable.ArrayBuffer[BigInt]()
      for (_ <- 0 until 20) {
        dut.clockDomain.waitSampling()
        seq3.append(dut.io.random.toBigInt)
      }
      
      println(s"Seq1 start: ${seq1.take(5).mkString(", ")}")
      println(s"Seq2 start: ${seq2.take(5).mkString(", ")}")
      println(s"Seq3 start: ${seq3.take(5).mkString(", ")}")
      
      assert(seq1 == seq2, "Same seed should produce same sequence")
      assert(seq1 != seq3, "Different seed should produce different sequence")
      
      println("Seeding test passed")
    }
  }

  test("RNG LFSR never gets stuck at zero") {
    Config.sim.withVerilator.doSim(new RandomNumberGenerator) { dut =>
      dut.clockDomain.forkStimulus(100 MHz)
      
      // Try to force LFSR to zero (will auto-recover due to XOR with tap)
      dut.lfsr #= 0
      dut.clockDomain.waitSampling(10)
      
      // Check that LFSR recovered and produces non-zero output
      var allZero = true
      var sawNonZero = false
      for (_ <- 0 until 100) {
        dut.clockDomain.waitSampling()
        val value = dut.io.random.toBigInt
        if (value != 0) {
          sawNonZero = true
          allZero = false
        }
      }
      
      assert(sawNonZero, "RNG should produce non-zero values after recovery")
      assert(!allZero, "RNG should not get stuck outputting zeros")
      println("Zero-state recovery test passed")
    }
  }

  test("RNG uniform distribution - comprehensive statistical test") {
    Config.sim.withVerilator.doSim(new RandomNumberGenerator) { dut =>
      dut.clockDomain.forkStimulus(100 MHz)
      dut.clockDomain.waitSampling(10)

      val numSamples = 50000
      val samples = mutable.ArrayBuffer[BigInt]()
      
      println(s"Collecting $numSamples samples for statistical analysis...")
      val startTime = System.currentTimeMillis()
      
      for (_ <- 0 until numSamples) {
        dut.clockDomain.waitSampling()
        samples.append(dut.io.random.toBigInt)
      }
      
      val elapsedMs = System.currentTimeMillis() - startTime
      println(s"Collection complete in ${elapsedMs}ms (${numSamples.toDouble / elapsedMs * 1000} samples/sec)")
      
      // Test 1: Bit balance - each bit should be ~50% ones
      println("\n=== Bit Balance Test ===")
      val bitCounts = Array.fill(32)(0)
      for (value <- samples) {
        for (bit <- 0 until 32) {
          if ((value & (BigInt(1) << bit)) != 0) {
            bitCounts(bit) += 1
          }
        }
      }
      
      var bitBalancePass = true
      for (bit <- 0 until 32) {
        val percentage = (bitCounts(bit) * 100.0) / numSamples
        if (percentage < 48.0 || percentage > 52.0) {
          println(f"  Bit $bit%2d: $percentage%5.2f%% [FAIL]")
          bitBalancePass = false
        }
      }
      if (bitBalancePass) {
        println(s"  All bits within 48-52% range ✓")
      }
      assert(bitBalancePass, "Bit balance test failed")
      
      // Test 2: Chi-square test for uniformity
      // Divide 32-bit space into 256 bins
      println("\n=== Chi-Square Uniformity Test ===")
      val numBins = 256
      val bins = Array.fill(numBins)(0)
      val expectedPerBin = numSamples.toDouble / numBins
      
      for (value <- samples) {
        val bin = (value & 0xFF).toInt  // Use lower 8 bits for binning
        bins(bin) += 1
      }
      
      // Calculate chi-square statistic
      var chiSquare = 0.0
      for (bin <- bins) {
        val diff = bin - expectedPerBin
        chiSquare += (diff * diff) / expectedPerBin
      }
      
      // For 255 degrees of freedom, critical value at 0.05 significance ≈ 293
      // At 0.01 significance ≈ 310
      println(f"  Chi-square statistic: $chiSquare%.2f")
      println(f"  Expected per bin: $expectedPerBin%.2f")
      println(f"  Critical value (α=0.05): 293.25")
      
      val chiSquarePass = chiSquare < 320  // Allow some margin
      if (chiSquarePass) {
        println(s"  Distribution is uniform ✓")
      } else {
        println(s"  Distribution may not be uniform [FAIL]")
      }
      assert(chiSquarePass, s"Chi-square test failed: $chiSquare > 320")
      
      // Test 3: Runs test - check for patterns
      println("\n=== Runs Test (MSB) ===")
      var runs = 1
      var lastBit = (samples(0) >> 31) & 1
      for (i <- 1 until samples.length) {
        val currentBit = (samples(i) >> 31) & 1
        if (currentBit != lastBit) {
          runs += 1
          lastBit = currentBit
        }
      }
      
      // Expected runs ≈ N/2, std dev ≈ sqrt(N)/2
      val expectedRuns = numSamples / 2.0
      val stdDev = math.sqrt(numSamples) / 2.0
      val zScore = math.abs(runs - expectedRuns) / stdDev
      
      println(f"  Number of runs: $runs")
      println(f"  Expected runs: $expectedRuns%.0f ± ${2*stdDev}%.0f (2σ)")
      println(f"  Z-score: $zScore%.2f")
      
      val runsPass = zScore < 3.0  // Within 3 standard deviations
      if (runsPass) {
        println(s"  No obvious patterns detected ✓")
      } else {
        println(s"  Possible pattern detected [FAIL]")
      }
      assert(runsPass, s"Runs test failed: z-score = $zScore")
      
      // Test 4: Unique values ratio
      println("\n=== Uniqueness Test ===")
      val uniqueCount = samples.toSet.size
      val uniqueRatio = (uniqueCount * 100.0) / numSamples
      println(f"  Unique values: $uniqueCount / $numSamples ($uniqueRatio%.2f%%)")
      
      // With 32-bit output and 50k samples, expect >99% unique (birthday paradox)
      val uniquenessPass = uniqueRatio > 99.0
      if (uniquenessPass) {
        println(s"  High uniqueness ✓")
      } else {
        println(s"  Low uniqueness [WARNING]")
      }
      assert(uniquenessPass, s"Uniqueness test failed: only $uniqueRatio% unique")
      
      // Test 5: Serial correlation (adjacent values should be uncorrelated)
      println("\n=== Serial Correlation Test ===")
      var correlation = 0.0
      val mean = samples.map(_.toDouble).sum / samples.length
      var variance = 0.0
      
      for (value <- samples) {
        val diff = value.toDouble - mean
        variance += diff * diff
      }
      variance /= samples.length
      
      for (i <- 0 until samples.length - 1) {
        val diff1 = samples(i).toDouble - mean
        val diff2 = samples(i + 1).toDouble - mean
        correlation += diff1 * diff2
      }
      correlation /= ((samples.length - 1) * variance)
      
      println(f"  Lag-1 correlation: $correlation%.6f")
      println(f"  Expected: ~0.0 (uncorrelated)")
      println(f"  Acceptable range: < 0.15 for PRNGs")
      
      val correlationPass = math.abs(correlation) < 0.15
      if (correlationPass) {
        if (math.abs(correlation) < 0.05) {
          println(s"  Excellent: Values are nearly uncorrelated ✓")
        } else {
          println(s"  Good: Low correlation for PRNG ✓")
        }
      } else {
        println(s"  Warning: High correlation detected [FAIL]")
      }
      assert(correlationPass, f"Serial correlation test failed: $correlation%.6f > 0.15")
      
      println("\n=== All Statistical Tests Passed ===")
    }
  }
}


