// SPDX-License-Identifier: MIT
// MyCPU is freely redistributable under the MIT License. See the file
// "LICENSE" for information on usage and redistribution of this file.

package riscv

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.core.ALU
import riscv.core.ALUFunctions

class ALUTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("ALU")

  // Helper to run ALU operation
  def runALU(dut: ALU, func: ALUFunctions.Type, op1: Long, op2: Long): Long = {
    dut.io.func.poke(func)
    dut.io.op1.poke(op1.U)
    dut.io.op2.poke(op2.U)
    dut.io.result.peekInt().toLong & 0xffffffffL
  }

  // ==================== Arithmetic Operations ====================

  it should "perform ADD correctly" in {
    test(new ALU).withAnnotations(TestAnnotations.annos) { dut =>
      assert(runALU(dut, ALUFunctions.add, 5, 3) == 8)
      assert(runALU(dut, ALUFunctions.add, 0, 0) == 0)
      assert(runALU(dut, ALUFunctions.add, 0xffffffffL, 1) == 0) // Overflow wraps
      assert(runALU(dut, ALUFunctions.add, 0x7fffffffL, 1) == 0x80000000L)
    }
  }

  it should "perform SUB correctly" in {
    test(new ALU).withAnnotations(TestAnnotations.annos) { dut =>
      assert(runALU(dut, ALUFunctions.sub, 10, 3) == 7)
      assert(runALU(dut, ALUFunctions.sub, 5, 5) == 0)
      assert(runALU(dut, ALUFunctions.sub, 0, 1) == 0xffffffffL) // Underflow wraps
      assert(runALU(dut, ALUFunctions.sub, 0x80000000L, 1) == 0x7fffffffL)
    }
  }

  // ==================== Logical Operations ====================

  it should "perform AND correctly" in {
    test(new ALU).withAnnotations(TestAnnotations.annos) { dut =>
      assert(runALU(dut, ALUFunctions.and, 0xff00ff00L, 0x0f0f0f0fL) == 0x0f000f00L)
      assert(runALU(dut, ALUFunctions.and, 0xffffffffL, 0x00000000L) == 0)
      assert(runALU(dut, ALUFunctions.and, 0xaaaaaaaaL, 0x55555555L) == 0)
    }
  }

  it should "perform OR correctly" in {
    test(new ALU).withAnnotations(TestAnnotations.annos) { dut =>
      assert(runALU(dut, ALUFunctions.or, 0xff00ff00L, 0x00ff00ffL) == 0xffffffffL)
      assert(runALU(dut, ALUFunctions.or, 0x00000000L, 0x00000000L) == 0)
      assert(runALU(dut, ALUFunctions.or, 0xaaaaaaaaL, 0x55555555L) == 0xffffffffL)
    }
  }

  it should "perform XOR correctly" in {
    test(new ALU).withAnnotations(TestAnnotations.annos) { dut =>
      assert(runALU(dut, ALUFunctions.xor, 0xff00ff00L, 0x0f0f0f0fL) == 0xf00ff00fL)
      assert(runALU(dut, ALUFunctions.xor, 0xffffffffL, 0xffffffffL) == 0)
      assert(runALU(dut, ALUFunctions.xor, 0xaaaaaaaaL, 0x55555555L) == 0xffffffffL)
    }
  }

  // ==================== Shift Operations ====================

  it should "perform SLL (shift left logical) correctly" in {
    test(new ALU).withAnnotations(TestAnnotations.annos) { dut =>
      assert(runALU(dut, ALUFunctions.sll, 1, 0) == 1)
      assert(runALU(dut, ALUFunctions.sll, 1, 4) == 16)
      assert(runALU(dut, ALUFunctions.sll, 1, 31) == 0x80000000L)
      assert(runALU(dut, ALUFunctions.sll, 0xff, 8) == 0xff00L)
      // Only lower 5 bits of shift amount used
      assert(runALU(dut, ALUFunctions.sll, 1, 32) == 1) // 32 & 0x1F = 0
    }
  }

  it should "perform SRL (shift right logical) correctly" in {
    test(new ALU).withAnnotations(TestAnnotations.annos) { dut =>
      assert(runALU(dut, ALUFunctions.srl, 16, 4) == 1)
      assert(runALU(dut, ALUFunctions.srl, 0x80000000L, 31) == 1)
      assert(runALU(dut, ALUFunctions.srl, 0xff00L, 8) == 0xff)
      // Logical shift: zero-fill from left
      assert(runALU(dut, ALUFunctions.srl, 0xffffffffL, 16) == 0xffffL)
    }
  }

  it should "perform SRA (shift right arithmetic) correctly" in {
    test(new ALU).withAnnotations(TestAnnotations.annos) { dut =>
      // Positive numbers (sign bit = 0) should behave like SRL
      assert(runALU(dut, ALUFunctions.sra, 0x7fffffffL, 4) == 0x07ffffffL)

      // Negative numbers (sign bit = 1) should sign-extend
      assert(runALU(dut, ALUFunctions.sra, 0x80000000L, 4) == 0xf8000000L)
      assert(runALU(dut, ALUFunctions.sra, 0xfffffff0L, 4) == 0xffffffffL)
    }
  }

  // ==================== Comparison Operations ====================

  it should "perform SLT (set less than, signed) correctly" in {
    test(new ALU).withAnnotations(TestAnnotations.annos) { dut =>
      // Positive comparisons
      assert(runALU(dut, ALUFunctions.slt, 5, 10) == 1)
      assert(runALU(dut, ALUFunctions.slt, 10, 5) == 0)
      assert(runALU(dut, ALUFunctions.slt, 5, 5) == 0)

      // Signed comparisons: -1 (0xFFFFFFFF) < 0
      assert(runALU(dut, ALUFunctions.slt, 0xffffffffL, 0) == 1) // -1 < 0
      assert(runALU(dut, ALUFunctions.slt, 0, 0xffffffffL) == 0) // 0 < -1 is false

      // Edge cases
      assert(runALU(dut, ALUFunctions.slt, 0x80000000L, 0x7fffffffL) == 1) // MIN < MAX
      assert(runALU(dut, ALUFunctions.slt, 0x7fffffffL, 0x80000000L) == 0) // MAX < MIN is false
    }
  }

  it should "perform SLTU (set less than, unsigned) correctly" in {
    test(new ALU).withAnnotations(TestAnnotations.annos) { dut =>
      // Basic comparisons
      assert(runALU(dut, ALUFunctions.sltu, 5, 10) == 1)
      assert(runALU(dut, ALUFunctions.sltu, 10, 5) == 0)
      assert(runALU(dut, ALUFunctions.sltu, 5, 5) == 0)

      // Unsigned comparisons: 0xFFFFFFFF > 0 (unsigned)
      assert(runALU(dut, ALUFunctions.sltu, 0xffffffffL, 0) == 0) // MAX_UINT > 0
      assert(runALU(dut, ALUFunctions.sltu, 0, 0xffffffffL) == 1) // 0 < MAX_UINT

      // Edge cases (different from signed)
      assert(runALU(dut, ALUFunctions.sltu, 0x80000000L, 0x7fffffffL) == 0) // 2^31 > 2^31-1 unsigned
      assert(runALU(dut, ALUFunctions.sltu, 0x7fffffffL, 0x80000000L) == 1)
    }
  }

  // ==================== Special Operations ====================

  it should "output zero for ALUFunctions.zero" in {
    test(new ALU).withAnnotations(TestAnnotations.annos) { dut =>
      assert(runALU(dut, ALUFunctions.zero, 0xffffffffL, 0xffffffffL) == 0)
      assert(runALU(dut, ALUFunctions.zero, 123, 456) == 0)
    }
  }

  // ==================== Edge Cases ====================

  it should "handle all-ones operands correctly" in {
    test(new ALU).withAnnotations(TestAnnotations.annos) { dut =>
      val allOnes = 0xffffffffL

      assert(runALU(dut, ALUFunctions.add, allOnes, allOnes) == 0xfffffffeL)
      assert(runALU(dut, ALUFunctions.and, allOnes, allOnes) == allOnes)
      assert(runALU(dut, ALUFunctions.or, allOnes, allOnes) == allOnes)
      assert(runALU(dut, ALUFunctions.xor, allOnes, allOnes) == 0)
    }
  }

  it should "handle zero operands correctly" in {
    test(new ALU).withAnnotations(TestAnnotations.annos) { dut =>
      assert(runALU(dut, ALUFunctions.add, 0, 0) == 0)
      assert(runALU(dut, ALUFunctions.sub, 0, 0) == 0)
      assert(runALU(dut, ALUFunctions.and, 0, 0xffffffffL) == 0)
      assert(runALU(dut, ALUFunctions.or, 0, 0) == 0)
      assert(runALU(dut, ALUFunctions.sll, 0, 31) == 0)
      assert(runALU(dut, ALUFunctions.srl, 0, 31) == 0)
    }
  }

  // ==================== M Extension Operations ====================

  it should "perform multiplication variants correctly" in {
    test(new ALU).withAnnotations(TestAnnotations.annos) { dut =>
      assert(runALU(dut, ALUFunctions.mul, 0x10000L, 0x10000L) == 0)
      assert(runALU(dut, ALUFunctions.mulh, 0x10000L, 0x10000L) == 1) // 0x1_0000 * 0x1_0000 = 0x1_0000_0000
      assert(runALU(dut, ALUFunctions.mulhsu, 0xffffffffL, 2) == 0xffffffffL) // (-1 * 2) >> 32 = 0xFFFFFFFF
      assert(runALU(dut, ALUFunctions.mulhu, 0xffffffffL, 0xffffffffL) == 0xfffffffeL) // (2^32 - 1)^2 high bits
    }
  }

  it should "perform division and remainder variants correctly" in {
    test(new ALU).withAnnotations(TestAnnotations.annos) { dut =>
      assert(runALU(dut, ALUFunctions.div, 7, 0xfffffffdL) == 0xfffffffeL) // 7 / -3 = -2
      assert(runALU(dut, ALUFunctions.div, 0x80000000L, 0xffffffffL) == 0x80000000L) // overflow case
      assert(runALU(dut, ALUFunctions.div, 123, 0) == 0xffffffffL) // divide by zero
      assert(runALU(dut, ALUFunctions.divu, 10, 3) == 3)
      assert(runALU(dut, ALUFunctions.divu, 0, 0) == 0xffffffffL)

      assert(runALU(dut, ALUFunctions.rem, 7, 0xfffffffdL) == 1) // 7 % -3 = 1
      assert(runALU(dut, ALUFunctions.rem, 0x80000000L, 0xffffffffL) == 0) // overflow remainder
      assert(runALU(dut, ALUFunctions.rem, 5, 0) == 5) // divide by zero remainder
      assert(runALU(dut, ALUFunctions.remu, 10, 3) == 1)
      assert(runALU(dut, ALUFunctions.remu, 0xdeadbeefL, 0) == 0xdeadbeefL)
    }
  }

  // ==================== Zba Shift-Add Operations ====================

  it should "perform shift-add instructions correctly" in {
    test(new ALU).withAnnotations(TestAnnotations.annos) { dut =>
      assert(runALU(dut, ALUFunctions.sh1add, 5, 2) == 12)
      assert(runALU(dut, ALUFunctions.sh2add, 3, 4) == 16)
      assert(runALU(dut, ALUFunctions.sh3add, 1, 1) == 9)
      assert(runALU(dut, ALUFunctions.sh3add, 0x20000000L, 1) == 1) // overflow wraps to low 32 bits
    }
  }
}
