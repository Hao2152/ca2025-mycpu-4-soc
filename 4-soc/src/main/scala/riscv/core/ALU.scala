// SPDX-License-Identifier: MIT
// MyCPU is freely redistributable under the MIT License. See the file
// "LICENSE" for information on usage and redistribution of this file.

package riscv.core

import chisel3._
import chisel3.util._
import riscv.Parameters

/**
 * ALU function encoding. Maps to RV32I instruction funct3/funct7 combinations.
 *
 * The ALUControl module translates instruction encoding to these operations.
 * Special case 'zero' produces constant 0 for non-ALU instructions that still
 * flow through the execute stage (e.g., stores use ALU for address calculation
 * but don't need a separate zero output).
 */
object ALUFunctions extends ChiselEnum {
  val zero, add, sub, sll, slt, xor, or, and, srl, sra, sltu,
  mul, mulh, mulhsu, mulhu, div, divu, rem, remu, sh1add, sh2add, sh3add = Value
}

/**
 * Arithmetic Logic Unit: combinational compute engine for the Execute stage.
 *
 * Implements all RV32I integer operations in a single cycle:
 * - Arithmetic: ADD (also used for address calculation), SUB
 * - Logical: AND, OR, XOR (bitwise operations)
 * - Shift: SLL (left), SRL (logical right), SRA (arithmetic right, sign-extends)
 * - Compare: SLT (signed), SLTU (unsigned) - output 1 if op1 < op2, else 0
 *
 * Shift amounts use only lower 5 bits of op2 (RISC-V spec: shamt[4:0]).
 * Comparison results are 1-bit values zero-extended to 32 bits.
 *
 * Critical path: SLT/SLTU comparisons and SRA (requires sign extension logic).
 */
class ALU extends Module {
  val io = IO(new Bundle {
    val func = Input(ALUFunctions())

    val op1 = Input(UInt(Parameters.DataWidth))
    val op2 = Input(UInt(Parameters.DataWidth))

    val result = Output(UInt(Parameters.DataWidth))
  })

  val op1Sext64 = Cat(Fill(Parameters.DataBits, io.op1(Parameters.DataBits - 1)), io.op1).asSInt
  val op2Sext64 = Cat(Fill(Parameters.DataBits, io.op2(Parameters.DataBits - 1)), io.op2).asSInt
  val op1Zext64 = Cat(0.U(Parameters.DataBits.W), io.op1)
  val op2Zext64 = Cat(0.U(Parameters.DataBits.W), io.op2)

  val mulUU = op1Zext64 * op2Zext64
  val mulSS = op1Sext64 * op2Sext64
  val mulSU = op1Sext64 * op2Zext64.asSInt

  val divByZero   = io.op2 === 0.U
  val divOverflow = (io.op1 === "h80000000".U(Parameters.DataWidth)) && (io.op2 === "hffffffff".U(Parameters.DataWidth))

  val divSignedResult = Mux(
    divByZero,
    (-1.S(Parameters.DataWidth)).asUInt,
    Mux(divOverflow, io.op1, (io.op1.asSInt / io.op2.asSInt).asUInt)
  )
  val divUnsignedResult = Mux(divByZero, Fill(Parameters.DataBits, 1.U), io.op1 / io.op2)

  val remSignedResult = Mux(
    divByZero,
    io.op1,
    Mux(divOverflow, 0.U, (io.op1.asSInt % io.op2.asSInt).asUInt)
  )
  val remUnsignedResult = Mux(divByZero, io.op1, io.op1 % io.op2)

  val sh1addResult = ((io.op1 << 1).asUInt +& io.op2)(Parameters.DataBits - 1, 0)
  val sh2addResult = ((io.op1 << 2).asUInt +& io.op2)(Parameters.DataBits - 1, 0)
  val sh3addResult = ((io.op1 << 3).asUInt +& io.op2)(Parameters.DataBits - 1, 0)

  io.result := 0.U
  switch(io.func) {
    is(ALUFunctions.add) {
      io.result := io.op1 + io.op2
    }
    is(ALUFunctions.sub) {
      io.result := io.op1 - io.op2
    }
    is(ALUFunctions.sll) {
      io.result := io.op1 << io.op2(4, 0)
    }
    is(ALUFunctions.slt) {
      io.result := io.op1.asSInt < io.op2.asSInt
    }
    is(ALUFunctions.xor) {
      io.result := io.op1 ^ io.op2
    }
    is(ALUFunctions.or) {
      io.result := io.op1 | io.op2
    }
    is(ALUFunctions.and) {
      io.result := io.op1 & io.op2
    }
    is(ALUFunctions.srl) {
      io.result := io.op1 >> io.op2(4, 0)
    }
    is(ALUFunctions.sra) {
      io.result := (io.op1.asSInt >> io.op2(4, 0)).asUInt
    }
    is(ALUFunctions.sltu) {
      io.result := io.op1 < io.op2
    }
    is(ALUFunctions.mul) {
      io.result := mulUU(Parameters.DataBits - 1, 0)
    }
    is(ALUFunctions.mulh) {
      io.result := mulSS(Parameters.DataBits * 2 - 1, Parameters.DataBits).asUInt
    }
    is(ALUFunctions.mulhsu) {
      io.result := mulSU(Parameters.DataBits * 2 - 1, Parameters.DataBits).asUInt
    }
    is(ALUFunctions.mulhu) {
      io.result := mulUU(Parameters.DataBits * 2 - 1, Parameters.DataBits)
    }
    is(ALUFunctions.div) {
      io.result := divSignedResult
    }
    is(ALUFunctions.divu) {
      io.result := divUnsignedResult
    }
    is(ALUFunctions.rem) {
      io.result := remSignedResult
    }
    is(ALUFunctions.remu) {
      io.result := remUnsignedResult
    }
    is(ALUFunctions.sh1add) {
      io.result := sh1addResult
    }
    is(ALUFunctions.sh2add) {
      io.result := sh2addResult
    }
    is(ALUFunctions.sh3add) {
      io.result := sh3addResult
    }
  }
}
