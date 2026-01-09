// SPDX-License-Identifier: MIT
// MyCPU is freely redistributable under the MIT License. See the file
// "LICENSE" for information on usage and redistribution of this file.

package riscv.core

import chisel3._
import chisel3.util._
import riscv.core.InstructionTypes
import riscv.core.Instructions
import riscv.core.InstructionsTypeI
import riscv.core.InstructionsTypeR
import riscv.core.InstructionsTypeM
import riscv.core.InstructionsTypeZba
import riscv.core.InstructionsTypeZbc
import riscv.core.InstructionsTypeZbs

class ALUControl extends Module {
  val io = IO(new Bundle {
    val opcode = Input(UInt(7.W))
    val funct3 = Input(UInt(3.W))
    val funct7 = Input(UInt(7.W))

    val alu_funct = Output(ALUFunctions())
  })

  io.alu_funct := ALUFunctions.zero

  switch(io.opcode) {
    is(InstructionTypes.I) {
      val aluFunctDefault = MuxLookup(
        io.funct3,
        ALUFunctions.zero
      )(
        IndexedSeq(
          InstructionsTypeI.addi  -> ALUFunctions.add,
          InstructionsTypeI.slli  -> ALUFunctions.sll,
          InstructionsTypeI.slti  -> ALUFunctions.slt,
          InstructionsTypeI.sltiu -> ALUFunctions.sltu,
          InstructionsTypeI.xori  -> ALUFunctions.xor,
          InstructionsTypeI.ori   -> ALUFunctions.or,
          InstructionsTypeI.andi  -> ALUFunctions.and,
          InstructionsTypeI.sri   -> Mux(io.funct7(5), ALUFunctions.sra, ALUFunctions.srl)
        )
      )
      when(io.funct7 === "b0100100".U) {
        io.alu_funct := MuxLookup(
          io.funct3,
          ALUFunctions.zero
        )(
          IndexedSeq(
            InstructionsTypeZbs.bclr -> ALUFunctions.bclr,
            InstructionsTypeZbs.bset -> ALUFunctions.bset,
            InstructionsTypeZbs.binv -> ALUFunctions.binv,
            InstructionsTypeZbs.bext -> ALUFunctions.bext
          )
        )
      }.otherwise {
        io.alu_funct := aluFunctDefault
      }
    }
    is(InstructionTypes.RM) {
      when(io.funct7 === "b0000001".U) {
        io.alu_funct := MuxLookup(
          io.funct3,
          ALUFunctions.zero
        )(
          IndexedSeq( // add M extension
            InstructionsTypeM.mul    -> ALUFunctions.mul,
            InstructionsTypeM.mulh   -> ALUFunctions.mulh,
            InstructionsTypeM.mulhsu -> ALUFunctions.mulhsu,
            InstructionsTypeM.mulhu  -> ALUFunctions.mulhu,
            InstructionsTypeM.div    -> ALUFunctions.div,
            InstructionsTypeM.divu   -> ALUFunctions.divu,
            InstructionsTypeM.rem    -> ALUFunctions.rem,
            InstructionsTypeM.remu   -> ALUFunctions.remu
          )
        )
      }.elsewhen(io.funct7 === "b0010000".U) {
        io.alu_funct := MuxLookup(
          io.funct3,
          ALUFunctions.zero
        )(
          IndexedSeq( // add Zba extension
            InstructionsTypeZba.sh1add -> ALUFunctions.sh1add,
            InstructionsTypeZba.sh2add -> ALUFunctions.sh2add,
            InstructionsTypeZba.sh3add -> ALUFunctions.sh3add
          )
        )
      }.elsewhen(io.funct7 === "b0000101".U) {
        io.alu_funct := MuxLookup(
          io.funct3,
          ALUFunctions.zero
        )(
          IndexedSeq(
            InstructionsTypeZbc.clmul  -> ALUFunctions.clmul,
            InstructionsTypeZbc.clmulr -> ALUFunctions.clmulr,
            InstructionsTypeZbc.clmulh -> ALUFunctions.clmulh
          )
        )
      }.elsewhen(io.funct7 === "b0100100".U) {
        io.alu_funct := MuxLookup(
          io.funct3,
          ALUFunctions.zero
        )(
          IndexedSeq(
            InstructionsTypeZbs.bclr -> ALUFunctions.bclr,
            InstructionsTypeZbs.bset -> ALUFunctions.bset,
            InstructionsTypeZbs.binv -> ALUFunctions.binv,
            InstructionsTypeZbs.bext -> ALUFunctions.bext
          )
        )
      }.otherwise {
        io.alu_funct := MuxLookup(
          io.funct3,
          ALUFunctions.zero
        )(
          IndexedSeq(
            InstructionsTypeR.add_sub -> Mux(io.funct7(5), ALUFunctions.sub, ALUFunctions.add),
            InstructionsTypeR.sll     -> ALUFunctions.sll,
            InstructionsTypeR.slt     -> ALUFunctions.slt,
            InstructionsTypeR.sltu    -> ALUFunctions.sltu,
            InstructionsTypeR.xor     -> ALUFunctions.xor,
            InstructionsTypeR.or      -> ALUFunctions.or,
            InstructionsTypeR.and     -> ALUFunctions.and,
            InstructionsTypeR.sr      -> Mux(io.funct7(5), ALUFunctions.sra, ALUFunctions.srl)
          )
        )
      }
    }
    is(InstructionTypes.B) {
      io.alu_funct := ALUFunctions.add
    }
    is(InstructionTypes.L) {
      io.alu_funct := ALUFunctions.add
    }
    is(InstructionTypes.S) {
      io.alu_funct := ALUFunctions.add
    }
    is(Instructions.jal) {
      io.alu_funct := ALUFunctions.add
    }
    is(Instructions.jalr) {
      io.alu_funct := ALUFunctions.add
    }
    is(Instructions.lui) {
      io.alu_funct := ALUFunctions.add
    }
    is(Instructions.auipc) {
      io.alu_funct := ALUFunctions.add
    }
  }
}
