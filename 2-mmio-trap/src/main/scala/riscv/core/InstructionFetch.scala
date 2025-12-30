// SPDX-License-Identifier: MIT
// MyCPU is freely redistributable under the MIT License. See the file
// "LICENSE" for information on usage and redistribution of this file.

package riscv.core

import chisel3._
import riscv.Parameters

// Program counter reset value
object ProgramCounter {
  val EntryAddress = Parameters.EntryAddress
}

// Instruction Fetch stage: maintains PC, fetches instructions, and handles interrupts
class InstructionFetch extends Module {
  val io = IO(new Bundle {
    val jump_flag_id              = Input(Bool())
    val jump_address_id           = Input(UInt(Parameters.AddrWidth))
    val interrupt_assert          = Input(Bool())
    val interrupt_handler_address = Input(UInt(Parameters.AddrWidth))
    val instruction_read_data     = Input(UInt(Parameters.DataWidth))
    val instruction_valid         = Input(Bool())

    val instruction_address = Output(UInt(Parameters.AddrWidth))
    val instruction         = Output(UInt(Parameters.InstructionWidth))
  })

  // Program counter register (PC)
  val pc = RegInit(ProgramCounter.EntryAddress)

  // ============================================================
  // [CA25: Exercise 15] PC Update Logic - Sequential vs Control Flow with Interrupts
  // ============================================================
  
  when(io.instruction_valid) {
    io.instruction := io.instruction_read_data

    // Priority: Interrupt > Jump > Sequential
    pc := Mux(
      io.interrupt_assert,                         // interrupt has highest priority
      io.interrupt_handler_address,                // PC = interrupt handler
      Mux(
        io.jump_flag_id,                           // next priority: jump/branch
        io.jump_address_id,                        // PC = jump target
        pc + 4.U                                    // otherwise sequential PC+4
      )
    )

  }.otherwise {
    // instruction invalid → hold PC and issue NOP
    pc             := pc
    io.instruction := 0x00000013.U // NOP
  }

  io.instruction_address := pc
}