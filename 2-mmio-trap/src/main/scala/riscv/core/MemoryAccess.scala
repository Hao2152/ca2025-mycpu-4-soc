// SPDX-License-Identifier: MIT
// MyCPU is freely redistributable under the MIT License. See the file
// "LICENSE" for information on usage and redistribution of this file.

package riscv.core

import chisel3._
import chisel3.util._
import peripheral.RAMBundle
import riscv.Parameters

// Memory Access stage: handles load/store operations with proper byte/halfword/word alignment
class MemoryAccess extends Module {
  val io = IO(new Bundle() {
    val alu_result          = Input(UInt(Parameters.DataWidth))
    val reg2_data           = Input(UInt(Parameters.DataWidth))
    val memory_read_enable  = Input(Bool())
    val memory_write_enable = Input(Bool())
    val funct3              = Input(UInt(3.W))

    val wb_memory_read_data = Output(UInt(Parameters.DataWidth))

    val memory_bundle = Flipped(new RAMBundle)
  })
  val mem_address_index = io.alu_result(log2Up(Parameters.WordSize) - 1, 0).asUInt

  io.memory_bundle.write_enable := false.B
  io.memory_bundle.write_data   := 0.U
  io.memory_bundle.address      := io.alu_result
  io.memory_bundle.write_strobe := VecInit(Seq.fill(Parameters.WordSize)(false.B))
  io.wb_memory_read_data        := 0.U

  // ============================================================
  // [CA25: Exercise 12] Load Data Extension - Sign and Zero Extension
  // ============================================================
  
  when(io.memory_read_enable) {
    val data  = io.memory_bundle.read_data
    val bytes = Wire(Vec(Parameters.WordSize, UInt(Parameters.ByteWidth)))
    for (i <- 0 until Parameters.WordSize) {
      bytes(i) := data((i + 1) * Parameters.ByteBits - 1, i * Parameters.ByteBits)
    }
    val byte = bytes(mem_address_index)
    val half = Mux(mem_address_index(1), Cat(bytes(3), bytes(2)), Cat(bytes(1), bytes(0)))

    io.wb_memory_read_data := MuxLookup(io.funct3, 0.U)(
      Seq(

        // LB (sign-extend byte)
        InstructionsTypeL.lb -> MuxLookup(mem_address_index,
          Cat(Fill(24, bytes(3)(7)), bytes(3)))(
          Seq(
            0.U -> Cat(Fill(24, bytes(0)(7)), bytes(0)),
            1.U -> Cat(Fill(24, bytes(1)(7)), bytes(1)),
            2.U -> Cat(Fill(24, bytes(2)(7)), bytes(2))
          )
        ),

        // LBU (zero-extend byte)
        InstructionsTypeL.lbu -> MuxLookup(mem_address_index,
          Cat(Fill(24, 0.U), bytes(3)))(
          Seq(
            0.U -> Cat(Fill(24, 0.U), bytes(0)),
            1.U -> Cat(Fill(24, 0.U), bytes(1)),
            2.U -> Cat(Fill(24, 0.U), bytes(2))
          )
        ),

        // LH (sign-extend halfword)
        InstructionsTypeL.lh -> Mux(
          mem_address_index === 0.U,
          Cat(Fill(16, half(15)), half),
          Cat(Fill(16, half(15)), half)
        ),

        // LHU (zero-extend halfword)
        InstructionsTypeL.lhu -> Mux(
          mem_address_index === 0.U,
          Cat(Fill(16, 0.U), half),
          Cat(Fill(16, 0.U), half)
        ),

        // LW (no extension)
        InstructionsTypeL.lw -> data
      )
    )

  // ============================================================
  // [CA25: Exercise 13] Store Data Alignment - Byte Strobes and Shifting
  // ============================================================
  }.elsewhen(io.memory_write_enable) {
    io.memory_bundle.write_data   := io.reg2_data
    io.memory_bundle.write_enable := true.B
    io.memory_bundle.write_strobe := VecInit(Seq.fill(Parameters.WordSize)(false.B))

    when(io.funct3 === InstructionsTypeS.sb) {
      // SB: enable 1 byte strobe, shift 8 * index
      io.memory_bundle.write_strobe(mem_address_index) := true.B
      io.memory_bundle.write_data :=
        (io.reg2_data(7, 0) << (mem_address_index << 3.U))

    }.elsewhen(io.funct3 === InstructionsTypeS.sh) {
      // SH: halfword write (bytes 0-1 or 2-3)
      when(mem_address_index(1) === 0.U) {
        // lower halfword (bytes 0,1)
        for (i <- 0 until Parameters.WordSize / 2) {
          io.memory_bundle.write_strobe(i) := true.B
        }
        io.memory_bundle.write_data := io.reg2_data(15, 0)
      }.otherwise {
        // upper halfword (bytes 2,3)
        for (i <- Parameters.WordSize / 2 until Parameters.WordSize) {
          io.memory_bundle.write_strobe(i) := true.B
        }
        io.memory_bundle.write_data :=
          (io.reg2_data(15, 0) << 16)
      }

    }.elsewhen(io.funct3 === InstructionsTypeS.sw) {
      for (i <- 0 until Parameters.WordSize) {
        io.memory_bundle.write_strobe(i) := true.B
      }
    }
  }
}