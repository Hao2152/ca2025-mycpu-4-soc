// SPDX-License-Identifier: MIT
// MyCPU is freely redistributable under the MIT License. See the file
// "LICENSE" for information on usage and redistribution of this file.

package riscv.core

import chisel3._
import chisel3.util._
import riscv.Parameters

// RISC-V Machine-mode CSR addresses (Privileged Spec Vol.II)
object CSRRegister {
  val MSTATUS  = 0x300.U(Parameters.CSRRegisterAddrWidth)
  val MIE      = 0x304.U(Parameters.CSRRegisterAddrWidth)
  val MTVEC    = 0x305.U(Parameters.CSRRegisterAddrWidth)
  val MSCRATCH = 0x340.U(Parameters.CSRRegisterAddrWidth)
  val MEPC     = 0x341.U(Parameters.CSRRegisterAddrWidth)
  val MCAUSE   = 0x342.U(Parameters.CSRRegisterAddrWidth)
  val CycleL   = 0xc00.U(Parameters.CSRRegisterAddrWidth)
  val CycleH   = 0xc80.U(Parameters.CSRRegisterAddrWidth)
}

class CSR extends Module {
  val io = IO(new Bundle {
    val reg_read_address_id    = Input(UInt(Parameters.CSRRegisterAddrWidth))
    val reg_write_enable_id    = Input(Bool())
    val reg_write_address_id   = Input(UInt(Parameters.CSRRegisterAddrWidth))
    val reg_write_data_ex      = Input(UInt(Parameters.DataWidth))
    val debug_reg_read_address = Input(UInt(Parameters.CSRRegisterAddrWidth))

    val debug_reg_read_data = Output(UInt(Parameters.DataWidth))
    val reg_read_data       = Output(UInt(Parameters.DataWidth))

    val clint_access_bundle = Flipped(new CSRDirectAccessBundle)
  })

  val mstatus  = RegInit(UInt(Parameters.DataWidth), 0.U)
  val mie      = RegInit(UInt(Parameters.DataWidth), 0.U)
  val mtvec    = RegInit(UInt(Parameters.DataWidth), 0.U)
  val mscratch = RegInit(UInt(Parameters.DataWidth), 0.U)
  val mepc     = RegInit(UInt(Parameters.DataWidth), 0.U)
  val mcause   = RegInit(UInt(Parameters.DataWidth), 0.U)
  val cycles   = RegInit(UInt(64.W), 0.U)

  // ============================================================
  // [CA25: Exercise 10] CSR Register Lookup Table - CSR Address Mapping
  // ============================================================
  val regLUT =
    IndexedSeq(
      CSRRegister.MSTATUS  -> mstatus,
      CSRRegister.MIE      -> mie,
      CSRRegister.MTVEC    -> mtvec,
      CSRRegister.MSCRATCH -> mscratch,
      CSRRegister.MEPC     -> mepc,
      CSRRegister.MCAUSE   -> mcause,

      CSRRegister.CycleL   -> cycles(31, 0),
      CSRRegister.CycleH   -> cycles(63, 32),
    )
  cycles := cycles + 1.U

  io.reg_read_data       := MuxLookup(io.reg_read_address_id, 0.U)(regLUT)
  io.debug_reg_read_data := MuxLookup(io.debug_reg_read_address, 0.U)(regLUT)

  io.clint_access_bundle.mstatus := mstatus
  io.clint_access_bundle.mtvec   := mtvec
  io.clint_access_bundle.mcause  := mcause
  io.clint_access_bundle.mepc    := mepc
  io.clint_access_bundle.mie     := mie

  // ============================================================
  // [CA25: Exercise 11] CSR Write Priority Logic
  // ============================================================
  
  when(io.clint_access_bundle.direct_write_enable) {
    mstatus := io.clint_access_bundle.mstatus_write_data
    mepc    := io.clint_access_bundle.mepc_write_data
    mcause  := io.clint_access_bundle.mcause_write_data

  }.elsewhen(io.reg_write_enable_id) {
    when(io.reg_write_address_id === CSRRegister.MSTATUS) {
      mstatus := io.reg_write_data_ex
    }.elsewhen(io.reg_write_address_id === CSRRegister.MEPC) {
      mepc := io.reg_write_data_ex
    }.elsewhen(io.reg_write_address_id === CSRRegister.MCAUSE) {
      mcause := io.reg_write_data_ex
    }
  }

  // CPU-only writable CSRs
  when(io.reg_write_enable_id) {
    when(io.reg_write_address_id === CSRRegister.MIE) {
      mie := io.reg_write_data_ex
    }.elsewhen(io.reg_write_address_id === CSRRegister.MTVEC) {
      mtvec := io.reg_write_data_ex
    }.elsewhen(io.reg_write_address_id === CSRRegister.MSCRATCH) {
      mscratch := io.reg_write_data_ex
    }
  }
}