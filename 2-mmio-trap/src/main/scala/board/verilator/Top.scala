// SPDX-License-Identifier: MIT
// MyCPU is freely redistributable under the MIT License. See the file
// "LICENSE" for information on usage and redistribution of this file.

package board.verilator

import chisel3._
import chisel3.stage.ChiselStage
import peripheral._
import riscv.core.CPU
import riscv.CPUBundle
import riscv.Parameters

class Top extends Module {
  val io = IO(new CPUBundle)

  val cpu = Module(new CPU)
  cpu.io.regs_debug_read_address     := io.regs_debug_read_address
  cpu.io.csr_regs_debug_read_address := io.csr_regs_debug_read_address
  io.csr_regs_debug_read_data        := cpu.io.csr_regs_debug_read_data
  io.regs_debug_read_data            := cpu.io.regs_debug_read_data

  // intercept UART signals
  io.deviceSelect := cpu.io.deviceSelect

  // CPU instruction input is controlled by external codes
  io.memory_bundle <> cpu.io.memory_bundle
  io.instruction_address   := cpu.io.instruction_address
  cpu.io.instruction       := io.instruction
  cpu.io.instruction_valid := io.instruction_valid

  cpu.io.interrupt_flag := io.interrupt_flag

  // VGA signals pass-through
  cpu.io.vga_pixclk  := io.vga_pixclk
  io.vga_hsync       := cpu.io.vga_hsync
  io.vga_vsync       := cpu.io.vga_vsync
  io.vga_rrggbb      := cpu.io.vga_rrggbb
  io.vga_activevideo := cpu.io.vga_activevideo
  io.vga_x_pos       := cpu.io.vga_x_pos
  io.vga_y_pos       := cpu.io.vga_y_pos
}

object VerilogGenerator extends App {
  (new ChiselStage).emitVerilog(
    new Top(),
    Array("--target-dir", "2-mmio-trap/verilog/verilator")
  )
}
