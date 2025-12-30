// SPDX-License-Identifier: MIT
// MyCPU is freely redistributable under the MIT License. See the file
// "LICENSE" for information on usage and redistribution of this file.

package riscv

import chisel3._
import peripheral.RAMBundle

class CPUBundle extends Bundle {
  val instruction_address         = Output(UInt(Parameters.AddrWidth))
  val instruction                 = Input(UInt(Parameters.DataWidth))
  val instruction_valid           = Input(Bool())
  val interrupt_flag              = Input(UInt(Parameters.InterruptFlagWidth))
  val memory_bundle               = Flipped(new RAMBundle)
  val deviceSelect                = Output(UInt(Parameters.SlaveDeviceCountBits.W))
  val regs_debug_read_address     = Input(UInt(Parameters.PhysicalRegisterAddrWidth))
  val regs_debug_read_data        = Output(UInt(Parameters.DataWidth))
  val csr_regs_debug_read_address = Input(UInt(Parameters.CSRRegisterAddrWidth))
  val csr_regs_debug_read_data    = Output(UInt(Parameters.DataWidth))

  // VGA outputs
  val vga_pixclk      = Input(Clock())     // VGA pixel clock (31.5 MHz)
  val vga_hsync       = Output(Bool())     // Horizontal sync
  val vga_vsync       = Output(Bool())     // Vertical sync
  val vga_rrggbb      = Output(UInt(6.W))  // 6-bit color output
  val vga_activevideo = Output(Bool())     // Active display region
  val vga_x_pos       = Output(UInt(10.W)) // Current pixel X position
  val vga_y_pos       = Output(UInt(10.W)) // Current pixel Y position
}
