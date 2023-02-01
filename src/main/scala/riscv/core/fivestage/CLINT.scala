// Copyright 2021 Howard Lau
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package riscv.core.fivestage

import chisel3._
import chisel3.util._
import riscv.Parameters

object InterruptStatus {
  val None = 0x0.U(8.W)
  val Timer0 = 0x1.U(8.W)
  val Ret = 0xFF.U(8.W)
}

object PrivilegeLevel {
  val User = 0.U(2.W)
  val Supervisor = 1.U(2.W)
  val Machine = 3.U(2.W)
}

object ExceptionCode {
  // Interrupts
  val SupervisorSoftwareInterrupt = 1.U((Parameters.DataBits - 1).W)
  val MachineSoftwareInterrupt = 3.U((Parameters.DataBits - 1).W)
  val SupervisorTimerInterrupt = 5.U((Parameters.DataBits - 1).W)
  val MachineTimerInterrupt = 7.U((Parameters.DataBits - 1).W)
  val SupervisorExternalInterrupt = 9.U((Parameters.DataBits - 1).W)
  val MachineExternalInterrupt = 11.U((Parameters.DataBits - 1).W)
  // Exceptions
  val InstructionAddressMisaligned = 0.U((Parameters.DataBits - 1).W)
  val InstructionAccessFault = 1.U((Parameters.DataBits - 1).W)
  val IllegalInstruction = 2.U((Parameters.DataBits - 1).W)
  val Breakpoint = 3.U((Parameters.DataBits - 1).W)
  val LoadAddressMisaligned = 4.U((Parameters.DataBits - 1).W)
  val LoadAccessFault = 5.U((Parameters.DataBits - 1).W)
  val StoreAddressMisaligned = 6.U((Parameters.DataBits - 1).W)
  val StoreAccessFault = 7.U((Parameters.DataBits - 1).W)
  val EnvironmentCallFromUMode = 8.U((Parameters.DataBits - 1).W)
  val EnvironmentCallFromSMode = 9.U((Parameters.DataBits - 1).W)
  val Reserved = 10.U((Parameters.DataBits - 1).W)
  val EnvironmentCallFromMMode = 11.U((Parameters.DataBits - 1).W)
  val InstructionPageFault = 12.U((Parameters.DataBits - 1).W)
  val LoadPageFault = 13.U((Parameters.DataBits - 1).W)
  val StorePageFault = 15.U((Parameters.DataBits - 1).W)
}

class CSRDirectAccessBundle extends Bundle {
  val mstatus = Input(new MSTATUS)
  val mepc = Input(UInt(Parameters.DataWidth))
  val mcause = Input(new MCAUSE)
  val mtvec = Input(UInt(Parameters.DataWidth))

  val mstatus_write_data = Output(new MSTATUS)
  val mepc_write_data = Output(UInt(Parameters.DataWidth))
  val mcause_write_data = Output(new MCAUSE)
  val mtval_write_data = Output(UInt(Parameters.DataWidth))

  val direct_write_enable = Output(Bool())
}

// Core Local Interrupt Controller
class CLINT extends Module {
  val io = IO(new Bundle {
    // Interrupt signals from peripherals
    val interrupt_flag = Input(UInt(Parameters.InterruptFlagWidth))

    // Exception signals from MMU etc.
    val exception_flag_id = Input(Bool())
    val exception_flag_mem = Input(Bool())
    val exception_flag_mmu = Input(Bool())
    val exception_code_id = Input(UInt((Parameters.DataBits - 1).W))
    val exception_code_mem = Input(UInt((Parameters.DataBits - 1).W))
    val exception_code_mmu = Input(UInt((Parameters.DataBits - 1).W))
    val exception_val_mmu = Input(UInt(Parameters.AddrWidth))

    val instruction_id = Input(UInt(Parameters.InstructionWidth))
    val instruction_address_if = Input(UInt(Parameters.AddrWidth))
    val instruction_address_id = Input(UInt(Parameters.AddrWidth))
    val instruction_address_mem = Input(UInt(Parameters.AddrWidth))
    val instruction_address_mmu = Input(UInt(Parameters.AddrWidth))

    // Trick for page-fault, synchronous with mmu
    val exception_token = Output(Bool())

    val jump_flag = Input(Bool())
    val jump_address = Input(UInt(Parameters.AddrWidth))

    val id_interrupt_handler_address = Output(UInt(Parameters.AddrWidth))
    val id_interrupt_assert = Output(Bool())

    val current_privilege = Output(UInt(2.W))

    val csr_bundle = new CSRDirectAccessBundle
  })

  val current_privilege = RegInit(PrivilegeLevel.Machine) // Current privilege level
  io.current_privilege := current_privilege

  io.exception_token := io.exception_flag_mmu   // Since we can handle exception in a single cycle,
                                                // there is no need to use this trick
                                                // TODO: Remove it later

  def set_mstatus(): Unit = {
    io.csr_bundle.mstatus_write_data := io.csr_bundle.mstatus
    io.csr_bundle.mstatus_write_data.MPIE := io.csr_bundle.mstatus.MIE  // Save MIE to MPIE
    io.csr_bundle.mstatus_write_data.MIE := false.B  // Disable interrupt
    io.csr_bundle.mstatus_write_data.MPP := current_privilege  // Save current privilege level to MPP
  }

  def recover_mstatus(): Unit = {
    io.csr_bundle.mstatus_write_data := io.csr_bundle.mstatus
    io.csr_bundle.mstatus_write_data.MIE := io.csr_bundle.mstatus.MPIE  // Recover MIE from MPIE
    current_privilege := io.csr_bundle.mstatus.MPP  // Recover privilege level from MPP
  }

  io.csr_bundle.mstatus_write_data := io.csr_bundle.mstatus
  io.csr_bundle.mepc_write_data := io.csr_bundle.mepc
  io.csr_bundle.mcause_write_data := io.csr_bundle.mcause
  io.csr_bundle.mtval_write_data := 0.U
  io.csr_bundle.direct_write_enable := false.B
  io.id_interrupt_assert := false.B
  io.id_interrupt_handler_address := 0.U

  when(io.exception_flag_id) {
    set_mstatus()
    io.csr_bundle.mepc_write_data := io.instruction_address_id
    io.csr_bundle.mcause_write_data.Interrupt := false.B
    io.csr_bundle.mcause_write_data.ExceptionCode := io.exception_code_id
    io.csr_bundle.direct_write_enable := true.B
    io.id_interrupt_assert := true.B
    io.id_interrupt_handler_address := io.csr_bundle.mtvec
  }.elsewhen(io.exception_flag_mem) {
    set_mstatus()
    io.csr_bundle.mepc_write_data := io.instruction_address_mem
    io.csr_bundle.mcause_write_data.Interrupt := false.B
    io.csr_bundle.mcause_write_data.ExceptionCode := io.exception_code_mem
    io.csr_bundle.direct_write_enable := true.B
    io.id_interrupt_assert := true.B
    io.id_interrupt_handler_address := io.csr_bundle.mtvec
  }.elsewhen(io.exception_flag_mmu) {
    set_mstatus()
    io.csr_bundle.mepc_write_data := io.instruction_address_mmu
    io.csr_bundle.mcause_write_data.Interrupt := false.B
    io.csr_bundle.mcause_write_data.ExceptionCode := io.exception_code_mmu
    io.csr_bundle.mtval_write_data := io.exception_val_mmu
    io.csr_bundle.direct_write_enable := true.B
    io.id_interrupt_assert := true.B
    io.id_interrupt_handler_address := io.csr_bundle.mtvec
  }.elsewhen(io.interrupt_flag =/= InterruptStatus.None && io.csr_bundle.mstatus.MIE) {
    set_mstatus()
    io.csr_bundle.mepc_write_data := Mux(
      io.jump_flag,
      io.jump_address,
      io.instruction_address_if,
    )
    io.csr_bundle.mcause_write_data.Interrupt := true.B
    io.csr_bundle.mcause_write_data.ExceptionCode := Mux(
      io.interrupt_flag(0),
      ExceptionCode.MachineTimerInterrupt,
      ExceptionCode.MachineExternalInterrupt
    )
    io.csr_bundle.direct_write_enable := true.B
    io.id_interrupt_assert := true.B
    io.id_interrupt_handler_address := io.csr_bundle.mtvec
  }.elsewhen(io.instruction_id === InstructionsRet.mret) {
    recover_mstatus()
    io.csr_bundle.direct_write_enable := true.B
    io.id_interrupt_assert := true.B
    io.id_interrupt_handler_address := io.csr_bundle.mepc
  }
}
