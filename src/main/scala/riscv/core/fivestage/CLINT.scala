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

    val instruction_id = Input(UInt(Parameters.InstructionWidth))
    val instruction_address_if = Input(UInt(Parameters.AddrWidth))

    // Exception signals from MMU etc.
    val exception_signal = Input(Bool())
    val instruction_address_cause_exception = Input(UInt(Parameters.AddrWidth))
    val exception_cause = Input(UInt(Parameters.DataWidth))
    val exception_val = Input(UInt(Parameters.AddrWidth))
    // Trick for page-fault, synchronous with mmu
    val exception_token = Output(Bool())

    val jump_flag = Input(Bool())
    val jump_address = Input(UInt(Parameters.AddrWidth))

    val id_interrupt_handler_address = Output(UInt(Parameters.AddrWidth))
    val id_interrupt_assert = Output(Bool())

    val csr_bundle = new CSRDirectAccessBundle
  })
  val interrupt_enable = io.csr_bundle.mstatus.MIE
  val instruction_address = Mux(
    io.jump_flag,
    io.jump_address,
    io.instruction_address_if,
  )

  val mstatus_disable_interrupt = Wire(new MSTATUS)
  mstatus_disable_interrupt := io.csr_bundle.mstatus
  mstatus_disable_interrupt.MIE := false.B
  val mstatus_recover_interrupt = Wire(new MSTATUS)
  mstatus_recover_interrupt := io.csr_bundle.mstatus
  mstatus_recover_interrupt.MIE := io.csr_bundle.mstatus.MPIE

  io.exception_token := io.exception_signal   // Since we can handle exception in a single cycle,
                                              // there is no need to tell mmu whether clint has handled the exception

  when(io.exception_signal) {
    io.csr_bundle.mstatus_write_data := mstatus_disable_interrupt               // disable interrupt
    io.csr_bundle.mepc_write_data := io.instruction_address_cause_exception     // instruction address to return on a mret instruction
    io.csr_bundle.mcause_write_data := io.exception_cause.asTypeOf(new MCAUSE)  // exception cause
    io.csr_bundle.mtval_write_data := io.exception_val              // currently we have only page_fault to write MTVAL
    io.csr_bundle.direct_write_enable := true.B
    io.id_interrupt_assert := true.B
    io.id_interrupt_handler_address := io.csr_bundle.mtvec
  }.elsewhen(io.instruction_id === InstructionsEnv.ecall || io.instruction_id === InstructionsEnv.ebreak) {
    io.csr_bundle.mstatus_write_data := mstatus_disable_interrupt   // disable interrupt
    io.csr_bundle.mepc_write_data := instruction_address            // instruction address to return on a mret instruction
    io.csr_bundle.mcause_write_data.Interrupt := false.B
    io.csr_bundle.mcause_write_data.ExceptionCode := MuxLookup(
      io.instruction_id,
      ExceptionCode.Reserved,
      IndexedSeq(
        InstructionsEnv.ecall -> ExceptionCode.EnvironmentCallFromMMode,
        InstructionsEnv.ebreak -> ExceptionCode.Breakpoint,
      )
    )
    io.csr_bundle.mtval_write_data := 0.U
    io.csr_bundle.direct_write_enable := true.B
    io.id_interrupt_assert := true.B
    io.id_interrupt_handler_address := io.csr_bundle.mtvec
  }.elsewhen(io.interrupt_flag =/= InterruptStatus.None && interrupt_enable) {
    io.csr_bundle.mstatus_write_data := mstatus_disable_interrupt   // disable interrupt
    io.csr_bundle.mepc_write_data := instruction_address            // instruction address to return on a mret instruction
    io.csr_bundle.mcause_write_data.Interrupt := true.B
    io.csr_bundle.mcause_write_data.ExceptionCode := Mux(
      io.interrupt_flag(0),
      ExceptionCode.MachineTimerInterrupt,
      ExceptionCode.MachineExternalInterrupt
    )
    io.csr_bundle.mtval_write_data := 0.U
    io.csr_bundle.direct_write_enable := true.B
    io.id_interrupt_assert := true.B
    io.id_interrupt_handler_address := io.csr_bundle.mtvec
  }.elsewhen(io.instruction_id === InstructionsRet.mret) {
    io.csr_bundle.mstatus_write_data := mstatus_recover_interrupt   // recover the value of MSTATUS before interrupt
    io.csr_bundle.mepc_write_data := io.csr_bundle.mepc             // no change on MEPC
    io.csr_bundle.mcause_write_data := io.csr_bundle.mcause         // no change on MCAUSE
    io.csr_bundle.mtval_write_data := 0.U
    io.csr_bundle.direct_write_enable := true.B
    io.id_interrupt_assert := true.B
    io.id_interrupt_handler_address := io.csr_bundle.mepc
  }.otherwise {
    io.csr_bundle.mstatus_write_data := io.csr_bundle.mstatus
    io.csr_bundle.mepc_write_data := io.csr_bundle.mepc
    io.csr_bundle.mcause_write_data := io.csr_bundle.mcause
    io.csr_bundle.mtval_write_data := 0.U
    io.csr_bundle.direct_write_enable := false.B
    io.id_interrupt_assert := false.B
    io.id_interrupt_handler_address := 0.U
  }
}
