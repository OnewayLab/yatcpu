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

// Exception codes in mcause, refer to Spec. Vol.II Page 39
// Note that exception codes in scause is a subset of mcause, refer to Spec. Vol.II Page 71
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
  val medeleg = Input(UInt(Parameters.DataWidth))
  val mideleg = Input(UInt(Parameters.DataWidth))
  val mepc = Input(UInt(Parameters.DataWidth))
  val mcause = Input(new CAUSE)
  val mtval = Input(UInt(Parameters.DataWidth))
  val mtvec = Input(new TVEC)
  val sepc = Input(UInt(Parameters.DataWidth))
  val scause = Input(new CAUSE)
  val stval = Input(UInt(Parameters.DataWidth))
  val stvec = Input(new TVEC)

  val mstatus_write_data = Output(new MSTATUS)
  val mepc_write_data = Output(UInt(Parameters.DataWidth))
  val mcause_write_data = Output(new CAUSE)
  val mtval_write_data = Output(UInt(Parameters.DataWidth))
  val sepc_write_data = Output(UInt(Parameters.DataWidth))
  val scause_write_data = Output(new CAUSE)
  val stval_write_data = Output(UInt(Parameters.DataWidth))

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

  io.exception_token := io.exception_flag_mmu // Since we can handle exception in a single cycle,
  // there is no need to use this trick
  // TODO: Remove it later

  // Init
  io.csr_bundle.mstatus_write_data := io.csr_bundle.mstatus
  io.csr_bundle.mepc_write_data := io.csr_bundle.mepc
  io.csr_bundle.mcause_write_data := io.csr_bundle.mcause
  io.csr_bundle.mtval_write_data := io.csr_bundle.mtval
  io.csr_bundle.sepc_write_data := io.csr_bundle.sepc
  io.csr_bundle.scause_write_data := io.csr_bundle.scause
  io.csr_bundle.stval_write_data := io.csr_bundle.stval
  io.csr_bundle.direct_write_enable := false.B
  io.id_interrupt_assert := false.B
  io.id_interrupt_handler_address := 0.U

  // Trap occurs in U/S/M-mode and handled in M-mode, refer to Spec. Vol.II Page 21
  def trap_into_M(epc: UInt, interrupt: Bool, code: UInt, value: UInt): Unit = {
    io.csr_bundle.mstatus_write_data.MPIE := io.csr_bundle.mstatus.MIE // Save MIE to MPIE
    io.csr_bundle.mstatus_write_data.MIE := false.B // Disable interrupt
    io.csr_bundle.mstatus_write_data.MPP := current_privilege // Save current privilege level to MPP
    io.csr_bundle.mepc_write_data := epc
    io.csr_bundle.mcause_write_data.Interrupt := interrupt
    io.csr_bundle.mcause_write_data.ExceptionCode := code
    io.csr_bundle.mtval_write_data := value
    io.csr_bundle.direct_write_enable := true.B
    io.id_interrupt_assert := true.B
    io.id_interrupt_handler_address := Mux( // Set trap handler address according to MTVEC, refer to Spec. Vol.II Page 29-30
      interrupt && io.csr_bundle.mtvec.Mode === TVEC.Mode.Vectored,
      io.csr_bundle.mtvec.Base + (code << 2.U),
      io.csr_bundle.mtvec.Base
    ) << 2.U
  }

  // Return from M-mode after a trap handled, refer to Spec. Vol.II Page 21
  def return_from_M(): Unit = {
    io.csr_bundle.mstatus_write_data.MIE := io.csr_bundle.mstatus.MPIE // Restore MIE from MPIE
    current_privilege := io.csr_bundle.mstatus.MPP // Restore current privilege level from MPP
    io.csr_bundle.mstatus_write_data.MPIE := true.B // Set MPIE to 1
    io.csr_bundle.mstatus_write_data.MPP := PrivilegeLevel.User // Set MPP to least-privileged mode
    when(io.csr_bundle.mstatus.MPP =/= PrivilegeLevel.Machine) {
      io.csr_bundle.mstatus_write_data.MPRV := false.B  // Set MPRV to 0 if MPP is not M
    }
    io.csr_bundle.direct_write_enable := true.B
    io.id_interrupt_assert := true.B
    io.id_interrupt_handler_address := io.csr_bundle.mepc
  }

  // Trap occurs in U/S-mode and handled in S-mode, refer to Spec. Vol.II Page 30
  def trap_into_S(epc: UInt, interrupt: Bool, code: UInt, value: UInt): Unit = {
    io.csr_bundle.mstatus_write_data.SPIE := io.csr_bundle.mstatus.SIE // Save SIE to SPIE
    io.csr_bundle.mstatus_write_data.SIE := false.B // Disable interrupt
    io.csr_bundle.mstatus_write_data.SPP := current_privilege === PrivilegeLevel.Supervisor // Save current privilege level to SPP
    io.csr_bundle.sepc_write_data := epc
    io.csr_bundle.scause_write_data.Interrupt := interrupt
    io.csr_bundle.scause_write_data.ExceptionCode := code
    io.csr_bundle.stval_write_data := value
    io.csr_bundle.direct_write_enable := true.B
    io.id_interrupt_assert := true.B
    io.id_interrupt_handler_address := Mux( // Set trap handler address according to STVEC, refer to Spec. Vol.II Page 66
      interrupt && io.csr_bundle.stvec.Mode === TVEC.Mode.Vectored,
      io.csr_bundle.stvec.Base + (code << 2.U),
      io.csr_bundle.stvec.Base
    ) << 2.U
  }

  // Return from S-mode after a trap handled
  def return_from_S(): Unit = {
    // Note that SSTATUS is a subset of MSTATUS, so we directly write MSTATUS here
    io.csr_bundle.mstatus_write_data.SIE := io.csr_bundle.mstatus.SPIE // Restore SIE from SPIE
    current_privilege := Mux(io.csr_bundle.mstatus.SPP, PrivilegeLevel.Supervisor, PrivilegeLevel.User) // Restore current privilege level from SPP
    io.csr_bundle.mstatus_write_data.SPIE := true.B // Set SPIE to 1
    io.csr_bundle.mstatus_write_data.SPP := false.B // Set SPP to 0
    io.csr_bundle.mstatus_write_data.MPRV := false.B // Because SPP always cannot be M, sret will always set MPRV to 0, refer to Spec. Vol.II Page 21
    io.csr_bundle.direct_write_enable := true.B
    io.id_interrupt_assert := true.B
    io.id_interrupt_handler_address := io.csr_bundle.sepc
  }

  // TODO: Implement exception priority
  // TODO: Write trap value to mtval/stval, refer to Spec. Vol.II Page 41 and 70
  when(io.exception_flag_id) {
    when(!current_privilege(1) && io.csr_bundle.medeleg(io.exception_code_id)) { // Exception occurs in U/S-mode and delegated to S-mode
      trap_into_S(io.instruction_address_id, false.B, io.exception_code_id, 0.U)
    }.otherwise { // Exception occurs in U/S/M-mode and handled in M-mode
      trap_into_M(io.instruction_address_id, false.B, io.exception_code_id, 0.U)
    }
  }.elsewhen(io.exception_flag_mem) {
    when(!current_privilege(1) && io.csr_bundle.medeleg(io.exception_code_mem)) { // Exception occurs in U/S-mode and delegated to S-mode
      trap_into_S(io.instruction_address_mem, false.B, io.exception_code_mem, 0.U)
    }.otherwise { // Exception occurs in U/S/M-mode and handled in M-mode
      trap_into_M(io.instruction_address_mem, false.B, io.exception_code_mem, 0.U)
    }
  }.elsewhen(io.exception_flag_mmu) {
    when(!current_privilege(1) && io.csr_bundle.medeleg(io.exception_code_mmu)) { // Exception occurs in U/S-mode and delegated to S-mode
      trap_into_S(io.instruction_address_mmu, false.B, io.exception_code_mmu, io.exception_val_mmu)
    }.otherwise { // Exception occurs in U/S/M-mode and handled in M-mode
      trap_into_M(io.instruction_address_mmu, false.B, io.exception_code_mmu, io.exception_val_mmu)
    }
  }.elsewhen(io.interrupt_flag =/= InterruptStatus.None) {
    // TODO: Support more than machine timer interrupt and machine external interrupt
    // TODO: Interrupts for lower-privilege modes, w<x, are always globally disabled regardless of the setting of any global wIE bit for the lower-privilege mode
    //  Interrupts for higher-privilege modes, y>x, are always globally enabled regardless of the setting of the global yIE bit for the higher-privilege mode
    val epc = Mux(
      io.jump_flag,
      io.jump_address,
      io.instruction_address_if,
    )
    val code = Mux(
      io.interrupt_flag(0),
      ExceptionCode.MachineTimerInterrupt,
      ExceptionCode.MachineExternalInterrupt
    )
    when(io.csr_bundle.mideleg(code)) { // Interrupt delegated to S-mode
      when(current_privilege === PrivilegeLevel.User || current_privilege === PrivilegeLevel.Supervisor && io.csr_bundle.mstatus.SIE) { // Interrupt occurs in U/S-mode
        trap_into_S(epc, true.B, code, 0.U)
      } // Note that interrupts occur in M-mode and delegated to S-mode will be ignore, refer to Spec. Vol.II Page 31
    }.elsewhen(io.csr_bundle.mstatus.MIE) { // Interrupt occurs in U/S/M-mode and handled in M-mode
      trap_into_M(epc, true.B, code, 0.U)
    }
  }.elsewhen(io.instruction_id === InstructionsRet.mret) {
    return_from_M() // Return from M-mode after a trap handled
  }.elsewhen(io.instruction_id === InstructionsRet.sret) {
    // When TSR=1, attempts to execute SRET while executing in S-mode will raise an illegal instruction exception,
    // refer to Spec. Vol.II Page 25 and 47
    when(io.csr_bundle.mstatus.TSR && current_privilege === PrivilegeLevel.Supervisor) {
      // TODO: Raise illegal instruction exception, currently not support nested trap
    }.otherwise {
      return_from_S() // Return from S-mode after a trap handled
    }
  }
}
