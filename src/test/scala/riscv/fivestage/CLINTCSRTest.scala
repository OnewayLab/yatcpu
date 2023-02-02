// Copyright 2022 hrpccs
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

package riscv.fivestage

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import riscv.core.fivestage._
import riscv.{Parameters, TestAnnotations}

class CLINTCSRTestTopModule extends Module {
  val io = IO(new Bundle {
    val csr_regs_read_address = Input(UInt(Parameters.CSRRegisterAddrWidth))
    val csr_regs_write_address = Input(UInt(Parameters.CSRRegisterAddrWidth))
    val csr_regs_write_data = Input(UInt(Parameters.DataWidth))
    val csr_regs_write_enable = Input(Bool())

    val interrupt_flag = Input(UInt(Parameters.InterruptFlagWidth))
    val exception_flag_id = Input(Bool())
    val exception_flag_mem = Input(Bool())
    val exception_flag_mmu = Input(Bool())
    val exception_code_id = Input(UInt((Parameters.DataBits - 1).W))
    val exception_code_mem = Input(UInt((Parameters.DataBits - 1).W))
    val exception_code_mmu = Input(UInt((Parameters.DataBits - 1).W))
    val exception_value_id = Input(UInt(Parameters.DataWidth))
    val exception_value_mem = Input(UInt(Parameters.DataWidth))
    val exception_value_mmu = Input(UInt(Parameters.DataWidth))

    val instruction_id = Input(UInt(Parameters.InstructionWidth))
    val instruction_address_if = Input(UInt(Parameters.AddrWidth))
    val instruction_address_id = Input(UInt(Parameters.AddrWidth))
    val instruction_address_mem = Input(UInt(Parameters.AddrWidth))
    val instruction_address_mmu = Input(UInt(Parameters.AddrWidth))

    val jump_flag = Input(Bool())
    val jump_address = Input(UInt(Parameters.AddrWidth))

    val interrupt_assert = Output(Bool())
    val interrupt_handler_address = Output(UInt(Parameters.DataWidth))
    val csr_regs_read_data = Output(UInt(Parameters.DataWidth))
  })
  val csr_regs = Module(new CSR)
  val clint = Module(new CLINT)

  clint.io.interrupt_flag := io.interrupt_flag
  clint.io.exception_flag_id := io.exception_flag_id
  clint.io.exception_flag_mem := io.exception_flag_mem
  clint.io.exception_flag_mmu := io.exception_flag_mmu
  clint.io.exception_code_id := io.exception_code_id
  clint.io.exception_code_mem := io.exception_code_mem
  clint.io.exception_code_mmu := io.exception_code_mmu
  clint.io.exception_value_id := io.exception_value_id
  clint.io.exception_value_mem := io.exception_value_mem
  clint.io.exception_value_mmu := io.exception_value_mmu
  clint.io.instruction_id := io.instruction_id
  clint.io.instruction_address_if := io.instruction_address_if
  clint.io.instruction_address_id := io.instruction_address_id
  clint.io.instruction_address_mem := io.instruction_address_mem
  clint.io.instruction_address_mmu := io.instruction_address_mmu
  clint.io.jump_flag := io.jump_flag
  clint.io.jump_address := io.jump_address

  io.interrupt_handler_address := clint.io.id_interrupt_handler_address
  io.interrupt_assert := clint.io.id_interrupt_assert
  io.csr_regs_read_data := csr_regs.io.id_reg_read_data
  csr_regs.io.reg_read_address_id := io.csr_regs_read_address
  csr_regs.io.reg_write_address_ex := io.csr_regs_write_address
  csr_regs.io.reg_write_data_ex := io.csr_regs_write_data
  csr_regs.io.reg_write_enable_ex := io.csr_regs_write_enable

  csr_regs.io.clint_access_bundle <> clint.io.csr_bundle

}

class CLINTCSRTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "CSR and CLINT of Five Stage CPU"
  it should "handle interrupts" in {
    test(new CLINTCSRTestTopModule).withAnnotations(TestAnnotations.annos) { c =>
      // Init CSRs
      c.io.jump_flag.poke(false.B)
      c.io.csr_regs_write_enable.poke(false.B)
      c.io.interrupt_flag.poke(0.U)
      c.clock.step()
      c.io.csr_regs_write_enable.poke(true.B)
      c.io.csr_regs_write_address.poke(CSRRegister.MTVEC)
      c.io.csr_regs_write_data.poke(0x1144L.U)
      c.clock.step()
      c.io.csr_regs_write_address.poke(CSRRegister.MSTATUS)
      c.io.csr_regs_write_data.poke(0x1888L.U)
      c.clock.step()
      c.io.csr_regs_write_enable.poke(false.B)

      // Handle interrupt when not jumping
      c.io.jump_flag.poke(false.B)
      c.io.instruction_address_if.poke(0x1900L.U)
      c.io.instruction_id.poke(InstructionsNop.nop)
      c.io.interrupt_flag.poke(1.U)
      c.io.interrupt_assert.expect(true.B)
      c.io.interrupt_handler_address.expect(0x1144L.U)
      c.clock.step()
      c.io.interrupt_flag.poke(0.U)
      c.io.csr_regs_read_address.poke(CSRRegister.MEPC)
      c.io.csr_regs_read_data.expect(0x1900L.U)
      c.io.csr_regs_read_address.poke(CSRRegister.MCAUSE)
      c.io.csr_regs_read_data.expect(0x80000007L.U)
      c.io.csr_regs_read_address.poke(CSRRegister.MSTATUS)
      c.io.csr_regs_read_data.expect(0x1880L.U)

      c.clock.step(25)

      // Return from interrupt handler
      c.io.instruction_id.poke(InstructionsRet.mret)
      c.io.interrupt_assert.expect(true.B)
      c.io.interrupt_handler_address.expect(0x1900L.U)
      c.clock.step()
      c.io.csr_regs_read_address.poke(CSRRegister.MSTATUS)
      c.io.csr_regs_read_data.expect(0x88L.U)

      // Handle interrupt when jumping
      c.io.jump_flag.poke(true.B)
      c.io.jump_address.poke(0x1990L.U)
      c.io.interrupt_flag.poke(2.U)
      c.io.interrupt_assert.expect(true.B)
      c.io.interrupt_handler_address.expect(0x1144L.U)
      c.clock.step()
      c.io.interrupt_flag.poke(0.U)
      c.io.csr_regs_read_address.poke(CSRRegister.MEPC)
      c.io.csr_regs_read_data.expect(0x1990L.U)
      c.io.csr_regs_read_address.poke(CSRRegister.MCAUSE)
      c.io.csr_regs_read_data.expect(0x8000000BL.U)
      c.io.csr_regs_read_address.poke(CSRRegister.MSTATUS)
      c.io.csr_regs_read_data.expect(0x1880L.U)

      c.clock.step(25)

      // Return from interrupt handler
      c.io.instruction_id.poke(InstructionsRet.mret)
      c.io.interrupt_assert.expect(true.B)
      c.io.interrupt_handler_address.expect(0x1990L.U)
      c.clock.step()
      c.io.csr_regs_read_address.poke(CSRRegister.MSTATUS)
      c.io.csr_regs_read_data.expect(0x88L.U)
      c.io.instruction_id.poke(InstructionsNop.nop)

      // Don't handle interrupt when global interrupt disabled
      c.io.csr_regs_write_enable.poke(true.B)
      c.io.csr_regs_write_address.poke(CSRRegister.MSTATUS)
      c.io.csr_regs_write_data.poke(0x1880L.U)
      c.io.interrupt_flag.poke(1.U)
      c.io.interrupt_assert.expect(false.B)
    }
  }

  it should "handle exceptions" in {
    test(new CLINTCSRTestTopModule).withAnnotations(TestAnnotations.annos) { c =>
      // Init CSRs
      c.io.instruction_id.poke(InstructionsNop.nop)
      c.io.jump_flag.poke(false.B)
      c.io.csr_regs_write_enable.poke(false.B)
      c.io.interrupt_flag.poke(0.U)
      c.clock.step()
      c.io.csr_regs_write_enable.poke(true.B)
      c.io.csr_regs_write_address.poke(CSRRegister.MTVEC)
      c.io.csr_regs_write_data.poke(0x1144L.U)
      c.clock.step()
      c.io.csr_regs_write_address.poke(CSRRegister.MSTATUS)
      c.io.csr_regs_write_data.poke(0x1888L.U)
      c.clock.step()
      c.io.csr_regs_write_enable.poke(false.B)

      // EnvironmentCallFromUMode from ID
      c.io.instruction_address_id.poke(0x1900L.U)
      c.io.exception_flag_id.poke(true.B)
      c.io.exception_code_id.poke(ExceptionCode.EnvironmentCallFromUMode)
      c.io.interrupt_assert.expect(true.B)
      c.io.interrupt_handler_address.expect(0x1144L.U)
      c.clock.step()
      c.io.exception_flag_id.poke(false.B)
      c.io.csr_regs_read_address.poke(CSRRegister.MEPC)
      c.io.csr_regs_read_data.expect(0x1900L.U)
      c.io.csr_regs_read_address.poke(CSRRegister.MCAUSE)
      c.io.csr_regs_read_data.expect(ExceptionCode.EnvironmentCallFromUMode)
      c.io.csr_regs_read_address.poke(CSRRegister.MSTATUS)
      c.io.csr_regs_read_data.expect(0x1880L.U)
      c.clock.step(25)
      // Return from exception handler
      c.io.instruction_id.poke(InstructionsRet.mret)
      c.io.interrupt_assert.expect(true.B)
      c.io.interrupt_handler_address.expect(0x1900L.U)
      c.clock.step()
      c.io.instruction_id.poke(InstructionsNop.nop)
      c.io.csr_regs_read_address.poke(CSRRegister.MSTATUS)
      c.io.csr_regs_read_data.expect(0x88L.U)

      // InstructionAddressMisaligned from ID
      c.io.instruction_address_id.poke(0x1990L.U)
      c.io.exception_flag_id.poke(true.B)
      c.io.exception_code_id.poke(ExceptionCode.InstructionAddressMisaligned)
      c.io.interrupt_assert.expect(true.B)
      c.io.interrupt_handler_address.expect(0x1144L.U)
      c.clock.step()
      c.io.exception_flag_id.poke(false.B)
      c.io.csr_regs_read_address.poke(CSRRegister.MEPC)
      c.io.csr_regs_read_data.expect(0x1990L.U)
      c.io.csr_regs_read_address.poke(CSRRegister.MCAUSE)
      c.io.csr_regs_read_data.expect(ExceptionCode.InstructionAddressMisaligned)
      c.io.csr_regs_read_address.poke(CSRRegister.MSTATUS)
      c.io.csr_regs_read_data.expect(0x1880L.U)
      c.clock.step(25)
      // Return from exception handler
      c.io.instruction_id.poke(InstructionsRet.mret)
      c.io.interrupt_assert.expect(true.B)
      c.io.interrupt_handler_address.expect(0x1990L.U)
      c.clock.step()
      c.io.instruction_id.poke(InstructionsNop.nop)
      c.io.csr_regs_read_address.poke(CSRRegister.MSTATUS)
      c.io.csr_regs_read_data.expect(0x88L.U)
    }
  }
}
