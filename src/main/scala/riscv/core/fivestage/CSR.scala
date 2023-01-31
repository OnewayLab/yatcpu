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


object CSRRegister {
  // Refer to Spec. Vol.II Page 8-10
  // Supervisor-level CSRs
  val SSTATUS = 0x100.U(Parameters.CSRRegisterAddrWidth)
  val SIE = 0x104.U(Parameters.CSRRegisterAddrWidth)
  val STVEC = 0x105.U(Parameters.CSRRegisterAddrWidth)
  val SCOUNTEREN = 0x106.U(Parameters.CSRRegisterAddrWidth)
  val SENVCFG = 0x10A.U(Parameters.CSRRegisterAddrWidth)
  val SCRATCH = 0x140.U(Parameters.CSRRegisterAddrWidth)
  val SEPC = 0x141.U(Parameters.CSRRegisterAddrWidth)
  val SCAUSE = 0x142.U(Parameters.CSRRegisterAddrWidth)
  val STVAL = 0x143.U(Parameters.CSRRegisterAddrWidth)
  val SIP = 0x144.U(Parameters.CSRRegisterAddrWidth)
  val SATP = 0x180.U(Parameters.CSRRegisterAddrWidth)
  val SCONTEXT = 0x5A8.U(Parameters.CSRRegisterAddrWidth)
  // Machine-level CSRs
  val MSTATUS = 0x300.U(Parameters.CSRRegisterAddrWidth)
  val MISA = 0x301.U(Parameters.CSRRegisterAddrWidth)
  val MEDELEG = 0x302.U(Parameters.CSRRegisterAddrWidth)
  val MIDELEG = 0x303.U(Parameters.CSRRegisterAddrWidth)
  val MIE = 0x304.U(Parameters.CSRRegisterAddrWidth)
  val MTVEC = 0x305.U(Parameters.CSRRegisterAddrWidth)
  val MCOUNTEREN = 0x306.U(Parameters.CSRRegisterAddrWidth)
  val MSCRATCH = 0x340.U(Parameters.CSRRegisterAddrWidth)
  val MEPC = 0x341.U(Parameters.CSRRegisterAddrWidth)
  val MCAUSE = 0x342.U(Parameters.CSRRegisterAddrWidth)
  val MTVAL = 0x343.U(Parameters.CSRRegisterAddrWidth)
  val MIP = 0x344.U(Parameters.CSRRegisterAddrWidth)
  val MTINST = 0x34A.U(Parameters.CSRRegisterAddrWidth)
  val MTVAL2 = 0x34B.U(Parameters.CSRRegisterAddrWidth)

  val CycleL = 0xc00.U(Parameters.CSRRegisterAddrWidth)
  val CycleH = 0xc80.U(Parameters.CSRRegisterAddrWidth)
}

class MSTATUS extends Bundle {
  val SD = Bool()
  val WPRI = UInt(8.W)
  val TSR = Bool()
  val TW = Bool()
  val TVM = Bool()
  val MXR = Bool()
  val SUM = Bool()
  val MPRV = Bool()
  val XS = UInt(2.W)
  val FS = UInt(2.W)
  val MPP = UInt(2.W)
  val VS = UInt(2.W)
  val SPP = Bool()
  val MPIE = Bool()
  val UBE = Bool()
  val SPIE = Bool()
  val WPRI2 = Bool()
  val MIE = Bool()
  val WPRI3 = Bool()
  val SIE = Bool()
  val WPRI4 = Bool()
}

class MCAUSE extends Bundle {
  val Interrupt = Bool()
  val ExceptionCode = UInt(31.W)
}

class CSR extends Module {
  val io = IO(new Bundle {
    val reg_read_address_id = Input(UInt(Parameters.CSRRegisterAddrWidth))
    val reg_write_enable_ex = Input(Bool())
    val reg_write_address_ex = Input(UInt(Parameters.CSRRegisterAddrWidth))
    val reg_write_data_ex = Input(UInt(Parameters.DataWidth))

    val id_reg_read_data = Output(UInt(Parameters.DataWidth))

    val mmu_enable = Output(Bool())
    val start_paging = Output(Bool())

    val mmu_csr_satp = Output(UInt(Parameters.DataWidth))
    val clint_access_bundle = Flipped(new CSRDirectAccessBundle)
  })

  val satp = RegInit(UInt(Parameters.DataWidth),0.U)
  val mstatus = RegInit(UInt(Parameters.DataWidth), 0.U)
  val mie = RegInit(UInt(Parameters.DataWidth), 0.U)
  val mtvec = RegInit(UInt(Parameters.DataWidth), 0.U)
  val mscratch = RegInit(UInt(Parameters.DataWidth), 0.U)
  val mepc = RegInit(UInt(Parameters.DataWidth), 0.U)
  val mcause = RegInit(UInt(Parameters.DataWidth), 0.U)
  val mtval = RegInit(UInt(Parameters.DataWidth), 0.U)
  val cycles = RegInit(UInt(64.W), 0.U)

  val regLUT = IndexedSeq(
    CSRRegister.SATP -> satp,
    CSRRegister.MSTATUS -> mstatus,
    CSRRegister.MIE -> mie,
    CSRRegister.MTVEC -> mtvec,
    CSRRegister.MSCRATCH -> mscratch,
    CSRRegister.MEPC -> mepc,
    CSRRegister.MCAUSE -> mcause,
    CSRRegister.MTVAL -> mtval,
    CSRRegister.CycleL -> cycles(31, 0),
    CSRRegister.CycleH -> cycles(63, 32),
  )
  cycles := cycles + 1.U

  io.mmu_enable := satp(31)
  io.start_paging := io.reg_write_enable_ex && !satp(31) && io.reg_write_data_ex(31)
  io.mmu_csr_satp := satp

  // If the pipeline and the CLINT are going to read and write the CSR at the same time, let the pipeline write first.
  // This is implemented in a single cycle by passing reg_write_data_ex to clint and writing the data from the CLINT to the CSR.
  io.id_reg_read_data := Mux(io.reg_write_enable_ex && io.reg_write_address_ex === io.reg_read_address_id, io.reg_write_data_ex, MuxLookup(io.reg_read_address_id, 0.U, regLUT))
  io.clint_access_bundle.mstatus := Mux(io.reg_write_enable_ex && io.reg_write_address_ex === CSRRegister.MSTATUS, io.reg_write_data_ex, mstatus).asTypeOf(new MSTATUS)
  io.clint_access_bundle.mtvec := Mux(io.reg_write_enable_ex && io.reg_write_address_ex === CSRRegister.MTVEC, io.reg_write_data_ex, mtvec)
  io.clint_access_bundle.mcause := Mux(io.reg_write_enable_ex && io.reg_write_address_ex === CSRRegister.MCAUSE, io.reg_write_data_ex, mcause).asTypeOf(new MCAUSE)
  io.clint_access_bundle.mepc := Mux(io.reg_write_enable_ex && io.reg_write_address_ex === CSRRegister.MEPC, io.reg_write_data_ex, mepc)
  when(io.clint_access_bundle.direct_write_enable) {
    mstatus := io.clint_access_bundle.mstatus_write_data.asUInt
    mepc := io.clint_access_bundle.mepc_write_data
    mcause := io.clint_access_bundle.mcause_write_data.asUInt
    mtval := io.clint_access_bundle.mtval_write_data
  }.elsewhen(io.reg_write_enable_ex) {
    when(io.reg_write_address_ex === CSRRegister.MSTATUS) {
      mstatus := io.reg_write_data_ex
    }.elsewhen(io.reg_write_address_ex === CSRRegister.MEPC) {
      mepc := io.reg_write_data_ex
    }.elsewhen(io.reg_write_address_ex === CSRRegister.MCAUSE) {
      mcause := io.reg_write_data_ex
    }.elsewhen(io.reg_write_address_ex === CSRRegister.MTVAL) {
      mtval := io.reg_write_data_ex
    }
  }
  when(io.reg_write_enable_ex) {
    when(io.reg_write_address_ex === CSRRegister.SATP) {
      satp := io.reg_write_data_ex
    }.elsewhen(io.reg_write_address_ex === CSRRegister.MIE) {
      mie := io.reg_write_data_ex
    }.elsewhen(io.reg_write_address_ex === CSRRegister.MTVEC) {
      mtvec := io.reg_write_data_ex
    }.elsewhen(io.reg_write_address_ex === CSRRegister.MSCRATCH) {
      mscratch := io.reg_write_data_ex
    }
  }
}
