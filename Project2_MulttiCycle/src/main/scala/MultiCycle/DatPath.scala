/*
 * Copyright 2018/5/12 Yan Wang (2015301517).
 * learn from https://github.com/zavs/ercesiMIPS
 *
 * Use of this source code is creating a MulttiCycle MIPS CPU
 * This is a project of my Class-ComputerOrganization
 *
 */

package MultiCycle

import chisel3.core.Bool
import chisel3._
import chisel3.util._

// define the exten
// the module is used to change the 16b num into 32b num by sign
class exten extends Module{
	val io = IO(new Bundle{
		val ExtOp				= Input(Bool())
		val inputexten 	= Input(UInt(16.W))
		val outputexten	= Output(UInt(32.W))
		})

  // choose the suit output by ExtOp
	when(io.ExtOp === 0.U){
		io.outputexten := Cat(Fill(16, 0.U), io.inputexten)
	}.elsewhen(io.ExtOp === 1.U){
		io.outputexten := Cat(Fill(16, io.inputexten(15)), io.inputexten)
	}

}

// define the IO from Data to Ctr
class DatToCtlIo extends Bundle()
{
	val zero 	 = Output(Bool())
	//send instruction to ctlpath from IR
	val Dec_Inst = Output(UInt(32.W))
}

// define the Io of Datapath
class DPathIo extends Bundle()
{
	val imem_addr	= Output(UInt(32.W))
	val Inst     	= Input(UInt(32.W))
	val dmem_addr	= Output(UInt(32.W))
	val dmem_datIn	= Output(UInt(32.W))
	val dmem_datOut	= Input(UInt(32.W))
	val ctl 		= Flipped(new CtltoDatIo)
	val dat  		= new DatToCtlIo()

	// use for test
	val test_rf_address = Input(UInt(5.W))
	val test_rf_data = Input(UInt(32.W))
	val test_write_rf = Input(Bool())
	val test_nextpc	= Output(UInt(32.W))
}

// define the Datapath
class DatPath extends Module {
	val io = IO(new DPathIo ())
	val pc 	= Module(new PC)
	val alu 	= Module(new ALU())
	val reg 	  	= Module(new Register())
	val exten  	= Module(new exten())

 // get Inst
	val reg_IR = RegInit(0.U(32.W))
	when(io.ctl.IR_en === 1.U){
		reg_IR := io.Inst
	}

 // connetct to the ctlpath
	val reg_E = RegInit(0.U(32.W))
	when(io.ctl.E_en === 1.U){
		reg_E := Mux((reg.io.busA^reg.io.busB) === 0.U, 1.U, 0.U)
	}

 // connect to busA
	val reg_A = RegInit(0.U(32.W))
	when(io.ctl.A_en === 1.U){
		reg_A := reg.io.busA
	}

 // connect to busB
	val reg_B = RegInit(0.U(32.W))
	when(io.ctl.B_en === 1.U){
		reg_B := reg.io.busB
	}

// connect to result of ALU
	val reg_S = RegInit(0.U(32.W))
	when(io.ctl.S_en === 1.U){
		reg_S := alu.io.ALUout
	}

 // connect to data of dmem
	val reg_M = RegInit(0.U(32.W))
	when(io.ctl.M_en === 1.U){
		reg_M := io.dmem_datOut
	}

 // RTL Register Transist Level
	io.dat.Dec_Inst := reg_IR

	//judge whether zero by xor
	io.dat.zero		:= reg_E


	//connect to register file
	//RegDst = 1 write data to rd, RegDst = 0, write data to rt
	reg.io.read_A := io.ctl.Rs
	reg.io.read_B := io.ctl.Rt

	// use for test
	reg.io.write_A		:= Mux(io.test_write_rf === 1.U, io.test_rf_address, Mux(io.ctl.RegDst === 1.U, io.ctl.Rd, io.ctl.Rt))
	reg.io.writeData			:= Mux(io.test_write_rf === 1.U, io.test_rf_data, Mux(io.ctl.MemtoReg === 1.U, reg_S, reg_M))
	reg.io.RegWrite		 	:= Mux(io.test_write_rf === 1.U, 1.U, io.ctl.RegWr)


	//connect to exten
	exten.io.ExtOp 			  := io.ctl.ExtOp
	exten.io.inputexten := io.ctl.Imm16


	//connect to PC
	pc.io.jumpAddr  := io.ctl.Imm26
	pc.io.beqAddr := exten.io.outputexten
	pc.io.isJump		 := io.ctl.jmp_nPC_MUX_sel
	pc.io.isBeq		 := io.ctl.beq_nPC_MUX_sel
	pc.io.PC_en		 := io.ctl.PC_en

	io.imem_addr 		 := pc.io.iMem_Addr

  //ALUsrc = 0, ALU input data comes from BusB,
  //ALUsrc = 1, ALU input data comes from imm/addr 16 extend to 32
	alu.io.in1 	:= reg_A
	alu.io.in2 	:= Mux(io.ctl.ALUsrc === 0.U, reg_B, exten.io.outputexten)
	alu.io.ALUctr := io.ctl.ALUctr

	io.dmem_addr 	:= reg_S
	io.dmem_datIn   := reg_B

	io.test_nextpc	:= pc.io.test_nextpc

}
