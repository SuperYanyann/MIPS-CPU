/*
 * Copyright 2018/5/12 Yan Wang (2015301517).
 * learn from https://github.com/zavs/ercesiMIPS and Zeng
 *
 * Use of this source code is creating a MulttiCycle MIPS CPU
 * This is a project of my Class-ComputerOrganization
 *
 */

package MultiCycle

import chisel3._
import chisel3.util._

// define the IO from Ctl to Data
class CtltoDatIo extends Bundle()
{
	val beq_nPC_MUX_sel = Output(Bool())
	val jmp_nPC_MUX_sel	= Output(Bool())
	val RegWr			= Output(Bool())
	val RegDst			= Output(Bool())
	val ExtOp			= Output(Bool())
	val ALUctr			= Output(UInt(3.W))//change to 3-bits
	val ALUsrc			= Output(Bool())
	val MemtoReg		= Output(Bool())

	//val register enable signal for Multi Cycle Processor
	//7 Output
	val PC_en			= Output(Bool())
	val IR_en			= Output(Bool())
	val E_en			= Output(Bool())
	val A_en			= Output(Bool())
	val B_en 			= Output(Bool())
	val S_en			= Output(Bool())
	val M_en			= Output(Bool())

	//split instruction
	val Rd 				= Output(UInt(5.W))
	val Rt 				= Output(UInt(5.W))
	val Rs 				= Output(UInt(5.W))
	val Imm16 			= Output(UInt(16.W))
	val	Imm26			= Output(UInt(26.W))
}

class CPathIo extends Bundle()
{
	// val Inst 		= Input(UInt(32.W))
	val boot		= Input(Bool())
	// boot = 1 test mode , boot = 0, regular mode

	val MemWr		= Output(Bool())

	//If CPU stopped or any exception happens, valid signal is set to 0
	val valid		= Output(Bool())


	val ctl 		= new CtltoDatIo()
	val dat 		= Flipped(new DatToCtlIo)
	val test_op     = Output(UInt(6.W))
	val test_func	= Output(UInt(6.W))
	val test_state  = Output(UInt(5.W))
}

object opMean
{
	def R_TYPE		= 0.U(6.W)	//000000
	def ORI			= 13.U(6.W)	//0xd
	def LW			= 35.U(6.W)	//010
	def SW			= 43.U(6.W)	//110
	def BEQ			= 4.U(6.W)	//111
	def JUMP		= 2.U(6.W)

	def FUNC_ADD 	= 32.U(6.W)
	def FUNC_SUB	= 34.U(6.W)
	def FUNC_OR		= 37.U(6.W)
	def FUNC_AND	= 36.U(6.W)
	def FUNC_SLT	= 42.U(6.W)

	def ALUctr_ADD 	= 2.U(3.W)
	def ALUctr_SUB  = 6.U(3.W)
	def ALUctr_AND	= 0.U(3.W)
	def ALUctr_OR	= 1.U(3.W)
	def ALUctr_SLT	= 7.U(3.W)
	def ALUctr_BEQ  = ALUctr_SUB
}

object stateMean {
	def IF_STATE  			= 0.U(5.W)	//instruction fetched, set reg_IR
	def BOOT_TEST_STATE		= 1.U(5.W)	//boot = 1, in test mode
	def ID_STATE			= 2.U(5.W)	//when state is not JUMP, decode from instruction and set reg_A, reg_B, reg_E
	def JUMP_STATE			= 3.U(5.W)	//JUMP

	def R_TYPE_STATE		= 4.U(5.W)	//R-type alu exec
	def ORI_STATE			= 5.U(5.W)	//ori alu exec
	def LW_STATE			= 6.U(5.W)	//lw alu exec
	def SW_STATE			= 7.U(5.W)	//sw alu exec
	def BEQ_STATE			= 8.U(5.W)	//beq alu exec
	def OTHER_STATE			= 9.U(5.W)	//other invalid instruction

	def LW_READ_STATE		= 10.U(5.W)	//lw instruction, read data from Dmem in this state
	def SW_STORE_STATE		= 11.U(5.W)	//sw instruction, write data to Dmem in this state

	def R_TYPE_STORE_STATE 	= 12.U(5.W)	//store r_type result of alu in register
	def ORI_STORE_STATE		= 13.U(5.W)	//store ori result in register
	def LW_STORE_STATE		= 14.U(5.W)	//store lw data in register
	def IDLE_STATE			= 15.U(5.W)
}

import opMean._
import stateMean._

class CtlPath extends Module()
{
	val io 			= IO(new CPathIo ())
	// decoder the op
	io.ctl.Rs 		:= io.dat.Dec_Inst(25, 21)
	io.ctl.Rt 		:= io.dat.Dec_Inst(20, 16)
	io.ctl.Rd 		:= io.dat.Dec_Inst(15, 11)
	io.ctl.Imm16	:= io.dat.Dec_Inst(15, 	0)
	io.ctl.Imm26	:= io.dat.Dec_Inst(25,  0)
	val op 			 = io.dat.Dec_Inst(31, 26)
	val func   		 = io.dat.Dec_Inst(5,   0)
	// use for test
	io.test_op		:= io.dat.Dec_Inst(31, 26)
	io.test_func	:= io.dat.Dec_Inst(5, 	0)


	//define finite state Machine
	val state = RegInit(15.U(32.W))
	val next_state = Wire(UInt(32.W))

	io.test_state := state
	// printf("go into ctlpath, Dec_Inst:%x\n", io.dat.Dec_Inst)

	switch (state){
		is (IDLE_STATE){
			when(io.boot === 1.U){
				next_state := IDLE_STATE
			}
			when(io.boot === 0.U){
				next_state := IF_STATE
			}
		}
		is (IF_STATE){
			next_state := ID_STATE
		}
		is (JUMP_STATE) {
			next_state := IF_STATE
		}
		is (ID_STATE) {
			when(op === R_TYPE)	{
				next_state := R_TYPE_STATE
			}
			.elsewhen(op === ORI) {
				next_state := ORI_STATE
			}
			.elsewhen(op === LW) {
				next_state := LW_STATE
			}
			.elsewhen(op === SW) {
				next_state := SW_STATE
			}
			.elsewhen(op === BEQ) {
				next_state := BEQ_STATE
			}
			.elsewhen(op === JUMP) {
				next_state := JUMP_STATE
			}
			.otherwise {
				next_state := OTHER_STATE
			}
		}
		is (R_TYPE_STATE) {
			next_state := R_TYPE_STORE_STATE
		}
		is (ORI_STATE) {
			next_state := ORI_STORE_STATE
		}
		is (LW_STATE) {
			next_state := LW_READ_STATE
		}
		is (SW_STATE) {
			next_state := SW_STORE_STATE
		}
		is (BEQ_STATE) {
			next_state := IF_STATE
		}
		is (OTHER_STATE) {
			next_state := IF_STATE
		}
		is (LW_READ_STATE) {
			next_state := LW_STORE_STATE
		}
		is (SW_STORE_STATE) {
			next_state := IF_STATE
		}
		is (R_TYPE_STORE_STATE) {
			next_state := IF_STATE
		}
		is (ORI_STORE_STATE) {
			next_state := IF_STATE
		}
		is (LW_STORE_STATE) {
			next_state := IF_STATE
		}
	}

//According now state to set control signal

	//use x_state to represent don't care state, and set default value to zero
	def x_state	= 0.U

	when(state === IF_STATE){
		io.valid	  			:= 1.U

		io.ctl.IR_en			:= 1.U

		io.ctl.PC_en			:= 0.U
		io.ctl.E_en				:= 0.U
		io.ctl.A_en				:= 0.U
		io.ctl.B_en 			:= 0.U
		io.ctl.S_en 			:= 0.U
		io.ctl.M_en				:= 0.U

		io.ctl.RegWr			:= 0.U
		io.MemWr 				:= 0.U

		//dont't care signal
		io.ctl.RegDst			:= x_state
		io.ctl.MemtoReg			:= x_state
		io.ctl.ALUsrc			:= x_state
		io.ctl.ALUctr 			:= x_state
		io.ctl.ExtOp			:= x_state
		io.ctl.jmp_nPC_MUX_sel	:= x_state
		io.ctl.beq_nPC_MUX_sel 	:= x_state
	}
	when(state === ID_STATE){
		io.valid	  			:= 1.U

		io.ctl.E_en				:= 1.U
		io.ctl.A_en				:= 1.U
		io.ctl.B_en 			:= 1.U

		io.ctl.IR_en			:= 0.U
		io.ctl.PC_en			:= 0.U
		io.ctl.S_en 			:= 0.U
		io.ctl.M_en				:= 0.U

		io.ctl.RegWr			:= 0.U
		io.MemWr 				:= 0.U

		//dont't care signal
		io.ctl.RegDst			:= x_state
		io.ctl.MemtoReg			:= x_state
		io.ctl.ALUsrc			:= x_state
		io.ctl.ALUctr 			:= x_state
		io.ctl.ExtOp			:= x_state
		io.ctl.jmp_nPC_MUX_sel	:= x_state
		io.ctl.beq_nPC_MUX_sel 	:= x_state
	}
	when(state === JUMP_STATE){
		io.valid	  			:= 1.U

		io.ctl.PC_en			:= 1.U
		io.ctl.jmp_nPC_MUX_sel	:= 1.U

		io.ctl.beq_nPC_MUX_sel 	:= 0.U

		io.ctl.IR_en			:= 0.U
		io.ctl.E_en				:= 0.U
		io.ctl.A_en				:= 0.U
		io.ctl.B_en 			:= 0.U
		io.ctl.S_en 			:= 0.U
		io.ctl.M_en				:= 0.U

		io.ctl.RegWr			:= 0.U
		io.MemWr 				:= 0.U

		io.ctl.RegDst			:= x_state
		io.ctl.MemtoReg			:= x_state
		io.ctl.ALUsrc			:= x_state
		io.ctl.ALUctr 			:= x_state
		io.ctl.ExtOp			:= x_state
	}
	when(state === R_TYPE_STATE){

		io.ctl.S_en 			:= 1.U
		io.ctl.ALUsrc			:= 0.U
		io.ctl.IR_en			:= 0.U
		io.ctl.PC_en			:= 0.U
		io.ctl.E_en				:= 0.U
		io.ctl.A_en				:= 0.U
		io.ctl.B_en 			:= 0.U
		io.ctl.M_en				:= 0.U
		io.ctl.RegWr			:= 0.U
		io.MemWr 				:= 0.U

		//dont't care signal
		io.ctl.RegDst			:= x_state
		io.ctl.MemtoReg			:= x_state
		io.ctl.ExtOp			:= x_state
		io.ctl.beq_nPC_MUX_sel 	:= x_state
		io.ctl.jmp_nPC_MUX_sel	:= x_state


		when(func === FUNC_ADD){
			io.ctl.ALUctr := ALUctr_ADD
			io.valid	  := 1.U
		}.elsewhen(func === FUNC_SUB){
			io.ctl.ALUctr := ALUctr_SUB
			io.valid	  := 1.U
		}.elsewhen(func === FUNC_SLT){
			io.ctl.ALUctr := ALUctr_SLT
			io.valid	  := 1.U
		}.elsewhen(func === FUNC_OR){
			io.ctl.ALUctr := ALUctr_OR
			io.valid	  := 1.U
		}.elsewhen(func === FUNC_AND){
			io.ctl.ALUctr := ALUctr_AND
			io.valid	  := 1.U
		}.elsewhen(func === 0.U){		//because when
			io.ctl.ALUctr := x_state
			io.valid	  := 1.U
		}
	}
	when(state === ORI_STATE){
		io.valid	  			:= 1.U

		io.ctl.S_en 			:= 1.U
		io.ctl.ALUsrc			:= 1.U
		io.ctl.ALUctr 			:= ALUctr_OR

		io.ctl.ExtOp			:= 0.U

		io.ctl.IR_en			:= 0.U
		io.ctl.PC_en			:= 0.U
		io.ctl.E_en				:= 0.U
		io.ctl.A_en				:= 0.U
		io.ctl.B_en 			:= 0.U
		io.ctl.M_en				:= 0.U

		io.ctl.RegWr			:= 0.U
		io.MemWr 				:= 0.U

		//dont't care signal
		io.ctl.RegDst			:= x_state
		io.ctl.MemtoReg			:= x_state
		io.ctl.beq_nPC_MUX_sel 	:= x_state
		io.ctl.jmp_nPC_MUX_sel	:= x_state
	}
	when(state === LW_STATE){
		io.valid	  			:= 1.U

		io.ctl.S_en 			:= 1.U
		io.ctl.ALUsrc			:= 1.U
		io.ctl.ALUctr 			:= ALUctr_ADD
		io.ctl.ExtOp			:= 1.U

		io.ctl.IR_en			:= 0.U
		io.ctl.PC_en			:= 0.U
		io.ctl.E_en				:= 0.U
		io.ctl.A_en				:= 0.U
		io.ctl.B_en 			:= 0.U
		io.ctl.M_en				:= 0.U

		io.ctl.RegWr			:= 0.U
		io.MemWr 				:= 0.U

		//dont't care signal
		io.ctl.RegDst			:= x_state
		io.ctl.MemtoReg			:= x_state
		io.ctl.beq_nPC_MUX_sel 	:= x_state
		io.ctl.jmp_nPC_MUX_sel	:= x_state
	}
	when(state === SW_STATE){
		io.valid	  			:= 1.U

		io.ctl.S_en 			:= 1.U
		io.ctl.ALUsrc			:= 1.U
		io.ctl.ALUctr 			:= ALUctr_ADD
		io.ctl.ExtOp			:= 1.U

		io.ctl.IR_en			:= 0.U
		io.ctl.PC_en			:= 0.U
		io.ctl.E_en				:= 0.U
		io.ctl.A_en				:= 0.U
		io.ctl.B_en 			:= 0.U
		io.ctl.M_en				:= 0.U

		io.ctl.RegWr			:= 0.U
		io.MemWr 				:= 0.U

		//dont't care signal
		io.ctl.RegDst			:= x_state
		io.ctl.MemtoReg			:= x_state
		io.ctl.beq_nPC_MUX_sel 	:= x_state
		io.ctl.jmp_nPC_MUX_sel	:= x_state
	}
	when(state === BEQ_STATE){
		io.valid	  			:= 1.U

		io.ctl.PC_en			:= 1.U
		io.ctl.ExtOp			:= 1.U

		io.ctl.beq_nPC_MUX_sel 	:= Mux(io.dat.zero === 1.U, 1.U, 0.U)
		io.ctl.jmp_nPC_MUX_sel	:= 0.U

		io.ctl.IR_en			:= 0.U
		io.ctl.E_en				:= 0.U
		io.ctl.A_en				:= 0.U
		io.ctl.B_en 			:= 0.U
		io.ctl.S_en 			:= 0.U
		io.ctl.M_en				:= 0.U

		io.ctl.RegWr			:= 0.U
		io.MemWr 				:= 0.U

		//dont't care signal
		io.ctl.RegDst			:= x_state
		io.ctl.MemtoReg			:= x_state
		io.ctl.ALUsrc			:= x_state
		io.ctl.ALUctr 			:= x_state
	}
	when(state === OTHER_STATE){
		io.valid	  			:= 0.U

		io.ctl.PC_en			:= 1.U

		io.ctl.beq_nPC_MUX_sel 	:= 0.U
		io.ctl.jmp_nPC_MUX_sel	:= 0.U

		io.ctl.IR_en			:= 0.U
		io.ctl.E_en				:= 0.U
		io.ctl.A_en				:= 0.U
		io.ctl.B_en 			:= 0.U
		io.ctl.S_en 			:= 0.U
		io.ctl.M_en				:= 0.U

		io.ctl.RegWr			:= 0.U
		io.MemWr 				:= 0.U

		//dont't care signal
		io.ctl.RegDst			:= x_state
		io.ctl.MemtoReg			:= x_state
		io.ctl.ALUsrc			:= x_state
		io.ctl.ALUctr 			:= x_state
		io.ctl.ExtOp			:= x_state
	}
	when(state === LW_READ_STATE){
		io.valid	  			:= 1.U

		io.ctl.M_en				:= 1.U

		io.ctl.IR_en			:= 0.U
		io.ctl.PC_en			:= 0.U
		io.ctl.E_en				:= 0.U
		io.ctl.A_en				:= 0.U
		io.ctl.B_en 			:= 0.U
		io.ctl.S_en 			:= 0.U

		io.ctl.RegWr			:= 0.U
		io.MemWr 				:= 0.U

		//dont't care signal
		io.ctl.RegDst			:= x_state
		io.ctl.MemtoReg			:= x_state
		io.ctl.ALUsrc			:= x_state
		io.ctl.ALUctr 			:= x_state
		io.ctl.ExtOp			:= x_state
		io.ctl.beq_nPC_MUX_sel 	:= x_state
		io.ctl.jmp_nPC_MUX_sel	:= x_state
	}
	when(state === SW_STORE_STATE){
		io.valid	  			:= 1.U

		io.MemWr 				:= 1.U
		io.ctl.PC_en			:= 1.U

		io.ctl.beq_nPC_MUX_sel 	:= 0.U
		io.ctl.jmp_nPC_MUX_sel	:= 0.U

		io.ctl.IR_en			:= 0.U
		io.ctl.E_en				:= 0.U
		io.ctl.A_en				:= 0.U
		io.ctl.B_en 			:= 0.U
		io.ctl.S_en 			:= 0.U
		io.ctl.M_en				:= 0.U

		io.ctl.RegWr			:= 0.U

		//dont't care signal
		io.ctl.RegDst			:= x_state
		io.ctl.MemtoReg			:= x_state
		io.ctl.ALUsrc			:= x_state
		io.ctl.ALUctr 			:= x_state
		io.ctl.ExtOp			:= x_state
	}
	when(state === R_TYPE_STORE_STATE){
		io.valid	  			:= 1.U

		io.ctl.RegWr			:= 1.U
		io.ctl.MemtoReg			:= 1.U
		io.ctl.RegDst			:= 1.U
		io.ctl.PC_en			:= 1.U

		io.ctl.beq_nPC_MUX_sel 	:= 0.U
		io.ctl.jmp_nPC_MUX_sel	:= 0.U

		io.ctl.IR_en			:= 0.U
		io.ctl.E_en				:= 0.U
		io.ctl.A_en				:= 0.U
		io.ctl.B_en 			:= 0.U
		io.ctl.S_en 			:= 0.U
		io.ctl.M_en				:= 0.U

		io.MemWr 				:= 0.U

		//dont't care signal
		io.ctl.ALUsrc			:= x_state
		io.ctl.ALUctr 			:= x_state
		io.ctl.ExtOp			:= x_state
	}
	when(state === ORI_STORE_STATE){
		io.valid	  			:= 1.U

		io.ctl.RegWr			:= 1.U
		io.ctl.MemtoReg			:= 1.U
		io.ctl.PC_en			:= 1.U

		io.ctl.RegDst			:= 0.U
		io.ctl.beq_nPC_MUX_sel 	:= 0.U
		io.ctl.jmp_nPC_MUX_sel	:= 0.U

		io.ctl.IR_en			:= 0.U
		io.ctl.E_en				:= 0.U
		io.ctl.A_en				:= 0.U
		io.ctl.B_en 			:= 0.U
		io.ctl.S_en 			:= 0.U
		io.ctl.M_en				:= 0.U

		io.MemWr 				:= 0.U

		//dont't care signal
		io.ctl.ALUsrc			:= x_state
		io.ctl.ALUctr 			:= x_state
		io.ctl.ExtOp			:= x_state
	}
	when(state === LW_STORE_STATE){
		io.valid	  			:= 1.U

		io.ctl.RegWr			:= 1.U
		io.ctl.PC_en			:= 1.U

		io.ctl.MemtoReg			:= 0.U
		io.ctl.RegDst			:= 0.U
		io.ctl.beq_nPC_MUX_sel 	:= 0.U
		io.ctl.jmp_nPC_MUX_sel	:= 0.U

		io.ctl.IR_en			:= 0.U
		io.ctl.E_en				:= 0.U
		io.ctl.A_en				:= 0.U
		io.ctl.B_en 			:= 0.U
		io.ctl.S_en 			:= 0.U
		io.ctl.M_en				:= 0.U

		io.MemWr 				:= 0.U

		io.ctl.ALUsrc			:= x_state
		io.ctl.ALUctr 			:= x_state
		io.ctl.ExtOp			:= x_state
	}
	when(state === IDLE_STATE){
		io.valid 				:= 1.U

		io.ctl.PC_en			:= 0.U
		io.ctl.IR_en			:= 0.U
		io.ctl.E_en				:= 0.U
		io.ctl.A_en				:= 0.U
		io.ctl.B_en 			:= 0.U
		io.ctl.S_en 			:= 0.U
		io.ctl.M_en				:= 0.U
		io.ctl.RegWr			:= 0.U
		io.MemWr 				:= 0.U
	}

	state := next_state
}
