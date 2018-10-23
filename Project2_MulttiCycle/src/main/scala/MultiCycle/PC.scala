/*
 * Copyright 2018/5/12 Yan Wang (2015301517).
 *
 * Use of this source code is creating a MulttiCycle MIPS CPU
 * This is a project of my Class-ComputerOrganization
 *
 */

package MultiCycle

import chisel3._
import chisel3.core.Bool
import chisel3.util._

class PC extends Module{
	val io = IO(new Bundle{
		val isJump			= Input(Bool())
		val isBeq			= Input(Bool())
		val jumpAddr	= Input(UInt(26.W))
		val beqAddr	= Input(UInt(32.W))
		val iMem_Addr		= Output(UInt(32.W))
		val test_nextpc		= Output(UInt(32.W))
		val PC_en			= Input(Bool())
		})
	val nextPC = Wire(UInt(32.W))
	val PCReg = RegInit(0.U(32.W))

	// write PCReg bt PC_en
	when(io.PC_en === 1.U){
		PCReg := nextPC
	}

	// after PCRed add 4
	val PCafter = PCReg + 4.U
	//get addr when jump
	val Cat_jumpAddr = Cat(PCafter(31, 28), io.jumpAddr, 0.U(2.W))
	//get addr when beq (pc+4+beqShamt)
	val addShamt_beqAddr = PCafter+(io.beqAddr << 2)	//in chisel shift left is to add zero in the tail

 // get the nextPC
	when(io.isJump){
		nextPC := Cat_jumpAddr
	}.elsewhen(io.isBeq){
		nextPC := addShamt_beqAddr
	}.otherwise{
		nextPC := PCafter
	}

	io.iMem_Addr := PCReg

	when(io.PC_en === 1.U){
		io.test_nextpc := nextPC
	}.otherwise{
		io.test_nextpc := PCReg
	}

}
