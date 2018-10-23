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

object ALU
{
	def ADD	= 2.U(3.W)	//010
	def SUB	= 6.U(3.W)	//110
	def AND	= 0.U(3.W)	//000
	def OR	= 1.U(3.W)	//001
	def SLT	= 7.U(3.W)	//111
	def BEQ	= SUB	//011
	def whetherSUB(cmd: UInt) = cmd === SUB || cmd === SLT
}

import ALU._

class ALU extends Module{
	val io = IO(new Bundle{
		val in1		= Input(UInt(32.W))
		val in2 	= Input(UInt(32.W))
		val ALUctr 	= Input(UInt(3.W))
		val ALUout	= Output(UInt(32.W))
		val cmp_out	= Output(Bool())
		})

	// get ADD or SUB by ALUctr
	val in2_inv = Mux(whetherSUB(io.ALUctr), ~io.in2, io.in2)//
	val getAdd = io.in1 + in2_inv + whetherSUB(io.ALUctr)

	// get Xor
	val getXor = io.in1 ^ in2_inv

  // get the cout
	io.cmp_out := ( io.in1 - io.in2 ) === 0.U

	// get AND and OR
 	val logic_out = Mux(io.ALUctr === OR, getXor, 0.U) |
  	Mux(io.ALUctr === OR || io.ALUctr === AND, io.in1 & io.in2, 0.U)

	// output the ADD or SUB
  val out = Mux(io.ALUctr === ADD || io.ALUctr === SUB, getAdd, logic_out)

	// output the STL
	val getSLTout = Mux(io.in1 < io.in2, 1.U(32.W), 0.U(32.W))

	io.ALUout := Mux(io.ALUctr === SLT, getSLTout, out)
}
