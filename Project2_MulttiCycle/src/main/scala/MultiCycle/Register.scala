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

// define the Register 5 input and 2 output
// the name of IO is same to the PPT
class Register extends Module{
	val io = IO(new Bundle{
		// the input of Reg
		val read_A 	= Input(UInt(5.W))
		val read_B  = Input(UInt(5.W))
		val write_A 	= Input(UInt(5.W))
		val writeData 		= Input(UInt(32.W))
		val RegWrite		= Input(Bool())
		// the output of Reg
		val busA		= Output(UInt(32.W))
		val busB		= Output(UInt(32.W))
		})
	val file = Mem(32, UInt(32.W))

// when the RegWrite is true , can write into
	when(io.RegWrite){
		file(io.write_A) := io.writeData
	}

	io.busA := file(io.read_A)
	io.busB := file(io.read_B)

}
