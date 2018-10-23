package solutions

import chisel3._
import chisel3.util._
import chisel3.printf._

class Adder extends Module {
  val io = IO(new Bundle {
    val A    = Input(UInt(32.W))
    val B    = Input(UInt(32.W))
    val Cin  = Input(UInt(1.W))
    val C  = Output(UInt(32.W))
    val Cout = Output(UInt(1.W))
  })


  val FAs   = Array.fill(32)(Module(new FullAdder()).io)
  val carry = Wire(Vec(32+1, UInt(1.W)))
  val sum   = Wire(Vec(32, Bool()))
  val temp1 = RegInit(init = 0.U(32.W))
  val temp2 = RegInit(init = 0.U(32.W))

  carry(0) := io.Cin
  temp1 := io.A
  temp2 := io.B
  //printf(p"A = $temp1 \n")
  //printf(p"B = $temp2 \n")

  for (i <- 0 until 32) {
    FAs(i).a := io.A(i)
    FAs(i).b := io.B(i)
    FAs(i).cin := carry(i)
    carry(i+1) := FAs(i).cout
    sum(i) := FAs(i).sum.toBool()
  }
  io.C:= sum.asUInt
  io.Cout := carry(32)
}
