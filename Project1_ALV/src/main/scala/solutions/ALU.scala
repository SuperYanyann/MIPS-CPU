/*
 * Copyright 2018/5/12 Yan Wang.
 *
 * Use of this source code is creating a ALU
 * This is a project of my ComputerOrganization
 *
 */
package solutions

import chisel3._
import chisel3.printf._
import chisel3.util.Cat

class ALU extends Module
{
  val io = IO(new Bundle{
    val a = Input(UInt(32.W))
    val b = Input(UInt(32.W))
    val opcode = Input(UInt(6.W))
    val start = Input(UInt(1.W))

    val c = Output(UInt(32.W))
    val zero = Output(UInt(1.W))
    val product = Output(UInt(64.W))
    val quotient = Output(UInt(32.W))
    val remaind = Output(UInt(32.W))
    val complete = Output(UInt(1.W))
  })

    // get a adder use to count
    val adderOnly = Module(new Adder())

    // some temp val uesd
    val tempVal1 = 0.U

    // Used to avoid "Reference ... is not fully initialized"
    // learn from "https://stackoverflow.com/questions/48128034/errors-related-to-output-initialization-is-produced-when-using-decoupled-io"
    adderOnly.io.A := 0.U
    adderOnly.io.B := 0.U
    adderOnly.io.Cin := 0.U
    io.c := 0.U
    io.zero := 0.U
    io.product := 0.U
    io.quotient := 0.U
    io.remaind := 0.U
    io.complete := 0.U

    // Attention: some trick
    // if you have an unexpect output, you can use the following:
    // printf(p"myUInt = $io.c")
    when ( io.opcode === 32.U ) {        // add 	100000
 		   adderOnly.io.A := io.a
       adderOnly.io.B := io.b
       io.c := adderOnly.io.C

 	  }
    .elsewhen ( io.opcode === 34.U ) {       // sub 	100010
 		   adderOnly.io.A := io.a
       adderOnly.io.B := ~io.b + 1.U
       io.c := adderOnly.io.C
    }
    .elsewhen ( io.opcode === 24.U ){        // mult 011000

      io.c := io.a * io.b
    }
    .elsewhen ( io.opcode === 25.U ){         // multu 011001
      val width = 16
      val n = (width / 2 + 1)

      val booths = for (i <- 0 until n) yield {
          val booth = Module(new Booth(width))
          if (i == 0) {
            booth.io.a := Cat(io.a(1, 0), 0.U(1.W))
          } else if (i != 8 ) {
            booth.io.a := io.a(i*2+1, i*2-1)
          } else {
            booth.io.a := Cat(0.U((3 - (width-i*2+1)).W), io.a(width - 1, i*2-1))
          }
          booth.io.b := io.b
          booth
      }

      var acc = 0.U((width * 2).W);

      val correct_low = Cat(for (i <- 16 to 0 by -1) yield if (i % 2 == 0) booths(i/2).io.cout else 0.U(1.W))
      val correct_hgh = Cat(for (i <- (n - 2) to 0 by -1) yield Cat(1.U, if (i == 0) 1.U else 0.U))
      val correct = Cat(correct_hgh, correct_low)

      for (i <- (n - 1) to 0 by -1) {
          acc = (acc << 2) + booths(i).io.out
      }
      acc = acc + correct
      io.c := acc(width*2-1, 0)

    }
    .elsewhen ( io.opcode === 26.U ){        // div 011010

      io.c := io.a / io.b

    }
    .elsewhen ( io.opcode === 27.U ){        // divu 011011
    io.c := io.a / io.b
/*
      val Atemp = RegInit(init = 0.U(32.W))
  val Btemp = RegInit(init = 0.U(32.W))
  val tempR = RegInit(init = 0.U(32.W))

  val symbol = 0.U(1.W)
  val state = 0.U(3.W)

  io.done := 0.U

  when  ( io.done === 1.U )
  {
    switch( state )
    {
    is ( 0.U )
    {
       symbol := io.A(31)^io.B(31)
       when (io.A(31) === 1.U)
       {
         adderOnly.io.A := ~io.A
         adderOnly.io.B := 0xFFFF.U
         Atemp := adderOnly.io.Cin
       }
       .otherwise
       {
         Atemp := io.A
       }
       when (io.B(31) === 0.U)
       {
         adderOnly.io.A := ~io.B
         adderOnly.io.B := 0xFFFF.U
         Btemp := adderOnly.io.Cin
       }
       .otherwise
       {
         Atemp := io.B
       }
       state := state + 1.U
    }
    is ( 1.U )
    {
      when (Atemp < (~Btemp + 0XFFF.U))
      {
        io.dre:= Atemp
        adderOnly.io.A := state
        adderOnly.io.B := 1.U
        state := adderOnly.io.Cin
        when (symbol === 1.U)
        {
          io.C := ~io.C + 1.U
        }
      }
      .otherwise
      {
        adderOnly.io.A := Atemp
        adderOnly.io.B := Btemp
        Atemp := adderOnly.io.Cin
        adderOnly.io.A := io.C
        adderOnly.io.B := 1.U
        io.C := adderOnly.io.Cin
      }
    is (2.U)
    {
      io.C := 1.U
      state := 0.U
    }
    is (3.U)
    {
      state := 0.U}
  } } }*/

    }
    .elsewhen ( io.opcode === 36.U ){        // and 100100

      io.c := io.a & io.b

    }
    .elsewhen ( io.opcode === 37.U ){        // or 100101

      io.c := io.a | io.b

    }
    .elsewhen ( io.opcode === 38.U ){        // xor 100110

      io.c := io.a ^ io.b

    }
    .elsewhen ( io.opcode === 39.U ){        // nor 100111

      io.c := ~(io.a | io.b)

    }

}
