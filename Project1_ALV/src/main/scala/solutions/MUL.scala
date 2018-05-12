package solutions

import chisel3._
import chisel3.util.Cat

class MUL extends Module {
    val io = IO(new Bundle{
        val a = Input(UInt(32.W))
        val b = Input(UInt(32.W))
        val c = Output(UInt((64).W))
    })
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
