package solutions

import chisel3.iotesters.PeekPokeTester

class AdderTests(c: Adder) extends PeekPokeTester(c){
  for (input1 <- 2 until 4){
      for (input2 <- 2 until 4){
        //val temp = b'0001'
        poke(c.io.A , input1 )
        poke(c.io.B , input2)
        poke(c.io.Cin, 0)
        step(1)
        expect(c.io.C , input1 + input2)
        expect(c.io.Cout, 0)
      }
    }

}
