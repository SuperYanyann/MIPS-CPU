package solutions

import chisel3.iotesters.PeekPokeTester

class ALU_test(c: ALU) extends PeekPokeTester(c){

  // test add
  for (input1 <- 0 until 32){
      for (input2 <- 0 until 32){
        poke(c.io.a , input1)
        poke(c.io.b , input2)
        poke(c.io.opcode, 32)
        poke(c.io.start, 0)
        step(1)
        expect(c.io.c , input1+input2)
      }
  }


  // test sub
  for (input1 <- 16 until 32){
      for (input2 <- 1 until 16){
        poke(c.io.a , input1)
        poke(c.io.b , input2)
        poke(c.io.opcode, 26)
        poke(c.io.start, 0)
        step(1)
        expect(c.io.c , input1/input2)
      }
  }


  // test mult
  for (input1 <- 2 until 16){
      for (input2 <- 2 until 16){
        poke(c.io.a , input1)
        poke(c.io.b , input2)
        poke(c.io.opcode, 25)
        poke(c.io.start, 0)
        step(1)
        expect(c.io.c , input1*input2)
      }
  }

}
