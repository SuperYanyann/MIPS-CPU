package solutions

import chisel3.iotesters.PeekPokeTester


class MULtests(c: MUL) extends PeekPokeTester(c){
  for (input1 <- 2 until 4){
      for (input2 <- 2 until 4){
        poke(c.io.a , input1)
        poke(c.io.b , input2)
        step(1)
        expect(c.io.c , input1 * input2)
      }
  }
}

/*
class MULtests(c: MUL) extends PeekPokeTester(c){
  for (i <- 0 until 10) {
    val a = rnd.nextInt(1 << 15)
    val b = rnd.nextInt(1 << 15)
    poke(c.io.a, a)
    poke(c.io.b, b)
    step(1)
    //printf("%d %d\n", a, b)
    expect(c.io.c, a * b)
}
}
*/
