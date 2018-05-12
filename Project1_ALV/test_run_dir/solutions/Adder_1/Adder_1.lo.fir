circuit Adder_1 : @[:@2.0]
  module Adder_1 : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_a : UInt<32> @[:@6.4]
    input io_b : UInt<32> @[:@6.4]
    output io_c : UInt<32> @[:@6.4]
  
    node _T_11 = add(io_a, io_b) @[Adder_1.scala 14:17:@8.4]
    node _T_12 = tail(_T_11, 1) @[Adder_1.scala 14:17:@9.4]
    io_c <= _T_12
