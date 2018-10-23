;buildInfoPackage: chisel3, version: 3.1.0, scalaVersion: 2.11.12, sbtVersion: 1.1.1, builtAtString: 2018-04-17 19:22:56.455, builtAtMillis: 1523992976455
circuit Adder_1 : 
  module Adder_1 : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<32>, flip b : UInt<32>, c : UInt<32>}
    
    node _T_11 = add(io.a, io.b) @[Adder_1.scala 14:17]
    node _T_12 = tail(_T_11, 1) @[Adder_1.scala 14:17]
    io.c <= _T_12 @[Adder_1.scala 14:9]
    
