circuit ALU : @[:@2.0]
  module FullAdder : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_a : UInt<1> @[:@6.4]
    input io_b : UInt<1> @[:@6.4]
    input io_cin : UInt<1> @[:@6.4]
    output io_sum : UInt<1> @[:@6.4]
    output io_cout : UInt<1> @[:@6.4]
  
    node a_xor_b = xor(io_a, io_b) @[AdderFull.scala 15:22:@8.4]
    node _T_15 = xor(a_xor_b, io_cin) @[AdderFull.scala 16:21:@9.4]
    node a_and_b = and(io_a, io_b) @[AdderFull.scala 18:22:@11.4]
    node b_and_cin = and(io_b, io_cin) @[AdderFull.scala 19:24:@12.4]
    node a_and_cin = and(io_a, io_cin) @[AdderFull.scala 20:24:@13.4]
    node _T_16 = or(a_and_b, b_and_cin) @[AdderFull.scala 21:22:@14.4]
    node _T_17 = or(_T_16, a_and_cin) @[AdderFull.scala 21:34:@15.4]
    io_sum <= _T_15
    io_cout <= _T_17

  module Adder : @[:@483.2]
    input clock : Clock @[:@484.4]
    input reset : UInt<1> @[:@485.4]
    input io_A : UInt<32> @[:@486.4]
    input io_B : UInt<32> @[:@486.4]
    input io_Cin : UInt<1> @[:@486.4]
    output io_C : UInt<32> @[:@486.4]
    output io_Cout : UInt<1> @[:@486.4]
  
    inst FullAdder of FullAdder @[Adder.scala 17:36:@488.4]
    inst FullAdder_1 of FullAdder @[Adder.scala 17:36:@491.4]
    inst FullAdder_2 of FullAdder @[Adder.scala 17:36:@494.4]
    inst FullAdder_3 of FullAdder @[Adder.scala 17:36:@497.4]
    inst FullAdder_4 of FullAdder @[Adder.scala 17:36:@500.4]
    inst FullAdder_5 of FullAdder @[Adder.scala 17:36:@503.4]
    inst FullAdder_6 of FullAdder @[Adder.scala 17:36:@506.4]
    inst FullAdder_7 of FullAdder @[Adder.scala 17:36:@509.4]
    inst FullAdder_8 of FullAdder @[Adder.scala 17:36:@512.4]
    inst FullAdder_9 of FullAdder @[Adder.scala 17:36:@515.4]
    inst FullAdder_10 of FullAdder @[Adder.scala 17:36:@518.4]
    inst FullAdder_11 of FullAdder @[Adder.scala 17:36:@521.4]
    inst FullAdder_12 of FullAdder @[Adder.scala 17:36:@524.4]
    inst FullAdder_13 of FullAdder @[Adder.scala 17:36:@527.4]
    inst FullAdder_14 of FullAdder @[Adder.scala 17:36:@530.4]
    inst FullAdder_15 of FullAdder @[Adder.scala 17:36:@533.4]
    inst FullAdder_16 of FullAdder @[Adder.scala 17:36:@536.4]
    inst FullAdder_17 of FullAdder @[Adder.scala 17:36:@539.4]
    inst FullAdder_18 of FullAdder @[Adder.scala 17:36:@542.4]
    inst FullAdder_19 of FullAdder @[Adder.scala 17:36:@545.4]
    inst FullAdder_20 of FullAdder @[Adder.scala 17:36:@548.4]
    inst FullAdder_21 of FullAdder @[Adder.scala 17:36:@551.4]
    inst FullAdder_22 of FullAdder @[Adder.scala 17:36:@554.4]
    inst FullAdder_23 of FullAdder @[Adder.scala 17:36:@557.4]
    inst FullAdder_24 of FullAdder @[Adder.scala 17:36:@560.4]
    inst FullAdder_25 of FullAdder @[Adder.scala 17:36:@563.4]
    inst FullAdder_26 of FullAdder @[Adder.scala 17:36:@566.4]
    inst FullAdder_27 of FullAdder @[Adder.scala 17:36:@569.4]
    inst FullAdder_28 of FullAdder @[Adder.scala 17:36:@572.4]
    inst FullAdder_29 of FullAdder @[Adder.scala 17:36:@575.4]
    inst FullAdder_30 of FullAdder @[Adder.scala 17:36:@578.4]
    inst FullAdder_31 of FullAdder @[Adder.scala 17:36:@581.4]
    reg temp1 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), temp1) @[Adder.scala 20:22:@586.4]
    reg temp2 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), temp2) @[Adder.scala 21:22:@587.4]
    node _T_94 = bits(io_A, 0, 0) @[Adder.scala 30:21:@591.4]
    node _T_95 = bits(io_B, 0, 0) @[Adder.scala 31:21:@593.4]
    node _T_96 = bits(FullAdder.io_sum, 0, 0) @[Adder.scala 34:32:@597.4]
    node _T_97 = bits(io_A, 1, 1) @[Adder.scala 30:21:@599.4]
    node _T_98 = bits(io_B, 1, 1) @[Adder.scala 31:21:@601.4]
    node _T_99 = bits(FullAdder_1.io_sum, 0, 0) @[Adder.scala 34:32:@605.4]
    node _T_100 = bits(io_A, 2, 2) @[Adder.scala 30:21:@607.4]
    node _T_101 = bits(io_B, 2, 2) @[Adder.scala 31:21:@609.4]
    node _T_102 = bits(FullAdder_2.io_sum, 0, 0) @[Adder.scala 34:32:@613.4]
    node _T_103 = bits(io_A, 3, 3) @[Adder.scala 30:21:@615.4]
    node _T_104 = bits(io_B, 3, 3) @[Adder.scala 31:21:@617.4]
    node _T_105 = bits(FullAdder_3.io_sum, 0, 0) @[Adder.scala 34:32:@621.4]
    node _T_106 = bits(io_A, 4, 4) @[Adder.scala 30:21:@623.4]
    node _T_107 = bits(io_B, 4, 4) @[Adder.scala 31:21:@625.4]
    node _T_108 = bits(FullAdder_4.io_sum, 0, 0) @[Adder.scala 34:32:@629.4]
    node _T_109 = bits(io_A, 5, 5) @[Adder.scala 30:21:@631.4]
    node _T_110 = bits(io_B, 5, 5) @[Adder.scala 31:21:@633.4]
    node _T_111 = bits(FullAdder_5.io_sum, 0, 0) @[Adder.scala 34:32:@637.4]
    node _T_112 = bits(io_A, 6, 6) @[Adder.scala 30:21:@639.4]
    node _T_113 = bits(io_B, 6, 6) @[Adder.scala 31:21:@641.4]
    node _T_114 = bits(FullAdder_6.io_sum, 0, 0) @[Adder.scala 34:32:@645.4]
    node _T_115 = bits(io_A, 7, 7) @[Adder.scala 30:21:@647.4]
    node _T_116 = bits(io_B, 7, 7) @[Adder.scala 31:21:@649.4]
    node _T_117 = bits(FullAdder_7.io_sum, 0, 0) @[Adder.scala 34:32:@653.4]
    node _T_118 = bits(io_A, 8, 8) @[Adder.scala 30:21:@655.4]
    node _T_119 = bits(io_B, 8, 8) @[Adder.scala 31:21:@657.4]
    node _T_120 = bits(FullAdder_8.io_sum, 0, 0) @[Adder.scala 34:32:@661.4]
    node _T_121 = bits(io_A, 9, 9) @[Adder.scala 30:21:@663.4]
    node _T_122 = bits(io_B, 9, 9) @[Adder.scala 31:21:@665.4]
    node _T_123 = bits(FullAdder_9.io_sum, 0, 0) @[Adder.scala 34:32:@669.4]
    node _T_124 = bits(io_A, 10, 10) @[Adder.scala 30:21:@671.4]
    node _T_125 = bits(io_B, 10, 10) @[Adder.scala 31:21:@673.4]
    node _T_126 = bits(FullAdder_10.io_sum, 0, 0) @[Adder.scala 34:32:@677.4]
    node _T_127 = bits(io_A, 11, 11) @[Adder.scala 30:21:@679.4]
    node _T_128 = bits(io_B, 11, 11) @[Adder.scala 31:21:@681.4]
    node _T_129 = bits(FullAdder_11.io_sum, 0, 0) @[Adder.scala 34:32:@685.4]
    node _T_130 = bits(io_A, 12, 12) @[Adder.scala 30:21:@687.4]
    node _T_131 = bits(io_B, 12, 12) @[Adder.scala 31:21:@689.4]
    node _T_132 = bits(FullAdder_12.io_sum, 0, 0) @[Adder.scala 34:32:@693.4]
    node _T_133 = bits(io_A, 13, 13) @[Adder.scala 30:21:@695.4]
    node _T_134 = bits(io_B, 13, 13) @[Adder.scala 31:21:@697.4]
    node _T_135 = bits(FullAdder_13.io_sum, 0, 0) @[Adder.scala 34:32:@701.4]
    node _T_136 = bits(io_A, 14, 14) @[Adder.scala 30:21:@703.4]
    node _T_137 = bits(io_B, 14, 14) @[Adder.scala 31:21:@705.4]
    node _T_138 = bits(FullAdder_14.io_sum, 0, 0) @[Adder.scala 34:32:@709.4]
    node _T_139 = bits(io_A, 15, 15) @[Adder.scala 30:21:@711.4]
    node _T_140 = bits(io_B, 15, 15) @[Adder.scala 31:21:@713.4]
    node _T_141 = bits(FullAdder_15.io_sum, 0, 0) @[Adder.scala 34:32:@717.4]
    node _T_142 = bits(io_A, 16, 16) @[Adder.scala 30:21:@719.4]
    node _T_143 = bits(io_B, 16, 16) @[Adder.scala 31:21:@721.4]
    node _T_144 = bits(FullAdder_16.io_sum, 0, 0) @[Adder.scala 34:32:@725.4]
    node _T_145 = bits(io_A, 17, 17) @[Adder.scala 30:21:@727.4]
    node _T_146 = bits(io_B, 17, 17) @[Adder.scala 31:21:@729.4]
    node _T_147 = bits(FullAdder_17.io_sum, 0, 0) @[Adder.scala 34:32:@733.4]
    node _T_148 = bits(io_A, 18, 18) @[Adder.scala 30:21:@735.4]
    node _T_149 = bits(io_B, 18, 18) @[Adder.scala 31:21:@737.4]
    node _T_150 = bits(FullAdder_18.io_sum, 0, 0) @[Adder.scala 34:32:@741.4]
    node _T_151 = bits(io_A, 19, 19) @[Adder.scala 30:21:@743.4]
    node _T_152 = bits(io_B, 19, 19) @[Adder.scala 31:21:@745.4]
    node _T_153 = bits(FullAdder_19.io_sum, 0, 0) @[Adder.scala 34:32:@749.4]
    node _T_154 = bits(io_A, 20, 20) @[Adder.scala 30:21:@751.4]
    node _T_155 = bits(io_B, 20, 20) @[Adder.scala 31:21:@753.4]
    node _T_156 = bits(FullAdder_20.io_sum, 0, 0) @[Adder.scala 34:32:@757.4]
    node _T_157 = bits(io_A, 21, 21) @[Adder.scala 30:21:@759.4]
    node _T_158 = bits(io_B, 21, 21) @[Adder.scala 31:21:@761.4]
    node _T_159 = bits(FullAdder_21.io_sum, 0, 0) @[Adder.scala 34:32:@765.4]
    node _T_160 = bits(io_A, 22, 22) @[Adder.scala 30:21:@767.4]
    node _T_161 = bits(io_B, 22, 22) @[Adder.scala 31:21:@769.4]
    node _T_162 = bits(FullAdder_22.io_sum, 0, 0) @[Adder.scala 34:32:@773.4]
    node _T_163 = bits(io_A, 23, 23) @[Adder.scala 30:21:@775.4]
    node _T_164 = bits(io_B, 23, 23) @[Adder.scala 31:21:@777.4]
    node _T_165 = bits(FullAdder_23.io_sum, 0, 0) @[Adder.scala 34:32:@781.4]
    node _T_166 = bits(io_A, 24, 24) @[Adder.scala 30:21:@783.4]
    node _T_167 = bits(io_B, 24, 24) @[Adder.scala 31:21:@785.4]
    node _T_168 = bits(FullAdder_24.io_sum, 0, 0) @[Adder.scala 34:32:@789.4]
    node _T_169 = bits(io_A, 25, 25) @[Adder.scala 30:21:@791.4]
    node _T_170 = bits(io_B, 25, 25) @[Adder.scala 31:21:@793.4]
    node _T_171 = bits(FullAdder_25.io_sum, 0, 0) @[Adder.scala 34:32:@797.4]
    node _T_172 = bits(io_A, 26, 26) @[Adder.scala 30:21:@799.4]
    node _T_173 = bits(io_B, 26, 26) @[Adder.scala 31:21:@801.4]
    node _T_174 = bits(FullAdder_26.io_sum, 0, 0) @[Adder.scala 34:32:@805.4]
    node _T_175 = bits(io_A, 27, 27) @[Adder.scala 30:21:@807.4]
    node _T_176 = bits(io_B, 27, 27) @[Adder.scala 31:21:@809.4]
    node _T_177 = bits(FullAdder_27.io_sum, 0, 0) @[Adder.scala 34:32:@813.4]
    node _T_178 = bits(io_A, 28, 28) @[Adder.scala 30:21:@815.4]
    node _T_179 = bits(io_B, 28, 28) @[Adder.scala 31:21:@817.4]
    node _T_180 = bits(FullAdder_28.io_sum, 0, 0) @[Adder.scala 34:32:@821.4]
    node _T_181 = bits(io_A, 29, 29) @[Adder.scala 30:21:@823.4]
    node _T_182 = bits(io_B, 29, 29) @[Adder.scala 31:21:@825.4]
    node _T_183 = bits(FullAdder_29.io_sum, 0, 0) @[Adder.scala 34:32:@829.4]
    node _T_184 = bits(io_A, 30, 30) @[Adder.scala 30:21:@831.4]
    node _T_185 = bits(io_B, 30, 30) @[Adder.scala 31:21:@833.4]
    node _T_186 = bits(FullAdder_30.io_sum, 0, 0) @[Adder.scala 34:32:@837.4]
    node _T_187 = bits(io_A, 31, 31) @[Adder.scala 30:21:@839.4]
    node _T_188 = bits(io_B, 31, 31) @[Adder.scala 31:21:@841.4]
    node _T_189 = bits(FullAdder_31.io_sum, 0, 0) @[Adder.scala 34:32:@845.4]
    node sum_1 = _T_99 @[Adder.scala 19:19:@585.4]
    node sum_0 = _T_96 @[Adder.scala 19:19:@585.4]
    node _T_190 = cat(sum_1, sum_0) @[Adder.scala 36:14:@847.4]
    node sum_3 = _T_105 @[Adder.scala 19:19:@585.4]
    node sum_2 = _T_102 @[Adder.scala 19:19:@585.4]
    node _T_191 = cat(sum_3, sum_2) @[Adder.scala 36:14:@848.4]
    node _T_192 = cat(_T_191, _T_190) @[Adder.scala 36:14:@849.4]
    node sum_5 = _T_111 @[Adder.scala 19:19:@585.4]
    node sum_4 = _T_108 @[Adder.scala 19:19:@585.4]
    node _T_193 = cat(sum_5, sum_4) @[Adder.scala 36:14:@850.4]
    node sum_7 = _T_117 @[Adder.scala 19:19:@585.4]
    node sum_6 = _T_114 @[Adder.scala 19:19:@585.4]
    node _T_194 = cat(sum_7, sum_6) @[Adder.scala 36:14:@851.4]
    node _T_195 = cat(_T_194, _T_193) @[Adder.scala 36:14:@852.4]
    node _T_196 = cat(_T_195, _T_192) @[Adder.scala 36:14:@853.4]
    node sum_9 = _T_123 @[Adder.scala 19:19:@585.4]
    node sum_8 = _T_120 @[Adder.scala 19:19:@585.4]
    node _T_197 = cat(sum_9, sum_8) @[Adder.scala 36:14:@854.4]
    node sum_11 = _T_129 @[Adder.scala 19:19:@585.4]
    node sum_10 = _T_126 @[Adder.scala 19:19:@585.4]
    node _T_198 = cat(sum_11, sum_10) @[Adder.scala 36:14:@855.4]
    node _T_199 = cat(_T_198, _T_197) @[Adder.scala 36:14:@856.4]
    node sum_13 = _T_135 @[Adder.scala 19:19:@585.4]
    node sum_12 = _T_132 @[Adder.scala 19:19:@585.4]
    node _T_200 = cat(sum_13, sum_12) @[Adder.scala 36:14:@857.4]
    node sum_15 = _T_141 @[Adder.scala 19:19:@585.4]
    node sum_14 = _T_138 @[Adder.scala 19:19:@585.4]
    node _T_201 = cat(sum_15, sum_14) @[Adder.scala 36:14:@858.4]
    node _T_202 = cat(_T_201, _T_200) @[Adder.scala 36:14:@859.4]
    node _T_203 = cat(_T_202, _T_199) @[Adder.scala 36:14:@860.4]
    node _T_204 = cat(_T_203, _T_196) @[Adder.scala 36:14:@861.4]
    node sum_17 = _T_147 @[Adder.scala 19:19:@585.4]
    node sum_16 = _T_144 @[Adder.scala 19:19:@585.4]
    node _T_205 = cat(sum_17, sum_16) @[Adder.scala 36:14:@862.4]
    node sum_19 = _T_153 @[Adder.scala 19:19:@585.4]
    node sum_18 = _T_150 @[Adder.scala 19:19:@585.4]
    node _T_206 = cat(sum_19, sum_18) @[Adder.scala 36:14:@863.4]
    node _T_207 = cat(_T_206, _T_205) @[Adder.scala 36:14:@864.4]
    node sum_21 = _T_159 @[Adder.scala 19:19:@585.4]
    node sum_20 = _T_156 @[Adder.scala 19:19:@585.4]
    node _T_208 = cat(sum_21, sum_20) @[Adder.scala 36:14:@865.4]
    node sum_23 = _T_165 @[Adder.scala 19:19:@585.4]
    node sum_22 = _T_162 @[Adder.scala 19:19:@585.4]
    node _T_209 = cat(sum_23, sum_22) @[Adder.scala 36:14:@866.4]
    node _T_210 = cat(_T_209, _T_208) @[Adder.scala 36:14:@867.4]
    node _T_211 = cat(_T_210, _T_207) @[Adder.scala 36:14:@868.4]
    node sum_25 = _T_171 @[Adder.scala 19:19:@585.4]
    node sum_24 = _T_168 @[Adder.scala 19:19:@585.4]
    node _T_212 = cat(sum_25, sum_24) @[Adder.scala 36:14:@869.4]
    node sum_27 = _T_177 @[Adder.scala 19:19:@585.4]
    node sum_26 = _T_174 @[Adder.scala 19:19:@585.4]
    node _T_213 = cat(sum_27, sum_26) @[Adder.scala 36:14:@870.4]
    node _T_214 = cat(_T_213, _T_212) @[Adder.scala 36:14:@871.4]
    node sum_29 = _T_183 @[Adder.scala 19:19:@585.4]
    node sum_28 = _T_180 @[Adder.scala 19:19:@585.4]
    node _T_215 = cat(sum_29, sum_28) @[Adder.scala 36:14:@872.4]
    node sum_31 = _T_189 @[Adder.scala 19:19:@585.4]
    node sum_30 = _T_186 @[Adder.scala 19:19:@585.4]
    node _T_216 = cat(sum_31, sum_30) @[Adder.scala 36:14:@873.4]
    node _T_217 = cat(_T_216, _T_215) @[Adder.scala 36:14:@874.4]
    node _T_218 = cat(_T_217, _T_214) @[Adder.scala 36:14:@875.4]
    node _T_219 = cat(_T_218, _T_211) @[Adder.scala 36:14:@876.4]
    node _T_220 = cat(_T_219, _T_204) @[Adder.scala 36:14:@877.4]
    node carry_0 = io_Cin @[Adder.scala 18:19:@584.4]
    node carry_1 = FullAdder.io_cout @[Adder.scala 18:19:@584.4]
    node carry_2 = FullAdder_1.io_cout @[Adder.scala 18:19:@584.4]
    node carry_3 = FullAdder_2.io_cout @[Adder.scala 18:19:@584.4]
    node carry_4 = FullAdder_3.io_cout @[Adder.scala 18:19:@584.4]
    node carry_5 = FullAdder_4.io_cout @[Adder.scala 18:19:@584.4]
    node carry_6 = FullAdder_5.io_cout @[Adder.scala 18:19:@584.4]
    node carry_7 = FullAdder_6.io_cout @[Adder.scala 18:19:@584.4]
    node carry_8 = FullAdder_7.io_cout @[Adder.scala 18:19:@584.4]
    node carry_9 = FullAdder_8.io_cout @[Adder.scala 18:19:@584.4]
    node carry_10 = FullAdder_9.io_cout @[Adder.scala 18:19:@584.4]
    node carry_11 = FullAdder_10.io_cout @[Adder.scala 18:19:@584.4]
    node carry_12 = FullAdder_11.io_cout @[Adder.scala 18:19:@584.4]
    node carry_13 = FullAdder_12.io_cout @[Adder.scala 18:19:@584.4]
    node carry_14 = FullAdder_13.io_cout @[Adder.scala 18:19:@584.4]
    node carry_15 = FullAdder_14.io_cout @[Adder.scala 18:19:@584.4]
    node carry_16 = FullAdder_15.io_cout @[Adder.scala 18:19:@584.4]
    node carry_17 = FullAdder_16.io_cout @[Adder.scala 18:19:@584.4]
    node carry_18 = FullAdder_17.io_cout @[Adder.scala 18:19:@584.4]
    node carry_19 = FullAdder_18.io_cout @[Adder.scala 18:19:@584.4]
    node carry_20 = FullAdder_19.io_cout @[Adder.scala 18:19:@584.4]
    node carry_21 = FullAdder_20.io_cout @[Adder.scala 18:19:@584.4]
    node carry_22 = FullAdder_21.io_cout @[Adder.scala 18:19:@584.4]
    node carry_23 = FullAdder_22.io_cout @[Adder.scala 18:19:@584.4]
    node carry_24 = FullAdder_23.io_cout @[Adder.scala 18:19:@584.4]
    node carry_25 = FullAdder_24.io_cout @[Adder.scala 18:19:@584.4]
    node carry_26 = FullAdder_25.io_cout @[Adder.scala 18:19:@584.4]
    node carry_27 = FullAdder_26.io_cout @[Adder.scala 18:19:@584.4]
    node carry_28 = FullAdder_27.io_cout @[Adder.scala 18:19:@584.4]
    node carry_29 = FullAdder_28.io_cout @[Adder.scala 18:19:@584.4]
    node carry_30 = FullAdder_29.io_cout @[Adder.scala 18:19:@584.4]
    node carry_31 = FullAdder_30.io_cout @[Adder.scala 18:19:@584.4]
    node carry_32 = FullAdder_31.io_cout @[Adder.scala 18:19:@584.4]
    io_C <= _T_220
    io_Cout <= carry_32
    FullAdder.clock <= clock
    FullAdder.reset <= reset
    FullAdder.io_a <= _T_94
    FullAdder.io_b <= _T_95
    FullAdder.io_cin <= carry_0
    FullAdder_1.clock <= clock
    FullAdder_1.reset <= reset
    FullAdder_1.io_a <= _T_97
    FullAdder_1.io_b <= _T_98
    FullAdder_1.io_cin <= carry_1
    FullAdder_2.clock <= clock
    FullAdder_2.reset <= reset
    FullAdder_2.io_a <= _T_100
    FullAdder_2.io_b <= _T_101
    FullAdder_2.io_cin <= carry_2
    FullAdder_3.clock <= clock
    FullAdder_3.reset <= reset
    FullAdder_3.io_a <= _T_103
    FullAdder_3.io_b <= _T_104
    FullAdder_3.io_cin <= carry_3
    FullAdder_4.clock <= clock
    FullAdder_4.reset <= reset
    FullAdder_4.io_a <= _T_106
    FullAdder_4.io_b <= _T_107
    FullAdder_4.io_cin <= carry_4
    FullAdder_5.clock <= clock
    FullAdder_5.reset <= reset
    FullAdder_5.io_a <= _T_109
    FullAdder_5.io_b <= _T_110
    FullAdder_5.io_cin <= carry_5
    FullAdder_6.clock <= clock
    FullAdder_6.reset <= reset
    FullAdder_6.io_a <= _T_112
    FullAdder_6.io_b <= _T_113
    FullAdder_6.io_cin <= carry_6
    FullAdder_7.clock <= clock
    FullAdder_7.reset <= reset
    FullAdder_7.io_a <= _T_115
    FullAdder_7.io_b <= _T_116
    FullAdder_7.io_cin <= carry_7
    FullAdder_8.clock <= clock
    FullAdder_8.reset <= reset
    FullAdder_8.io_a <= _T_118
    FullAdder_8.io_b <= _T_119
    FullAdder_8.io_cin <= carry_8
    FullAdder_9.clock <= clock
    FullAdder_9.reset <= reset
    FullAdder_9.io_a <= _T_121
    FullAdder_9.io_b <= _T_122
    FullAdder_9.io_cin <= carry_9
    FullAdder_10.clock <= clock
    FullAdder_10.reset <= reset
    FullAdder_10.io_a <= _T_124
    FullAdder_10.io_b <= _T_125
    FullAdder_10.io_cin <= carry_10
    FullAdder_11.clock <= clock
    FullAdder_11.reset <= reset
    FullAdder_11.io_a <= _T_127
    FullAdder_11.io_b <= _T_128
    FullAdder_11.io_cin <= carry_11
    FullAdder_12.clock <= clock
    FullAdder_12.reset <= reset
    FullAdder_12.io_a <= _T_130
    FullAdder_12.io_b <= _T_131
    FullAdder_12.io_cin <= carry_12
    FullAdder_13.clock <= clock
    FullAdder_13.reset <= reset
    FullAdder_13.io_a <= _T_133
    FullAdder_13.io_b <= _T_134
    FullAdder_13.io_cin <= carry_13
    FullAdder_14.clock <= clock
    FullAdder_14.reset <= reset
    FullAdder_14.io_a <= _T_136
    FullAdder_14.io_b <= _T_137
    FullAdder_14.io_cin <= carry_14
    FullAdder_15.clock <= clock
    FullAdder_15.reset <= reset
    FullAdder_15.io_a <= _T_139
    FullAdder_15.io_b <= _T_140
    FullAdder_15.io_cin <= carry_15
    FullAdder_16.clock <= clock
    FullAdder_16.reset <= reset
    FullAdder_16.io_a <= _T_142
    FullAdder_16.io_b <= _T_143
    FullAdder_16.io_cin <= carry_16
    FullAdder_17.clock <= clock
    FullAdder_17.reset <= reset
    FullAdder_17.io_a <= _T_145
    FullAdder_17.io_b <= _T_146
    FullAdder_17.io_cin <= carry_17
    FullAdder_18.clock <= clock
    FullAdder_18.reset <= reset
    FullAdder_18.io_a <= _T_148
    FullAdder_18.io_b <= _T_149
    FullAdder_18.io_cin <= carry_18
    FullAdder_19.clock <= clock
    FullAdder_19.reset <= reset
    FullAdder_19.io_a <= _T_151
    FullAdder_19.io_b <= _T_152
    FullAdder_19.io_cin <= carry_19
    FullAdder_20.clock <= clock
    FullAdder_20.reset <= reset
    FullAdder_20.io_a <= _T_154
    FullAdder_20.io_b <= _T_155
    FullAdder_20.io_cin <= carry_20
    FullAdder_21.clock <= clock
    FullAdder_21.reset <= reset
    FullAdder_21.io_a <= _T_157
    FullAdder_21.io_b <= _T_158
    FullAdder_21.io_cin <= carry_21
    FullAdder_22.clock <= clock
    FullAdder_22.reset <= reset
    FullAdder_22.io_a <= _T_160
    FullAdder_22.io_b <= _T_161
    FullAdder_22.io_cin <= carry_22
    FullAdder_23.clock <= clock
    FullAdder_23.reset <= reset
    FullAdder_23.io_a <= _T_163
    FullAdder_23.io_b <= _T_164
    FullAdder_23.io_cin <= carry_23
    FullAdder_24.clock <= clock
    FullAdder_24.reset <= reset
    FullAdder_24.io_a <= _T_166
    FullAdder_24.io_b <= _T_167
    FullAdder_24.io_cin <= carry_24
    FullAdder_25.clock <= clock
    FullAdder_25.reset <= reset
    FullAdder_25.io_a <= _T_169
    FullAdder_25.io_b <= _T_170
    FullAdder_25.io_cin <= carry_25
    FullAdder_26.clock <= clock
    FullAdder_26.reset <= reset
    FullAdder_26.io_a <= _T_172
    FullAdder_26.io_b <= _T_173
    FullAdder_26.io_cin <= carry_26
    FullAdder_27.clock <= clock
    FullAdder_27.reset <= reset
    FullAdder_27.io_a <= _T_175
    FullAdder_27.io_b <= _T_176
    FullAdder_27.io_cin <= carry_27
    FullAdder_28.clock <= clock
    FullAdder_28.reset <= reset
    FullAdder_28.io_a <= _T_178
    FullAdder_28.io_b <= _T_179
    FullAdder_28.io_cin <= carry_28
    FullAdder_29.clock <= clock
    FullAdder_29.reset <= reset
    FullAdder_29.io_a <= _T_181
    FullAdder_29.io_b <= _T_182
    FullAdder_29.io_cin <= carry_29
    FullAdder_30.clock <= clock
    FullAdder_30.reset <= reset
    FullAdder_30.io_a <= _T_184
    FullAdder_30.io_b <= _T_185
    FullAdder_30.io_cin <= carry_30
    FullAdder_31.clock <= clock
    FullAdder_31.reset <= reset
    FullAdder_31.io_a <= _T_187
    FullAdder_31.io_b <= _T_188
    FullAdder_31.io_cin <= carry_31
    temp1 <= mux(reset, UInt<32>("h0"), io_A)
    temp2 <= mux(reset, UInt<32>("h0"), io_B)

  module Booth : @[:@881.2]
    input clock : Clock @[:@882.4]
    input reset : UInt<1> @[:@883.4]
    input io_a : UInt<3> @[:@884.4]
    input io_b : UInt<16> @[:@884.4]
    output io_out : UInt<18> @[:@884.4]
    output io_cout : UInt<1> @[:@884.4]
  
    node _T_14 = eq(io_a, UInt<1>("h0")) @[Booth.scala 15:15:@886.4]
    node _T_16 = eq(io_a, UInt<3>("h7")) @[Booth.scala 15:31:@887.4]
    node _T_17 = or(_T_14, _T_16) @[Booth.scala 15:23:@888.4]
    node _T_20 = cat(UInt<1>("h1"), UInt<17>("h0")) @[Cat.scala 30:58:@890.6]
    node _T_23 = eq(io_a, UInt<1>("h1")) @[Booth.scala 18:22:@895.6]
    node _T_25 = eq(io_a, UInt<2>("h2")) @[Booth.scala 18:38:@896.6]
    node _T_26 = or(_T_23, _T_25) @[Booth.scala 18:30:@897.6]
    node _T_29 = cat(UInt<1>("h1"), UInt<1>("h0")) @[Cat.scala 30:58:@899.8]
    node _T_30 = cat(_T_29, io_b) @[Cat.scala 30:58:@900.8]
    node _T_33 = eq(io_a, UInt<3>("h5")) @[Booth.scala 21:22:@905.8]
    node _T_35 = eq(io_a, UInt<3>("h6")) @[Booth.scala 21:38:@906.8]
    node _T_36 = or(_T_33, _T_35) @[Booth.scala 21:30:@907.8]
    node _T_39 = not(io_b) @[Booth.scala 22:43:@909.10]
    node _T_40 = cat(UInt<1>("h0"), UInt<1>("h1")) @[Cat.scala 30:58:@910.10]
    node _T_41 = cat(_T_40, _T_39) @[Cat.scala 30:58:@911.10]
    node _T_44 = eq(io_a, UInt<2>("h3")) @[Booth.scala 24:22:@916.10]
    node _T_47 = cat(UInt<1>("h1"), io_b) @[Cat.scala 30:58:@918.12]
    node _T_48 = cat(_T_47, UInt<1>("h0")) @[Cat.scala 30:58:@919.12]
    node _T_51 = not(io_b) @[Booth.scala 28:33:@924.12]
    node _T_53 = cat(UInt<1>("h0"), _T_51) @[Cat.scala 30:58:@925.12]
    node _T_54 = cat(_T_53, UInt<1>("h1")) @[Cat.scala 30:58:@926.12]
    node _GEN_0 = mux(_T_44, _T_48, _T_54) @[Booth.scala 24:31:@917.10]
    node _GEN_1 = mux(_T_44, UInt<1>("h0"), UInt<1>("h1")) @[Booth.scala 24:31:@917.10]
    node _GEN_2 = mux(_T_36, _T_41, _GEN_0) @[Booth.scala 21:47:@908.8]
    node _GEN_3 = mux(_T_36, UInt<1>("h1"), _GEN_1) @[Booth.scala 21:47:@908.8]
    node _GEN_4 = mux(_T_26, _T_30, _GEN_2) @[Booth.scala 18:47:@898.6]
    node _GEN_5 = mux(_T_26, UInt<1>("h0"), _GEN_3) @[Booth.scala 18:47:@898.6]
    node _GEN_6 = mux(_T_17, _T_20, _GEN_4) @[Booth.scala 15:40:@889.4]
    node _GEN_7 = mux(_T_17, UInt<1>("h0"), _GEN_5) @[Booth.scala 15:40:@889.4]
    io_out <= _GEN_6
    io_cout <= _GEN_7

  module ALU : @[:@1331.2]
    input clock : Clock @[:@1332.4]
    input reset : UInt<1> @[:@1333.4]
    input io_a : UInt<32> @[:@1334.4]
    input io_b : UInt<32> @[:@1334.4]
    input io_opcode : UInt<6> @[:@1334.4]
    input io_start : UInt<1> @[:@1334.4]
    output io_c : UInt<32> @[:@1334.4]
    output io_zero : UInt<1> @[:@1334.4]
    output io_product : UInt<64> @[:@1334.4]
    output io_quotient : UInt<32> @[:@1334.4]
    output io_remaind : UInt<32> @[:@1334.4]
    output io_complete : UInt<1> @[:@1334.4]
  
    inst adderOnly of Adder @[ALU.scala 31:27:@1336.4]
    inst Booth of Booth @[ALU.scala 71:29:@1373.12]
    inst Booth_1 of Booth @[ALU.scala 71:29:@1380.12]
    inst Booth_2 of Booth @[ALU.scala 71:29:@1386.12]
    inst Booth_3 of Booth @[ALU.scala 71:29:@1392.12]
    inst Booth_4 of Booth @[ALU.scala 71:29:@1398.12]
    inst Booth_5 of Booth @[ALU.scala 71:29:@1404.12]
    inst Booth_6 of Booth @[ALU.scala 71:29:@1410.12]
    inst Booth_7 of Booth @[ALU.scala 71:29:@1416.12]
    inst Booth_8 of Booth @[ALU.scala 71:29:@1422.12]
    node _T_35 = eq(io_opcode, UInt<6>("h20")) @[ALU.scala 51:22:@1348.4]
    node _T_37 = eq(io_opcode, UInt<6>("h22")) @[ALU.scala 57:27:@1355.6]
    node _T_38 = not(io_b) @[ALU.scala 59:26:@1358.8]
    node _T_40 = add(_T_38, UInt<1>("h1")) @[ALU.scala 59:32:@1359.8]
    node _T_41 = tail(_T_40, 1) @[ALU.scala 59:32:@1360.8]
    node _T_43 = eq(io_opcode, UInt<5>("h18")) @[ALU.scala 62:27:@1365.8]
    node _T_44 = mul(io_a, io_b) @[ALU.scala 64:20:@1367.10]
    node _T_46 = eq(io_opcode, UInt<5>("h19")) @[ALU.scala 66:27:@1371.10]
    node _T_47 = bits(io_a, 1, 0) @[ALU.scala 73:35:@1376.12]
    node _T_49 = cat(_T_47, UInt<1>("h0")) @[Cat.scala 30:58:@1377.12]
    node _T_50 = bits(io_a, 3, 1) @[ALU.scala 75:31:@1383.12]
    node _T_51 = bits(io_a, 5, 3) @[ALU.scala 75:31:@1389.12]
    node _T_52 = bits(io_a, 7, 5) @[ALU.scala 75:31:@1395.12]
    node _T_53 = bits(io_a, 9, 7) @[ALU.scala 75:31:@1401.12]
    node _T_54 = bits(io_a, 11, 9) @[ALU.scala 75:31:@1407.12]
    node _T_55 = bits(io_a, 13, 11) @[ALU.scala 75:31:@1413.12]
    node _T_56 = bits(io_a, 15, 13) @[ALU.scala 75:31:@1419.12]
    node _T_58 = bits(io_a, 15, 15) @[ALU.scala 77:63:@1425.12]
    node _T_59 = cat(UInt<2>("h0"), _T_58) @[Cat.scala 30:58:@1426.12]
    node _T_69 = cat(UInt<1>("h0"), Booth.io_cout) @[Cat.scala 30:58:@1429.12]
    node _T_70 = cat(UInt<1>("h0"), Booth_1.io_cout) @[Cat.scala 30:58:@1430.12]
    node _T_71 = cat(_T_70, _T_69) @[Cat.scala 30:58:@1431.12]
    node _T_72 = cat(UInt<1>("h0"), Booth_2.io_cout) @[Cat.scala 30:58:@1432.12]
    node _T_73 = cat(UInt<1>("h0"), Booth_3.io_cout) @[Cat.scala 30:58:@1433.12]
    node _T_74 = cat(_T_73, _T_72) @[Cat.scala 30:58:@1434.12]
    node _T_75 = cat(_T_74, _T_71) @[Cat.scala 30:58:@1435.12]
    node _T_76 = cat(UInt<1>("h0"), Booth_4.io_cout) @[Cat.scala 30:58:@1436.12]
    node _T_77 = cat(UInt<1>("h0"), Booth_5.io_cout) @[Cat.scala 30:58:@1437.12]
    node _T_78 = cat(_T_77, _T_76) @[Cat.scala 30:58:@1438.12]
    node _T_79 = cat(UInt<1>("h0"), Booth_6.io_cout) @[Cat.scala 30:58:@1439.12]
    node _T_80 = cat(Booth_8.io_cout, UInt<1>("h0")) @[Cat.scala 30:58:@1440.12]
    node _T_81 = cat(_T_80, Booth_7.io_cout) @[Cat.scala 30:58:@1441.12]
    node _T_82 = cat(_T_81, _T_79) @[Cat.scala 30:58:@1442.12]
    node _T_83 = cat(_T_82, _T_78) @[Cat.scala 30:58:@1443.12]
    node _T_84 = cat(_T_83, _T_75) @[Cat.scala 30:58:@1444.12]
    node _T_87 = cat(UInt<1>("h1"), UInt<1>("h0")) @[Cat.scala 30:58:@1445.12]
    node _T_90 = cat(UInt<1>("h1"), UInt<1>("h0")) @[Cat.scala 30:58:@1446.12]
    node _T_93 = cat(UInt<1>("h1"), UInt<1>("h0")) @[Cat.scala 30:58:@1447.12]
    node _T_96 = cat(UInt<1>("h1"), UInt<1>("h0")) @[Cat.scala 30:58:@1448.12]
    node _T_99 = cat(UInt<1>("h1"), UInt<1>("h0")) @[Cat.scala 30:58:@1449.12]
    node _T_102 = cat(UInt<1>("h1"), UInt<1>("h0")) @[Cat.scala 30:58:@1450.12]
    node _T_105 = cat(UInt<1>("h1"), UInt<1>("h0")) @[Cat.scala 30:58:@1451.12]
    node _T_108 = cat(UInt<1>("h1"), UInt<1>("h1")) @[Cat.scala 30:58:@1452.12]
    node _T_109 = cat(_T_105, _T_108) @[Cat.scala 30:58:@1453.12]
    node _T_110 = cat(_T_99, _T_102) @[Cat.scala 30:58:@1454.12]
    node _T_111 = cat(_T_110, _T_109) @[Cat.scala 30:58:@1455.12]
    node _T_112 = cat(_T_93, _T_96) @[Cat.scala 30:58:@1456.12]
    node _T_113 = cat(_T_87, _T_90) @[Cat.scala 30:58:@1457.12]
    node _T_114 = cat(_T_113, _T_112) @[Cat.scala 30:58:@1458.12]
    node _T_115 = cat(_T_114, _T_111) @[Cat.scala 30:58:@1459.12]
    node _T_116 = cat(_T_115, _T_84) @[Cat.scala 30:58:@1460.12]
    node _T_117 = shl(UInt<32>("h0"), 2) @[ALU.scala 90:22:@1461.12]
    node _T_118 = add(_T_117, Booth_8.io_out) @[ALU.scala 90:28:@1462.12]
    node _T_119 = tail(_T_118, 1) @[ALU.scala 90:28:@1463.12]
    node _T_120 = shl(_T_119, 2) @[ALU.scala 90:22:@1464.12]
    node _T_121 = add(_T_120, Booth_7.io_out) @[ALU.scala 90:28:@1465.12]
    node _T_122 = tail(_T_121, 1) @[ALU.scala 90:28:@1466.12]
    node _T_123 = shl(_T_122, 2) @[ALU.scala 90:22:@1467.12]
    node _T_124 = add(_T_123, Booth_6.io_out) @[ALU.scala 90:28:@1468.12]
    node _T_125 = tail(_T_124, 1) @[ALU.scala 90:28:@1469.12]
    node _T_126 = shl(_T_125, 2) @[ALU.scala 90:22:@1470.12]
    node _T_127 = add(_T_126, Booth_5.io_out) @[ALU.scala 90:28:@1471.12]
    node _T_128 = tail(_T_127, 1) @[ALU.scala 90:28:@1472.12]
    node _T_129 = shl(_T_128, 2) @[ALU.scala 90:22:@1473.12]
    node _T_130 = add(_T_129, Booth_4.io_out) @[ALU.scala 90:28:@1474.12]
    node _T_131 = tail(_T_130, 1) @[ALU.scala 90:28:@1475.12]
    node _T_132 = shl(_T_131, 2) @[ALU.scala 90:22:@1476.12]
    node _T_133 = add(_T_132, Booth_3.io_out) @[ALU.scala 90:28:@1477.12]
    node _T_134 = tail(_T_133, 1) @[ALU.scala 90:28:@1478.12]
    node _T_135 = shl(_T_134, 2) @[ALU.scala 90:22:@1479.12]
    node _T_136 = add(_T_135, Booth_2.io_out) @[ALU.scala 90:28:@1480.12]
    node _T_137 = tail(_T_136, 1) @[ALU.scala 90:28:@1481.12]
    node _T_138 = shl(_T_137, 2) @[ALU.scala 90:22:@1482.12]
    node _T_139 = add(_T_138, Booth_1.io_out) @[ALU.scala 90:28:@1483.12]
    node _T_140 = tail(_T_139, 1) @[ALU.scala 90:28:@1484.12]
    node _T_141 = shl(_T_140, 2) @[ALU.scala 90:22:@1485.12]
    node _T_142 = add(_T_141, Booth.io_out) @[ALU.scala 90:28:@1486.12]
    node _T_143 = tail(_T_142, 1) @[ALU.scala 90:28:@1487.12]
    node _T_144 = add(_T_143, _T_116) @[ALU.scala 92:17:@1488.12]
    node _T_145 = tail(_T_144, 1) @[ALU.scala 92:17:@1489.12]
    node _T_146 = bits(_T_145, 31, 0) @[ALU.scala 93:18:@1490.12]
    node _T_148 = eq(io_opcode, UInt<5>("h1a")) @[ALU.scala 96:27:@1494.12]
    node _T_149 = div(io_a, io_b) @[ALU.scala 98:20:@1496.14]
    node _T_151 = eq(io_opcode, UInt<5>("h1b")) @[ALU.scala 101:27:@1500.14]
    node _T_152 = div(io_a, io_b) @[ALU.scala 102:18:@1502.16]
    node _T_154 = eq(io_opcode, UInt<6>("h24")) @[ALU.scala 175:27:@1506.16]
    node _T_155 = and(io_a, io_b) @[ALU.scala 177:20:@1508.18]
    node _T_157 = eq(io_opcode, UInt<6>("h25")) @[ALU.scala 180:27:@1512.18]
    node _T_158 = or(io_a, io_b) @[ALU.scala 182:20:@1514.20]
    node _T_160 = eq(io_opcode, UInt<6>("h26")) @[ALU.scala 185:27:@1518.20]
    node _T_161 = xor(io_a, io_b) @[ALU.scala 187:20:@1520.22]
    node _T_163 = eq(io_opcode, UInt<6>("h27")) @[ALU.scala 190:27:@1524.22]
    node _T_164 = or(io_a, io_b) @[ALU.scala 192:22:@1526.24]
    node _T_165 = not(_T_164) @[ALU.scala 192:15:@1527.24]
    node _GEN_0 = mux(_T_163, _T_165, UInt<1>("h0")) @[ALU.scala 190:37:@1525.22]
    node _GEN_1 = mux(_T_160, _T_161, _GEN_0) @[ALU.scala 185:37:@1519.20]
    node _GEN_2 = mux(_T_157, _T_158, _GEN_1) @[ALU.scala 180:37:@1513.18]
    node _GEN_3 = mux(_T_154, _T_155, _GEN_2) @[ALU.scala 175:37:@1507.16]
    node _GEN_4 = mux(_T_151, _T_152, _GEN_3) @[ALU.scala 101:37:@1501.14]
    node _GEN_5 = mux(_T_148, _T_149, _GEN_4) @[ALU.scala 96:37:@1495.12]
    node _GEN_6 = mux(_T_46, _T_146, _GEN_5) @[ALU.scala 66:37:@1372.10]
    node _GEN_7 = mux(_T_43, _T_44, _GEN_6) @[ALU.scala 62:37:@1366.8]
    node _GEN_8 = mux(_T_37, io_a, UInt<1>("h0")) @[ALU.scala 57:38:@1356.6]
    node _GEN_9 = mux(_T_37, _T_41, UInt<1>("h0")) @[ALU.scala 57:38:@1356.6]
    node _GEN_10 = mux(_T_37, adderOnly.io_C, _GEN_7) @[ALU.scala 57:38:@1356.6]
    node _GEN_11 = mux(_T_35, io_a, _GEN_8) @[ALU.scala 51:33:@1349.4]
    node _GEN_12 = mux(_T_35, io_b, _GEN_9) @[ALU.scala 51:33:@1349.4]
    node _GEN_13 = mux(_T_35, adderOnly.io_C, _GEN_10) @[ALU.scala 51:33:@1349.4]
    io_c <= bits(_GEN_13, 31, 0)
    io_zero <= UInt<1>("h0")
    io_product <= UInt<1>("h0")
    io_quotient <= UInt<1>("h0")
    io_remaind <= UInt<1>("h0")
    io_complete <= UInt<1>("h0")
    adderOnly.clock <= clock
    adderOnly.reset <= reset
    adderOnly.io_A <= _GEN_11
    adderOnly.io_B <= _GEN_12
    adderOnly.io_Cin <= UInt<1>("h0")
    Booth.clock <= clock
    Booth.reset <= reset
    Booth.io_a <= _T_49
    Booth.io_b <= bits(io_b, 15, 0)
    Booth_1.clock <= clock
    Booth_1.reset <= reset
    Booth_1.io_a <= _T_50
    Booth_1.io_b <= bits(io_b, 15, 0)
    Booth_2.clock <= clock
    Booth_2.reset <= reset
    Booth_2.io_a <= _T_51
    Booth_2.io_b <= bits(io_b, 15, 0)
    Booth_3.clock <= clock
    Booth_3.reset <= reset
    Booth_3.io_a <= _T_52
    Booth_3.io_b <= bits(io_b, 15, 0)
    Booth_4.clock <= clock
    Booth_4.reset <= reset
    Booth_4.io_a <= _T_53
    Booth_4.io_b <= bits(io_b, 15, 0)
    Booth_5.clock <= clock
    Booth_5.reset <= reset
    Booth_5.io_a <= _T_54
    Booth_5.io_b <= bits(io_b, 15, 0)
    Booth_6.clock <= clock
    Booth_6.reset <= reset
    Booth_6.io_a <= _T_55
    Booth_6.io_b <= bits(io_b, 15, 0)
    Booth_7.clock <= clock
    Booth_7.reset <= reset
    Booth_7.io_a <= _T_56
    Booth_7.io_b <= bits(io_b, 15, 0)
    Booth_8.clock <= clock
    Booth_8.reset <= reset
    Booth_8.io_a <= _T_59
    Booth_8.io_b <= bits(io_b, 15, 0)
