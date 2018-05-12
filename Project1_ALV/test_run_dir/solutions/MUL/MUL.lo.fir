circuit MUL : @[:@2.0]
  module Booth : @[:@3.2]
    input clock : Clock @[:@4.4]
    input reset : UInt<1> @[:@5.4]
    input io_a : UInt<3> @[:@6.4]
    input io_b : UInt<16> @[:@6.4]
    output io_out : UInt<18> @[:@6.4]
    output io_cout : UInt<1> @[:@6.4]
  
    node _T_14 = eq(io_a, UInt<1>("h0")) @[Booth.scala 15:15:@8.4]
    node _T_16 = eq(io_a, UInt<3>("h7")) @[Booth.scala 15:31:@9.4]
    node _T_17 = or(_T_14, _T_16) @[Booth.scala 15:23:@10.4]
    node _T_20 = cat(UInt<1>("h1"), UInt<17>("h0")) @[Cat.scala 30:58:@12.6]
    node _T_23 = eq(io_a, UInt<1>("h1")) @[Booth.scala 18:22:@17.6]
    node _T_25 = eq(io_a, UInt<2>("h2")) @[Booth.scala 18:38:@18.6]
    node _T_26 = or(_T_23, _T_25) @[Booth.scala 18:30:@19.6]
    node _T_29 = cat(UInt<1>("h1"), UInt<1>("h0")) @[Cat.scala 30:58:@21.8]
    node _T_30 = cat(_T_29, io_b) @[Cat.scala 30:58:@22.8]
    node _T_33 = eq(io_a, UInt<3>("h5")) @[Booth.scala 21:22:@27.8]
    node _T_35 = eq(io_a, UInt<3>("h6")) @[Booth.scala 21:38:@28.8]
    node _T_36 = or(_T_33, _T_35) @[Booth.scala 21:30:@29.8]
    node _T_39 = not(io_b) @[Booth.scala 22:43:@31.10]
    node _T_40 = cat(UInt<1>("h0"), UInt<1>("h1")) @[Cat.scala 30:58:@32.10]
    node _T_41 = cat(_T_40, _T_39) @[Cat.scala 30:58:@33.10]
    node _T_44 = eq(io_a, UInt<2>("h3")) @[Booth.scala 24:22:@38.10]
    node _T_47 = cat(UInt<1>("h1"), io_b) @[Cat.scala 30:58:@40.12]
    node _T_48 = cat(_T_47, UInt<1>("h0")) @[Cat.scala 30:58:@41.12]
    node _T_51 = not(io_b) @[Booth.scala 28:33:@46.12]
    node _T_53 = cat(UInt<1>("h0"), _T_51) @[Cat.scala 30:58:@47.12]
    node _T_54 = cat(_T_53, UInt<1>("h1")) @[Cat.scala 30:58:@48.12]
    node _GEN_0 = mux(_T_44, _T_48, _T_54) @[Booth.scala 24:31:@39.10]
    node _GEN_1 = mux(_T_44, UInt<1>("h0"), UInt<1>("h1")) @[Booth.scala 24:31:@39.10]
    node _GEN_2 = mux(_T_36, _T_41, _GEN_0) @[Booth.scala 21:47:@30.8]
    node _GEN_3 = mux(_T_36, UInt<1>("h1"), _GEN_1) @[Booth.scala 21:47:@30.8]
    node _GEN_4 = mux(_T_26, _T_30, _GEN_2) @[Booth.scala 18:47:@20.6]
    node _GEN_5 = mux(_T_26, UInt<1>("h0"), _GEN_3) @[Booth.scala 18:47:@20.6]
    node _GEN_6 = mux(_T_17, _T_20, _GEN_4) @[Booth.scala 15:40:@11.4]
    node _GEN_7 = mux(_T_17, UInt<1>("h0"), _GEN_5) @[Booth.scala 15:40:@11.4]
    io_out <= _GEN_6
    io_cout <= _GEN_7

  module MUL : @[:@453.2]
    input clock : Clock @[:@454.4]
    input reset : UInt<1> @[:@455.4]
    input io_a : UInt<32> @[:@456.4]
    input io_b : UInt<32> @[:@456.4]
    output io_c : UInt<64> @[:@456.4]
  
    inst booths_0 of Booth @[MUL.scala 16:27:@458.4]
    inst booths_1 of Booth @[MUL.scala 16:27:@465.4]
    inst booths_2 of Booth @[MUL.scala 16:27:@471.4]
    inst booths_3 of Booth @[MUL.scala 16:27:@477.4]
    inst booths_4 of Booth @[MUL.scala 16:27:@483.4]
    inst booths_5 of Booth @[MUL.scala 16:27:@489.4]
    inst booths_6 of Booth @[MUL.scala 16:27:@495.4]
    inst booths_7 of Booth @[MUL.scala 16:27:@501.4]
    inst booths_8 of Booth @[MUL.scala 16:27:@507.4]
    node _T_11 = bits(io_a, 1, 0) @[MUL.scala 18:33:@461.4]
    node _T_13 = cat(_T_11, UInt<1>("h0")) @[Cat.scala 30:58:@462.4]
    node _T_14 = bits(io_a, 3, 1) @[MUL.scala 20:29:@468.4]
    node _T_15 = bits(io_a, 5, 3) @[MUL.scala 20:29:@474.4]
    node _T_16 = bits(io_a, 7, 5) @[MUL.scala 20:29:@480.4]
    node _T_17 = bits(io_a, 9, 7) @[MUL.scala 20:29:@486.4]
    node _T_18 = bits(io_a, 11, 9) @[MUL.scala 20:29:@492.4]
    node _T_19 = bits(io_a, 13, 11) @[MUL.scala 20:29:@498.4]
    node _T_20 = bits(io_a, 15, 13) @[MUL.scala 20:29:@504.4]
    node _T_22 = bits(io_a, 15, 15) @[MUL.scala 22:61:@510.4]
    node _T_23 = cat(UInt<2>("h0"), _T_22) @[Cat.scala 30:58:@511.4]
    node _T_33 = cat(UInt<1>("h0"), booths_0.io_cout) @[Cat.scala 30:58:@514.4]
    node _T_34 = cat(UInt<1>("h0"), booths_1.io_cout) @[Cat.scala 30:58:@515.4]
    node _T_35 = cat(_T_34, _T_33) @[Cat.scala 30:58:@516.4]
    node _T_36 = cat(UInt<1>("h0"), booths_2.io_cout) @[Cat.scala 30:58:@517.4]
    node _T_37 = cat(UInt<1>("h0"), booths_3.io_cout) @[Cat.scala 30:58:@518.4]
    node _T_38 = cat(_T_37, _T_36) @[Cat.scala 30:58:@519.4]
    node _T_39 = cat(_T_38, _T_35) @[Cat.scala 30:58:@520.4]
    node _T_40 = cat(UInt<1>("h0"), booths_4.io_cout) @[Cat.scala 30:58:@521.4]
    node _T_41 = cat(UInt<1>("h0"), booths_5.io_cout) @[Cat.scala 30:58:@522.4]
    node _T_42 = cat(_T_41, _T_40) @[Cat.scala 30:58:@523.4]
    node _T_43 = cat(UInt<1>("h0"), booths_6.io_cout) @[Cat.scala 30:58:@524.4]
    node _T_44 = cat(booths_8.io_cout, UInt<1>("h0")) @[Cat.scala 30:58:@525.4]
    node _T_45 = cat(_T_44, booths_7.io_cout) @[Cat.scala 30:58:@526.4]
    node _T_46 = cat(_T_45, _T_43) @[Cat.scala 30:58:@527.4]
    node _T_47 = cat(_T_46, _T_42) @[Cat.scala 30:58:@528.4]
    node correct_low = cat(_T_47, _T_39) @[Cat.scala 30:58:@529.4]
    node _T_50 = cat(UInt<1>("h1"), UInt<1>("h0")) @[Cat.scala 30:58:@530.4]
    node _T_53 = cat(UInt<1>("h1"), UInt<1>("h0")) @[Cat.scala 30:58:@531.4]
    node _T_56 = cat(UInt<1>("h1"), UInt<1>("h0")) @[Cat.scala 30:58:@532.4]
    node _T_59 = cat(UInt<1>("h1"), UInt<1>("h0")) @[Cat.scala 30:58:@533.4]
    node _T_62 = cat(UInt<1>("h1"), UInt<1>("h0")) @[Cat.scala 30:58:@534.4]
    node _T_65 = cat(UInt<1>("h1"), UInt<1>("h0")) @[Cat.scala 30:58:@535.4]
    node _T_68 = cat(UInt<1>("h1"), UInt<1>("h0")) @[Cat.scala 30:58:@536.4]
    node _T_71 = cat(UInt<1>("h1"), UInt<1>("h1")) @[Cat.scala 30:58:@537.4]
    node _T_72 = cat(_T_68, _T_71) @[Cat.scala 30:58:@538.4]
    node _T_73 = cat(_T_62, _T_65) @[Cat.scala 30:58:@539.4]
    node _T_74 = cat(_T_73, _T_72) @[Cat.scala 30:58:@540.4]
    node _T_75 = cat(_T_56, _T_59) @[Cat.scala 30:58:@541.4]
    node _T_76 = cat(_T_50, _T_53) @[Cat.scala 30:58:@542.4]
    node _T_77 = cat(_T_76, _T_75) @[Cat.scala 30:58:@543.4]
    node correct_hgh = cat(_T_77, _T_74) @[Cat.scala 30:58:@544.4]
    node correct = cat(correct_hgh, correct_low) @[Cat.scala 30:58:@545.4]
    node _T_78 = shl(UInt<32>("h0"), 2) @[MUL.scala 35:20:@546.4]
    node _T_79 = add(_T_78, booths_8.io_out) @[MUL.scala 35:26:@547.4]
    node _T_80 = tail(_T_79, 1) @[MUL.scala 35:26:@548.4]
    node _T_81 = shl(_T_80, 2) @[MUL.scala 35:20:@549.4]
    node _T_82 = add(_T_81, booths_7.io_out) @[MUL.scala 35:26:@550.4]
    node _T_83 = tail(_T_82, 1) @[MUL.scala 35:26:@551.4]
    node _T_84 = shl(_T_83, 2) @[MUL.scala 35:20:@552.4]
    node _T_85 = add(_T_84, booths_6.io_out) @[MUL.scala 35:26:@553.4]
    node _T_86 = tail(_T_85, 1) @[MUL.scala 35:26:@554.4]
    node _T_87 = shl(_T_86, 2) @[MUL.scala 35:20:@555.4]
    node _T_88 = add(_T_87, booths_5.io_out) @[MUL.scala 35:26:@556.4]
    node _T_89 = tail(_T_88, 1) @[MUL.scala 35:26:@557.4]
    node _T_90 = shl(_T_89, 2) @[MUL.scala 35:20:@558.4]
    node _T_91 = add(_T_90, booths_4.io_out) @[MUL.scala 35:26:@559.4]
    node _T_92 = tail(_T_91, 1) @[MUL.scala 35:26:@560.4]
    node _T_93 = shl(_T_92, 2) @[MUL.scala 35:20:@561.4]
    node _T_94 = add(_T_93, booths_3.io_out) @[MUL.scala 35:26:@562.4]
    node _T_95 = tail(_T_94, 1) @[MUL.scala 35:26:@563.4]
    node _T_96 = shl(_T_95, 2) @[MUL.scala 35:20:@564.4]
    node _T_97 = add(_T_96, booths_2.io_out) @[MUL.scala 35:26:@565.4]
    node _T_98 = tail(_T_97, 1) @[MUL.scala 35:26:@566.4]
    node _T_99 = shl(_T_98, 2) @[MUL.scala 35:20:@567.4]
    node _T_100 = add(_T_99, booths_1.io_out) @[MUL.scala 35:26:@568.4]
    node _T_101 = tail(_T_100, 1) @[MUL.scala 35:26:@569.4]
    node _T_102 = shl(_T_101, 2) @[MUL.scala 35:20:@570.4]
    node _T_103 = add(_T_102, booths_0.io_out) @[MUL.scala 35:26:@571.4]
    node _T_104 = tail(_T_103, 1) @[MUL.scala 35:26:@572.4]
    node _T_105 = add(_T_104, correct) @[MUL.scala 38:15:@573.4]
    node acc = tail(_T_105, 1) @[MUL.scala 38:15:@574.4]
    node _T_106 = bits(acc, 31, 0) @[MUL.scala 40:16:@575.4]
    io_c <= _T_106
    booths_0.clock <= clock
    booths_0.reset <= reset
    booths_0.io_a <= _T_13
    booths_0.io_b <= bits(io_b, 15, 0)
    booths_1.clock <= clock
    booths_1.reset <= reset
    booths_1.io_a <= _T_14
    booths_1.io_b <= bits(io_b, 15, 0)
    booths_2.clock <= clock
    booths_2.reset <= reset
    booths_2.io_a <= _T_15
    booths_2.io_b <= bits(io_b, 15, 0)
    booths_3.clock <= clock
    booths_3.reset <= reset
    booths_3.io_a <= _T_16
    booths_3.io_b <= bits(io_b, 15, 0)
    booths_4.clock <= clock
    booths_4.reset <= reset
    booths_4.io_a <= _T_17
    booths_4.io_b <= bits(io_b, 15, 0)
    booths_5.clock <= clock
    booths_5.reset <= reset
    booths_5.io_a <= _T_18
    booths_5.io_b <= bits(io_b, 15, 0)
    booths_6.clock <= clock
    booths_6.reset <= reset
    booths_6.io_a <= _T_19
    booths_6.io_b <= bits(io_b, 15, 0)
    booths_7.clock <= clock
    booths_7.reset <= reset
    booths_7.io_a <= _T_20
    booths_7.io_b <= bits(io_b, 15, 0)
    booths_8.clock <= clock
    booths_8.reset <= reset
    booths_8.io_a <= _T_23
    booths_8.io_b <= bits(io_b, 15, 0)
