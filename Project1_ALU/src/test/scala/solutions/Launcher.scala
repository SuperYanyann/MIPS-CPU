package solutions

import chisel3._
import chisel3.iotesters.{Driver, TesterOptionsManager}
import utils.TutorialRunner


object Launcher{
  val tests = Map(
    "Adder" -> { (manager: TesterOptionsManager) =>
      Driver.execute(() => new Adder(), manager) {
        (c) => new AdderTests(c)
      }},

      "MUL" -> { (manager: TesterOptionsManager) =>
       Driver.execute(() => new MUL(),manager){
         (c) => new MULtests(c)
       }},

     "ALU" -> { (manager: TesterOptionsManager) =>
      Driver.execute(() => new ALU(),manager){
        (c) => new ALU_test(c)
      }}
  )

  def main(args: Array[String]): Unit = {
  TutorialRunner("solutions", tests, args)
}
}
