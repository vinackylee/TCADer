/*
package boom.tests

import scala.util.Random

import org.scalatest._

import chisel3._
import chisel3.util._
import chisel3.iotesters._

import freechips.rocketchip.config.{Parameters,Config}
import freechips.rocketchip.system._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
//import boom.common.{BoomTilesKey,LargeBoomConfig}
import chipyard._

import boom._
import boom.exu._
import boom.bpu._


class YGJKTests(c: YGJKModuleImp) extends PeekPokeTester(c) {
  println("*********************test*********************") 
 // poke(c.io.a, a)
 for( i <- 1 to 9){
	step(1)

}
}


object testMain {
  implicit val boomParams: Parameters = BoomTestUtils.getBoomParameters("LargeBoomConfig")
  val rocc2ygjk = LazyModule(new RoCC2YGJK(OpcodeSet.all))
  def main(args: Array[String]): Unit = {
    chisel3.iotesters.Driver.execute(args,() =>new YGJKModuleImp(rocc2ygjk)(boomParams))( c => new YGJKTests(c))
    //chisel3.iotesters.Driver.execute(args,() =>new RoCC2YGJK(boomParams))( c => new R2YTests(c))  
}

}
*/
