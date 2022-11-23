
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

import boom._
import boom.exu._
import boom.bpu._
import boom.acc._


class MACTests(c: OuterProduct) extends PeekPokeTester(c) {
  println("*********************test*********************") 
 // poke(c.io.a, a)
 for( i <- 1 to 9){
	step(1)

}
}


object testMAC {
  def main(args: Array[String]): Unit = {
    chisel3.iotesters.Driver.execute(args,() =>new OuterProduct())( c => new MACTests(c))
    //chisel3.iotesters.Driver.execute(args,() =>new RoCC2YGJK(boomParams))( c => new R2YTests(c))  
}

}

