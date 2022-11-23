
package chipyard.tests

import scala.util.Random

import org.scalatest._

import chisel3._
import chisel3.util._
import chisel3.iotesters._

import freechips.rocketchip.config.{Parameters,Config}
import freechips.rocketchip.system._
//import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import boom.common._

import boom._
import boom.exu._
import boom.acc._

import chipyard._
import chipyard.config._
import chipyard.iobinders._

class BHTTests(c: OuterProduct) extends PeekPokeTester(c) {
  println("*********************test*********************")
}

object testMain {
//  implicit val boomParams: Parameters = BoomTestUtils.getBoomParameters("LargeBoomConfig")
  def main(args: Array[String]): Unit = {
    chisel3.iotesters.Driver.execute(args, () => new OuterProduct)( c => new BHTTests(c))
  }
}
