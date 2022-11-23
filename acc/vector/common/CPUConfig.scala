package common
import chisel3._
case class CPUConfig()
{
  val xprlen = 32
  val pcInc = 8
  val nInst: Int = pcInc >> 2
  require(nInst == 2)
  val delayFechinst   = true
  val use_cc: Boolean = delayFechinst && true // use icache under delay Fech inst setting is on
  val hasBTB = true
  val pcLSB = 2
  val verbose = false
  val incRd: UInt = 0.U(4.W)
  val iccRd: UInt = 1.U(4.W)
  val inst_width = 32
  val data_width = 32
  val addr_width = 32
  // vector register length
  val vlen   = 512
  val sew    = 9   // log(512)
}
