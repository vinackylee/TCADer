package fuxi

import chisel3._
import chisel3.util.Valid
import common._


import freechips.rocketchip.config._
import boom.exu.ygjk._

class Withacc extends Config((site,here,up) => {  
    case BuildYGAC => 
        (p:Parameters) => {
            implicit val conf = new CPUConfig   
            val myAccel = Module(new Core)
            myAccel
        }
    }
)

class InterfaceIO(val data_width: Int) extends Bundle {
  val xcpt = Input(new Valid(UInt(data_width.W)))
  val kill = Input(Bool())
  val forward  = Input(Vec(2, Bool()))
  val inst     = Output(Vec(2, Valid(UInt(data_width.W))))
  val pc       = Output(Vec(2, UInt(data_width.W)))
  val split    = Output(Bool())
  val pred     = Output(new Predict(data_width))
  val branch   = Output(Bool())
  val call     = Output(Bool())
  val retn     = Output(Bool())
  val bj_sel   = Output(Vec(2, Bool())) //determine pick which btb
  val ras_pop  = Input(Bool())
  val ras_push = Input(Valid(UInt(data_width.W)))
  val fb_pc    = Input(UInt(data_width.W))
  val fb_type  = Input(UInt(BTBType.SZ.W))
  val feedBack = Input(new PredictVal(data_width))
}

class Core(implicit conf: CPUConfig) extends MyACCModule with BTBParams {

  val trans  = Module(new Transform())
 

  val frontEnd = Module(new FrontEnd())
  val backEnd  = Module(new BackEnd())


  frontEnd.io.pc  := io.ctl.config.cfgData1
  
  frontEnd.io.mem <> trans.io.inner
  trans.io.outer  <> io.imem

  backEnd.io.mem  <> io.dmem

  frontEnd.io.back <> backEnd.io.front
  frontEnd.io.cyc  := backEnd.io.cyc
  

  io.ctl.axh_jdone := backEnd.io.finish

  io.ctl.axh_jrunning := DontCare
  
  trans.io.cyc := backEnd.io.cyc

  when (io.ctl.hax_jval) {
    printf("\nfuxi start\n")
  }

  when (backEnd.io.finish) {
     printf("\nfuxi finish\n")
  }

  when (io.dmem.req.valid && io.dmem.req.bits.fcn(0) ) {
    printf("\nw addr %x data %x\n", io.dmem.req.bits.addr, io.dmem.req.bits.data(31, 0))
  }

  when (io.dmem.resp.valid && ! io.dmem.req.bits.fcn(0) ) {
    printf("\nr addr %x data %x\n", io.dmem.req.bits.addr, io.dmem.resp.bits.data(31, 0))
  }
}