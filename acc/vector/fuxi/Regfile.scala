//**************************************************************************
// RISCV Processor Register File
//--------------------------------------------------------------------------
//

package fuxi

import chisel3._
import common._

class Regfile(implicit val conf: CPUConfig) extends Module {
  val io = IO(new Bundle{
    val rs1_addr = Input(Vec(2, UInt(5.W)))
    val rs1_data = Output(Vec(2, UInt(conf.xprlen.W)))
    val rs2_addr = Input(Vec(2, UInt(5.W)))
    val rs2_data = Output(Vec(2, UInt(conf.xprlen.W)))

    val waddr    = Input(Vec(2, UInt(5.W)))
    val wdata    = Input(Vec(2, UInt(conf.xprlen.W)))
    val wen      = Input(Vec(2, Bool()))
  })

  val regfile = Mem(32,UInt(conf.xprlen.W))
  
  for (i <- 0 until 2) {
    when (io.wen(i)) {
      regfile(io.waddr(i)) := io.wdata(i)  
      printf("scala%d = %x\n", io.waddr(i), io.wdata(i))
    }
  }
  
  for (i <- 0 until 2) {
    io.rs1_data(i) := Mux(io.rs1_addr(i) =/= 0.U, regfile(io.rs1_addr(i)), 0.U)
    io.rs2_data(i) := Mux(io.rs2_addr(i) =/= 0.U, regfile(io.rs2_addr(i)), 0.U)
  }
}

class VRegfile(implicit val conf: CPUConfig) extends Module {
  val io = IO(new Bundle{
    val vs1_addr = Input(Vec(2, UInt(5.W)))
    val vs1_data = Output(Vec(2, UInt(conf.vlen.W)))

    val vs2_addr = Input(Vec(2, UInt(5.W)))
    val vs2_data = Output(Vec(2, UInt(conf.vlen.W)))

    val vd_addr = Input(Vec(2, UInt(5.W)))
    val vd_data = Output(Vec(2, UInt(conf.vlen.W)))

    val waddr    = Input(Vec(2, UInt(5.W)))
    val wdata    = Input(Vec(2, UInt(conf.vlen.W)))
    val wen      = Input(Vec(2, Bool()))

    val vmask    = Input(Vec(2, Bool()))
    val vl       = Input(Vec(2, UInt(32.W)))
  })

  val regfile = Mem(32, UInt(conf.vlen.W))

  for (i <- 0 until 2) {
    io.vs1_data(i) := Mux(io.vs1_addr(i) =/= 0.U, regfile(io.vs1_addr(i)), 0.U)
    io.vs2_data(i) := Mux(io.vs2_addr(i) =/= 0.U, regfile(io.vs2_addr(i)), 0.U)
    io.vd_data(i)  := Mux(io.vd_addr(i) =/= 0.U,  regfile(io.vd_addr(i)), 0.U)
  }

  // debug
  for (i <- 0 until 2) {
    when (io.wen(i) === true.B) {
      regfile(io.waddr(i)) := io.wdata(i)  
      printf("vector%d = %d\n", io.waddr(i), io.wdata(i)(31, 0))
    }
  }
}