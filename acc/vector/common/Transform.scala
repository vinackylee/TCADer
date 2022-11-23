package common

import chisel3._
import chisel3.util.{Cat, MuxCase}

class AxiRIO(val data_width: Int) extends Bundle {
  val data     = Input(UInt(data_width.W))
  val valid    = Input(Bool())
  val last     = Input(Bool())
  val id       = Input(UInt(4.W))
}

class AxiARIO(val addr_width: Int) extends Bundle {
  val ready   = Input(Bool())
  val valid   = Output(Bool())
  def fire: Bool = ready && valid
  val id      = Output(UInt(4.W))
  val addr    = Output(UInt(addr_width.W))
  val burst   = Output(UInt(2.W))
  val size    = Output(UInt(3.W))
  val len     = Output(UInt(8.W))
}

class AxiIO(val data_width: Int, val addr_width: Int) extends Bundle {
  val r  = new AxiRIO(data_width)
  val ar = new AxiARIO(addr_width)
}

object Latch {
  def apply(in: Bool, wait: Bool, addition: Bool = true.B): Bool = {
    val in_latch = RegInit(false.B)
    when (wait) { in_latch := false.B
    }.elsewhen(in && addition) {in_latch := true.B}
    in || in_latch
  }
}

class Transform(implicit conf: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val outer = new MemPortIo(conf.vlen)
    val inner = Flipped(new AxiIO(conf.vlen, conf.addr_width))
    val cyc = Input(UInt(conf.xprlen.W))
  })


  // req 是 DecoupledIO
  io.outer.req.valid     := io.inner.ar.valid
  io.outer.req.bits.addr := io.inner.ar.addr
  io.inner.ar.ready      := io.outer.req.ready

  // resp 是 ValidIO
  // AxiRIO 没有 ready
  // class AxiRIO(val data_width: Int) extends Bundle {
  //   val data     = Input(UInt(data_width.W))
  //   val valid    = Input(Bool())
  //   val last     = Input(Bool())
  //   val id       = Input(UInt(4.W))
  // }
  io.inner.r.data        := io.outer.resp.bits.data
  io.inner.r.valid       := io.outer.resp.valid
  io.inner.r.last        := io.outer.resp.valid
  io.inner.r.id          := conf.iccRd

  io.outer.req.bits.fcn  := M_VRD
  io.outer.req.bits.typ  := MT_VLEN
  io.outer.req.bits.data := DontCare

//  when (io.cyc >= 11610.U && io.cyc <= 11620.U) {
//    printf("Transform: Cyc = %d lfsr = %d cnt = [%d %d %d %d] pc = %x %x valid [%x %x %x %x] [%x %x] inst = DASM(%x) \n"
//      , io.cyc
//      , lfsr5
//      , cnt
//      , actual_cnt
//      , expect_cnt
//      , valid_cnt
//      , io.outer.req.bits.addr
//      , io.inner.ar.addr
//      , valid
//      , wire_valid
//      , first_valid
//      , double_valid
//      , io.inner.ar.valid
//      , io.inner.ar.ready
//      , io.outer.resp.bits.data
//    )
//  }

}

class SimpleTrans(implicit conf: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val outer = new MemPortIo(conf.vlen)
    val inner = Flipped(new MemPortIo(conf.vlen))
    val cyc = Input(UInt(conf.xprlen.W))
  })
  io.inner.req.ready := true.B
  val valid = RegInit(false.B)
  val bits  = Reg(new MemReq(conf.vlen))
  val stall = valid && !io.outer.resp.valid
  when (!stall) {
    valid := io.inner.req.valid
    bits  := io.inner.req.bits
  }
  io.outer.req.valid := valid
  io.outer.req.bits  := bits
  io.inner.resp := io.outer.resp
  //TODO: print memory trace
 // when (io.outer.req.valid && !stall) {
 //   printf("\nMemory: Cyc= %d ", io.cyc)
 //   when (io.outer.req.bits.fcn === M_VWR) {
 //     printf("STORE[ %x %x %x]\n",
 //       io.outer.req.bits.typ,
 //       io.outer.req.bits.addr,
 //       io.outer.req.bits.data)
 //   }.otherwise {
 //     printf("LOAD[ %x %x]\n",
 //       io.outer.req.bits.typ,
 //       io.outer.req.bits.addr)
 //   }
 // }

}