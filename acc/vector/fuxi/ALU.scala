package fuxi

import chisel3._
import chisel3.util.{Cat, MuxCase}
import common.CPUConfig

class Target(val addr_width: Int) extends Bundle {
  val brjmp = UInt(addr_width.W)
  val jpreg = UInt(addr_width.W)
  val conti = UInt(addr_width.W)
}

class CtrlIO extends Bundle {
  val fun      = Input(UInt(ALU_X.getWidth.W))
  val br_type  = Input(UInt(BR_N.getWidth.W))
  val wb_sel   = Input(UInt(WB_X.getWidth.W))
  val pc_sel   = Output(UInt(PC_4.getWidth.W))
}

class ALU(implicit conf: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val op1  = Input(UInt(conf.xprlen.W))
    val op2  = Input(UInt(conf.xprlen.W))
    val pc   = Input(UInt(conf.xprlen.W))
    val ctrl = new CtrlIO()
    val rs2_data = Input(UInt(conf.xprlen.W))
    val result   = Output(UInt(conf.xprlen.W))
    val target   = Output(new Target(conf.xprlen))
  })

  // ALU
  val alu_shamt: UInt  = io.op2(4,0).asUInt
  val add_result: UInt = (io.op1 + io.op2)(conf.xprlen-1,0)

  //only for debug purposes right now until debug() works
  val result = Wire(UInt(conf.xprlen.W))
  result := MuxCase(0.U, Array( // FIXME: one Hot
    (io.ctrl.fun === ALU_ADD)   -> add_result,
    (io.ctrl.fun === ALU_SUB)   -> (io.op1 - io.op2).asUInt,
    (io.ctrl.fun === ALU_AND)   -> (io.op1 & io.op2).asUInt,
    (io.ctrl.fun === ALU_OR)    -> (io.op1 | io.op2).asUInt,
    (io.ctrl.fun === ALU_XOR)   -> (io.op1 ^ io.op2).asUInt,
    (io.ctrl.fun === ALU_SLT)   -> (io.op1.asSInt < io.op2.asSInt).asUInt,
    (io.ctrl.fun === ALU_SLTU)  -> (io.op1 < io.op2).asUInt,
    (io.ctrl.fun === ALU_SLL)   -> (io.op1 << alu_shamt)(conf.xprlen-1, 0).asUInt,
    (io.ctrl.fun === ALU_SRA)   -> (io.op1.asSInt >> alu_shamt).asUInt,
    (io.ctrl.fun === ALU_SRL)   -> (io.op1 >> alu_shamt).asUInt,
    (io.ctrl.fun === ALU_COPY_1)->  io.op1,
    (io.ctrl.fun === ALU_COPY_2)->  io.op2))

  // Branch/Jump Target Calculation
  io.target.brjmp    := io.pc + io.op2
  io.target.jpreg    := Cat(add_result(conf.xprlen-1,1), 0.U(1.W))
  io.target.conti    := io.pc + 4.U
  io.result := Mux(io.ctrl.wb_sel === WB_PC4, io.target.conti, result)

  val br_eq: Bool  = io.op1 === io.rs2_data
  val br_lt: Bool  = io.op1.asSInt < io.rs2_data.asSInt
  val br_ltu: Bool = io.op1.asUInt < io.rs2_data.asUInt

  // Branch Logic
  io.ctrl.pc_sel :=
    Mux(io.ctrl.br_type === BR_N  , PC_4,
    Mux(io.ctrl.br_type === BR_NE , Mux(!br_eq,  PC_BRJMP, PC_4),
    Mux(io.ctrl.br_type === BR_EQ , Mux( br_eq,  PC_BRJMP, PC_4),
    Mux(io.ctrl.br_type === BR_GE , Mux(!br_lt,  PC_BRJMP, PC_4),
    Mux(io.ctrl.br_type === BR_GEU, Mux(!br_ltu, PC_BRJMP, PC_4),
    Mux(io.ctrl.br_type === BR_LT , Mux( br_lt,  PC_BRJMP, PC_4),
    Mux(io.ctrl.br_type === BR_LTU, Mux( br_ltu, PC_BRJMP, PC_4),
    Mux(io.ctrl.br_type === BR_J  , PC_BRJMP,
    Mux(io.ctrl.br_type === BR_JR , PC_JALR,
    PC_4 )))))))))
}


// vector
class v_ALU(implicit conf: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val op1  = Input(UInt(conf.vlen.W))
    val op2  = Input(UInt(conf.vlen.W))
    val fun  = Input(UInt(ALU_X.getWidth.W))
    val result   = Output(UInt(conf.vlen.W))
  })

  io := DontCare
  // ALU
  val alu_shamt:  UInt = io.op2(4,0).asUInt
  val add_result: UInt = (io.op1 + io.op2)(conf.vlen - 1,0)
  val mul_result: UInt = (io.op1 * io.op2)(conf.vlen - 1,0)
  //only for debug purposes right now until debug() works
  val result = Wire(UInt(conf.vlen.W))
  result := MuxCase(0.U, Array( // FIXME: one Hot
    (io.fun === ALU_ADD)   -> add_result,
    (io.fun === ALU_MUL)   -> mul_result,
    
    (io.fun === ALU_SUB)   -> (io.op1 - io.op2).asUInt,
    (io.fun === ALU_AND)   -> (io.op1 & io.op2).asUInt,
    (io.fun === ALU_OR)    -> (io.op1 | io.op2).asUInt,
    (io.fun === ALU_XOR)   -> (io.op1 ^ io.op2).asUInt,
    (io.fun === ALU_SLT)   -> (io.op1.asSInt < io.op2.asSInt).asUInt,
    (io.fun === ALU_SLTU)  -> (io.op1 < io.op2).asUInt,
    (io.fun === ALU_SLL)   -> (io.op1 << alu_shamt)(conf.vlen - 1, 0).asUInt,
    (io.fun === ALU_SRA)   -> (io.op1.asSInt >> alu_shamt).asUInt,
    (io.fun === ALU_SRL)   -> (io.op1 >> alu_shamt).asUInt,
    (io.fun === ALU_COPY_1)->  io.op1,
    (io.fun === ALU_COPY_2)->  io.op2))

  io.result := result

  // when (io.fun === ALU_ADD) {
  //   printf("v_ALU op1 = %x  op2 = %x result = %x\n"
  //         , io.op1(31, 0)
  //         , io.op2(31, 0)
  //         , io.result(31, 0)
  //   )
  // }
    
}

class VALU (implicit conf: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val op1  = Input(UInt(conf.vlen.W))
    val op2  = Input(UInt(conf.vlen.W))
    val fun  = Input(UInt(ALU_X.getWidth.W))
    val sew  = Input(UInt(SEW_X.getWidth.W))
    val result   = Output(UInt(conf.vlen.W))
  })


  val v_alu = VecInit(Seq.fill(conf.vlen / 8)(Module(new v_ALU())).map(_.io))

  // val op1_data_sew8  = Wire(Vec(conf.vlen / 8, UInt(8.W)))
  // val op2_data_sew8  = Wire(Vec(conf.vlen / 8, UInt(8.W)))

  // val op1_data_sew16 = Wire(Vec(conf.vlen / 16, UInt(16.W)))
  // val op2_data_sew16 = Wire(Vec(conf.vlen / 16, UInt(16.W)))
  
  val op1_data_sew32 = Wire(Vec(conf.vlen / 32, UInt(32.W)))
  val op2_data_sew32 = Wire(Vec(conf.vlen / 32, UInt(32.W)))

  // val op1_data_sew64 = Wire(Vec(conf.vlen / 64, UInt(64.W)))
  // val op2_data_sew64 = Wire(Vec(conf.vlen / 64, UInt(64.W)))


  val v_alu_bits = WireInit(VecInit(Seq.fill(conf.vlen)(0.U(1.W))))

  // only test for 32 bits
  for (i <- 0 until conf.vlen / 32) {
    op1_data_sew32(i) := io.op1(i * 32 + 31, i * 32)
    op2_data_sew32(i) := io.op2(i * 32 + 31, i * 32)
  }

  for (i <- 0 until conf.vlen / 8) {
      v_alu(i).op1 := DontCare
      v_alu(i).op2 := DontCare
      v_alu(i).fun := DontCare
  }

  for (i <- 0 until conf.vlen / 32) {
      v_alu(i).op1 := op1_data_sew32(i)
      v_alu(i).op2 := op2_data_sew32(i)
      v_alu(i).fun := io.fun
  }
  

  io.result := Cat(v_alu.map(_.result(31,0)).reverse)
  
  // when (io.fun === ALU_ADD) {
  //   printf("VALU\nop1 = %x\nop2 = %x\nresult = %x\n", io.op1(31, 0), io.op2(31, 0), io.result(31, 0))
  // }

    
}