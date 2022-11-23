package fuxi

import chisel3._
import chisel3.util._
import common._

object Stage {
  val DEC = 0
  val EXE = 1
  val MEM = 2
  val Num: Int = MEM + 1
}

class WbCrtl extends Bundle {
  val rf_wen = Bool()
}

class MemCrtl extends WbCrtl {
  val mem_en   = Bool()
  val csr_cmd  = UInt(CSR.SZ)
  val illegal  = Bool()
}

class ExeCrtl extends MemCrtl {
  val bj_sel  = Bool()
  val br_type = UInt(BR_N.getWidth.W)
}

class DecCrtl extends WbCrtl {
  val rs1_oen = Bool()
  val rs2_oen = Bool()
  val csr_cmd = UInt(CSR.SZ)

  // vector
  val vrf_wen = Bool()
}

class Wb extends Bundle {
  val rf_wen = Bool()
  val wbaddr = UInt(5.W)

  // vector
  val v_inst = Bool()
  val vrf_wen = Bool()
  val vwbaddr = UInt(5.W)
  // val vwbdata = UInt(conf.vlen.W)
  val vwbdata = UInt(512.W)
  val vmask   = Bool()
  val vl      = UInt(32.W)

  val vfinish = Bool()
}

class Mem(implicit val conf: CPUConfig) extends Wb {
  val mem_en   = Bool()
  val csr_cmd  = UInt(CSR.SZ)
  val illegal  = Bool()
  val pc       = UInt(conf.xprlen.W)
  val inst     = UInt(conf.xprlen.W)
  val rs2_data = UInt(conf.xprlen.W)
  val wb_sel   = UInt(WB_X.getWidth.W)
  val mem_fcn  = UInt(M_X.getWidth.W)
  val mem_typ  = UInt(MT_X.getWidth.W)
}

class Exe(implicit conf: CPUConfig) extends Mem {
  val br_type  = UInt(BR_N.getWidth.W)
  val bj_sel   = Bool()
  val op1_data = UInt(conf.xprlen.W)
  val op2_data = UInt(conf.xprlen.W)
  val alu_fun  = UInt(ALU_X.getWidth.W)

  // vector
  val vop1_data = UInt(conf.vlen.W)
  val vop2_data = UInt(conf.vlen.W)
  val vop3_data = UInt(conf.vlen.W)
  val vs2_data  = UInt(conf.vlen.W)
  val vsew      = UInt(SEW_X.getWidth.W)
  val vacc      = Bool()
}

object Pulse {
  def apply(in: Bool, forward: Bool): Bool = {
    val in_latch = RegInit(true.B)
    when (forward) { in_latch := true.B
    }.elsewhen(in) { in_latch := false.B}
    in && in_latch
  }
}

class BackEnd(implicit conf: CPUConfig) extends Module with BTBParams {
  val io = IO(new Bundle {
    val mem    = new MemPortIo(conf.vlen)
    val cyc    = Output(UInt(conf.xprlen.W))
    val finish = Output(Bool())
    val front  = Flipped(new InterfaceIO(conf.xprlen))
  })

  val csr = Module(new CSRFile())
  val regfile = Module(new Regfile())
  val alu = Array.fill(2)(Module(new ALU()).io)
  
  io.cyc    := csr.io.time(conf.xprlen-1,0)

  // vector
  val vregfile = Module(new VRegfile())
  val valu = Array.fill(2)(Module(new VALU()).io)
  val vacc = Array.fill(2)(Module(new VALU()).io)
  // vector end ============


  // Decode Stage ===========================================================================================================================================
  // ========================================================================================================================================================
  val xcpt      = Wire(Valid(UInt(conf.xprlen.W)))
  val stall     = Wire(Vec(2, Vec(Stage.Num, Bool())))

  val dec       = Array.fill(2)(Module(new InstDecoder()).io)
  val dec_wire  = Wire(Vec(2, new DecCrtl))

  val exe       = Reg(Vec(2, new Exe))
  val exe_valid = RegInit(VecInit(Seq.fill(2)(false.B)))
  val exe_wire  = Wire(Vec(2, new ExeCrtl))

  val mem       = Reg(Vec(2, new Mem()))
  val mem_valid = RegInit(VecInit(Seq.fill(2)(false.B)))
  val mem_wire  = Wire(Vec(2, new MemCrtl))

  val wb        = Reg(Vec(2, new Wb()))
  val wb_valid  = RegInit(VecInit(Seq.fill(2)(false.B)))
  val wb_wire   = Wire(Vec(2, new WbCrtl))

  val exe_wbdata = Wire(Vec(2, UInt(conf.xprlen.W)))
  val mem_wbdata = Wire(Vec(2, UInt(conf.xprlen.W)))
  val wb_wbdata  = Reg(Vec(2, UInt(conf.xprlen.W)))

  //vector
  val exe_vwbdata = Wire(Vec(2, UInt(conf.vlen.W)))
  val mem_vwbdata = Wire(Vec(2, UInt(conf.vlen.W)))
  // vector end

  // Bypass Muxes
  val dec_op1_data  = Wire(Vec(2, UInt(conf.xprlen.W)))
  val dec_op2_data  = Wire(Vec(2, UInt(conf.xprlen.W)))
  val dec_rs1_data  = Wire(Vec(2, UInt(conf.xprlen.W)))
  val dec_rs2_data  = Wire(Vec(2, UInt(conf.xprlen.W)))
  
  // Register File
  val rf_rs1_data = Wire(Vec(2, UInt()))
  val rf_rs2_data = Wire(Vec(2, UInt()))

  // vector Bypass Muxes
  val dec_vop1_data  = Wire(Vec(2, UInt(conf.vlen.W)))
  val dec_vop2_data  = Wire(Vec(2, UInt(conf.vlen.W)))
  val dec_vop3_data  = Wire(Vec(2, UInt(conf.vlen.W)))
  val dec_vd_data   = Wire(Vec(2, UInt(conf.vlen.W)))
  val dec_vs1_data  = Wire(Vec(2, UInt(conf.vlen.W)))
  val dec_vs2_data  = Wire(Vec(2, UInt(conf.vlen.W)))

  // vector VRegister File
  val vrf_vs1_data = Wire(Vec(2, UInt()))
  val vrf_vs2_data = Wire(Vec(2, UInt()))
  val vrf_vd_data = Wire(Vec(2, UInt()))

  val vtype        = csr.io.vtype
  val vma          = vtype.vma
  val vta          = vtype.vta
  val vsew         = vtype.vsew
  val vlmul        = Cat(vtype.vlmul_f, vtype.vlmul_nf)

  val vl           = csr.io.vl
  // vector end ====================


  for (i <- 0 until 2) {
    dec(i).inst  := io.front.inst(i).bits
    dec_wire(i).rs1_oen := io.front.inst(i).valid && dec(i).cinfo.rs1_oen
    dec_wire(i).rs2_oen := io.front.inst(i).valid && dec(i).cinfo.rs2_oen
    dec_wire(i).rf_wen  := io.front.inst(i).valid && dec(i).cinfo.rf_wen && dec(i).wbaddr =/= 0.U
    dec_wire(i).csr_cmd := Mux(io.front.inst(i).valid, dec(i).cinfo.csr_cmd, CSR.N)
    dec_wire(i).vrf_wen  := io.front.inst(i).valid && dec(i).cinfo.vrf_wen

    regfile.io.rs1_addr(i) := dec(i).rs1_addr
    regfile.io.rs2_addr(i) := dec(i).rs2_addr
    rf_rs1_data(i) := regfile.io.rs1_data(i)
    rf_rs2_data(i) := regfile.io.rs2_data(i)

    vregfile.io.vs1_addr(i) := dec(i).vs1_addr
    vregfile.io.vs2_addr(i) := dec(i).vs2_addr
    vregfile.io.vd_addr(i)  := dec(i).vwbaddr

    vrf_vs1_data(i) := vregfile.io.vs1_data(i)
    vrf_vs2_data(i) := vregfile.io.vs2_data(i)
    vrf_vd_data(i)  := vregfile.io.vd_data(i)
    // vector end ===============

    dec_rs1_data(i) := MuxCase(rf_rs1_data(i), Array(
      ((exe(1).wbaddr === dec(i).rs1_addr) && exe_wire(1).rf_wen) -> exe_wbdata(1),
      ((exe(0).wbaddr === dec(i).rs1_addr) && exe_wire(0).rf_wen) -> exe_wbdata(0),
      ((mem(1).wbaddr === dec(i).rs1_addr) && mem_wire(1).rf_wen) -> mem_wbdata(1),
      ((mem(0).wbaddr === dec(i).rs1_addr) && mem_wire(0).rf_wen) -> mem_wbdata(0),
      (( wb(1).wbaddr === dec(i).rs1_addr) &&  wb_wire(1).rf_wen) -> wb_wbdata(1),
      (( wb(0).wbaddr === dec(i).rs1_addr) &&  wb_wire(0).rf_wen) -> wb_wbdata(0)
    ))

    dec_rs2_data(i) := MuxCase(rf_rs2_data(i), Array(
      ((exe(1).wbaddr === dec(i).rs2_addr) && exe_wire(1).rf_wen) -> exe_wbdata(1),
      ((exe(0).wbaddr === dec(i).rs2_addr) && exe_wire(0).rf_wen) -> exe_wbdata(0),
      ((mem(1).wbaddr === dec(i).rs2_addr) && mem_wire(1).rf_wen) -> mem_wbdata(1),
      ((mem(0).wbaddr === dec(i).rs2_addr) && mem_wire(0).rf_wen) -> mem_wbdata(0),
      (( wb(1).wbaddr === dec(i).rs2_addr) &&  wb_wire(1).rf_wen) -> wb_wbdata(1),
      (( wb(0).wbaddr === dec(i).rs2_addr) &&  wb_wire(0).rf_wen) -> wb_wbdata(0)
    ))

    dec_op1_data(i) := MuxCase(dec_rs1_data(i), Array(
      (dec(i).cinfo.op1_sel === OP1_IMZ)-> dec(i).dinfo.imm_z,
      (dec(i).cinfo.op1_sel === OP1_PC) -> io.front.pc(i)))

    dec_op2_data(i) := MuxCase(dec_rs2_data(i), Array(
      (dec(i).cinfo.op2_sel === OP2_ITYPE)  -> dec(i).dinfo.imm_i,
      (dec(i).cinfo.op2_sel === OP2_STYPE)  -> dec(i).dinfo.imm_s,
      (dec(i).cinfo.op2_sel === OP2_SBTYPE) -> dec(i).dinfo.imm_sb,
      (dec(i).cinfo.op2_sel === OP2_UTYPE)  -> dec(i).dinfo.imm_u,
      (dec(i).cinfo.op2_sel === OP2_UJTYPE) -> dec(i).dinfo.imm_uj,
      (dec(i).cinfo.op2_sel === OP2_X)      -> 0.U))

    // vector Bypass
    dec_vs1_data(i) := MuxCase(vrf_vs1_data(i), Array(
       ((exe(1).vwbaddr === dec(i).vs1_addr) && exe(1).vrf_wen && exe_valid(1)) -> exe_vwbdata(1),
       ((exe(0).vwbaddr === dec(i).vs1_addr) && exe(0).vrf_wen && exe_valid(0)) -> exe_vwbdata(0),
       ((mem(1).vwbaddr === dec(i).vs1_addr) && mem(1).vrf_wen && mem_valid(1)) -> mem_vwbdata(1),
       ((mem(0).vwbaddr === dec(i).vs1_addr) && mem(0).vrf_wen && mem_valid(0)) -> mem_vwbdata(0),
       (( wb(1).vwbaddr === dec(i).vs1_addr) && wb(1).vrf_wen && wb_valid(1))  -> wb(1).vwbdata,
       (( wb(0).vwbaddr === dec(i).vs1_addr) && wb(0).vrf_wen && wb_valid(0))  -> wb(0).vwbdata
    ))

    dec_vs2_data(i) := MuxCase(vrf_vs2_data(i), Array(
       ((exe(1).vwbaddr === dec(i).vs2_addr) && exe(1).vrf_wen && exe_valid(1)) -> exe_vwbdata(1),
       ((exe(0).vwbaddr === dec(i).vs2_addr) && exe(0).vrf_wen && exe_valid(0)) -> exe_vwbdata(0),
       ((mem(1).vwbaddr === dec(i).vs2_addr) && mem(1).vrf_wen && mem_valid(1)) -> mem_vwbdata(1),
       ((mem(0).vwbaddr === dec(i).vs2_addr) && mem(0).vrf_wen && mem_valid(0)) -> mem_vwbdata(0),
       (( wb(1).vwbaddr === dec(i).vs2_addr) && wb(1).vrf_wen && wb_valid(1))  -> wb(1).vwbdata,
       (( wb(0).vwbaddr === dec(i).vs2_addr) && wb(0).vrf_wen && wb_valid(0))  -> wb(0).vwbdata
    ))

    dec_vd_data(i) := MuxCase(vrf_vd_data(i), Array(
       ((exe(1).vwbaddr === dec(i).vwbaddr) && exe(1).vrf_wen && exe_valid(1)) -> exe_vwbdata(1),
       ((exe(0).vwbaddr === dec(i).vwbaddr) && exe(0).vrf_wen && exe_valid(0)) -> exe_vwbdata(0),
       ((mem(1).vwbaddr === dec(i).vwbaddr) && mem(1).vrf_wen && mem_valid(1)) -> mem_vwbdata(1),
       ((mem(0).vwbaddr === dec(i).vwbaddr) && mem(0).vrf_wen && mem_valid(0)) -> mem_vwbdata(0),
       (( wb(1).vwbaddr === dec(i).vwbaddr) && wb(1).vrf_wen && wb_valid(1))   -> wb(1).vwbdata,
       (( wb(0).vwbaddr === dec(i).vwbaddr) && wb(0).vrf_wen && wb_valid(0))   -> wb(0).vwbdata
    ))

    dec_vop1_data(i) := MuxCase(dec_vs1_data(i), Array((dec(i).cinfo.op1_sel === OP1_RS1) -> Cat(Seq.fill(16)(dec_rs1_data(i)))))

    dec_vop2_data(i) := dec_vs2_data(i)

    dec_vop3_data(i) := dec_vd_data(i)
    // vector bypass end =================


    when (!stall(1)(Stage.EXE) && !stall(1)(Stage.MEM)) {
      exe(i).rf_wen   := dec(i).cinfo.rf_wen
      exe(i).mem_en   := dec(i).cinfo.mem_en
      // convert CSR instructions with raddr1 == 0 to read-only CSR commands
      exe(i).csr_cmd  := Mux((dec(i).cinfo.csr_cmd === CSR.S || dec(i).cinfo.csr_cmd === CSR.C) &&
                              dec(i).rs1_addr === 0.U, CSR.R, dec(i).cinfo.csr_cmd)
      exe(i).illegal  := dec(i).cinfo.illegal
      exe(i).br_type  := dec(i).cinfo.br_type
      exe(i).bj_sel   := io.front.bj_sel(i)
      exe(i).pc       := io.front.pc(i)
      exe(i).op1_data := dec_op1_data(i)
      exe(i).op2_data := dec_op2_data(i)
      exe(i).rs2_data := dec_rs2_data(i)
      exe(i).inst     := dec(i).inst
      exe(i).alu_fun  := dec(i).cinfo.alu_fun
      exe(i).wb_sel   := dec(i).cinfo.wb_sel
      exe(i).wbaddr   := dec(i).wbaddr
      exe(i).mem_fcn  := dec(i).cinfo.mem_fcn
      exe(i).mem_typ  := dec(i).cinfo.mem_typ

      // vector
      exe(i).v_inst    := dec(i).cinfo.v_inst
      exe(i).vop1_data := dec_vop1_data(i)
      exe(i).vop2_data := dec_vop2_data(i)
      exe(i).vop3_data := dec_vop3_data(i)
      exe(i).vs2_data  := dec_vop2_data(i)
      exe(i).vwbaddr   := dec(i).vwbaddr
      exe(i).vrf_wen   := dec(i).cinfo.vrf_wen
      exe(i).vmask     := dec(i).cinfo.vmask
      exe(i).vacc      := dec(i).cinfo.vacc
      exe(i).vfinish   := dec(i).cinfo.vfinish

      exe(i).vl        := vl
      exe(i).vsew      := vsew
      exe(i).vwbdata   := DontCare

      // debug
      // when (dec(i).cinfo.v_inst) {
      //     printf("\n%d DASM(%x)\n", i.U(2.W), dec(i).inst)
      //     printf("vs1 addr %d %x\n", dec(i).vs1_addr, dec_vop1_data(i))
      //     printf("vs2 addr %d %x\n", dec(i).vs2_addr, dec_vop2_data(i)(31, 0))  

      //     printf("ex0 addr %d %x\n", exe(0).vwbaddr, valu(0).result(31, 0))
      //     printf("ex1 addr %d %x\n", exe(1).vwbaddr, valu(1).result(31, 0))
      //     printf("me0 addr %d %x\n", mem(0).vwbaddr, mem_vwbdata(0)(31, 0))
      //     printf("me1 addr %d %x\n", mem(1).vwbaddr, mem_vwbdata(1)(31, 0))
      //     printf("wb0 addr %d %x\n", wb(0).vwbaddr,  wb(0).vwbdata(31, 0))
      //     printf("wb1 addr %d %x\n", wb(1).vwbaddr,  wb(1).vwbdata(31, 0))
      // }
      
    }
  }
  val exe_btb    = Reg(new Predict(conf.data_width))
  val exe_branch = Reg(Bool())
  val exe_call   = Reg(Bool())
  val exe_retn   = Reg(Bool())
  when (!stall(1)(Stage.EXE) && !stall(1)(Stage.MEM)) {
    exe_btb    := io.front.pred
    exe_branch := io.front.branch
    exe_call   := io.front.call
    exe_retn   := io.front.retn
  }

  val mispredict = Wire(Bool()) //based on is branch jump inst
  val exe_cancel = Wire(Bool())
  when (((stall(0)(Stage.DEC) || stall(1)(Stage.EXE)) && !stall(0)(Stage.EXE) && !stall(1)(Stage.MEM)) || xcpt.valid) {
    exe_valid(0) := false.B
  }.elsewhen(!stall(1)(Stage.EXE) && !stall(1)(Stage.MEM)) {
    exe_valid(0) := io.front.inst(0).valid && !mispredict
  }


  when ((stall(1)(Stage.DEC) && !stall(1)(Stage.EXE) && !stall(1)(Stage.MEM)) || xcpt.valid || exe_cancel) {
    exe_valid(1) := false.B
  }.elsewhen(!stall(1)(Stage.EXE) && !stall(1)(Stage.MEM)) {
    exe_valid(1) := io.front.inst(1).valid && !mispredict && !io.front.split
  }

  // debug
  // printf("DEC io.front.inst(0) %x io.front.inst(1) %x\n", io.front.inst(0).valid, io.front.inst(1).valid)

  // Execute Stage ==========================================================================================================================================
  //=========================================================================================================================================================
  val exe_bj_valid    = exe_wire.map(_.bj_sel).reduce(_||_) //only have one
  val exe_wire_branch = exe_bj_valid && exe_branch
  val exe_wire_call   = exe_bj_valid && exe_call
  val exe_wire_retn   = exe_bj_valid && exe_retn
  val mem_reg_wbdata  = Reg(Vec(2, UInt(conf.xprlen.W)))

  for (i <- 0 until 2) {
    exe_wire(i).rf_wen  := exe_valid(i) && exe(i).rf_wen && exe(i).wbaddr =/= 0.U
    exe_wire(i).mem_en  := exe_valid(i) && exe(i).mem_en
    exe_wire(i).csr_cmd := Mux(exe_valid(i), exe(i).csr_cmd, CSR.N)
    exe_wire(i).br_type := Mux(exe_valid(i), exe(i).br_type, BR_N)
    exe_wire(i).bj_sel  := exe_valid(i) && exe(i).bj_sel
    exe_wire(i).illegal := exe_valid(i) && exe(i).illegal

    alu(i).op1          := exe(i).op1_data
    alu(i).op2          := exe(i).op2_data
    alu(i).pc           := exe(i).pc
    alu(i).rs2_data     := exe(i).rs2_data
    alu(i).ctrl.fun     := exe(i).alu_fun
    alu(i).ctrl.br_type := exe_wire(i).br_type
    alu(i).ctrl.wb_sel  := exe(i).wb_sel

    exe_wbdata(i)       := alu(i).result

    // vector
    valu(i).op1         := exe(i).vop1_data
    valu(i).op2         := exe(i).vop2_data
    valu(i).fun         := exe(i).alu_fun
    valu(i).sew         := exe(i).vsew

    vacc(i).op1         := valu(i).result
    vacc(i).op2         := exe(i).vop3_data
    vacc(i).fun         := ALU_ADD
    vacc(i).sew         := exe(i).vsew

    exe_vwbdata(i)      := Mux(exe(i).vacc, vacc(i).result, valu(i).result)
    // vector end ======

    when (!stall(1)(Stage.MEM)) {
      mem(i).rf_wen   := exe(i).rf_wen
      mem(i).mem_en   := exe(i).mem_en
      mem(i).csr_cmd  := exe(i).csr_cmd
      mem(i).illegal  := exe(i).illegal
      mem(i).pc       := exe(i).pc
      mem(i).inst     := exe(i).inst
      mem(i).wb_sel   := exe(i).wb_sel
      mem(i).wbaddr   := exe(i).wbaddr
      mem(i).rs2_data := exe(i).rs2_data
      mem(i).mem_fcn  := exe(i).mem_fcn
      mem(i).mem_typ  := exe(i).mem_typ
      mem_reg_wbdata(i) := exe_wbdata(i)

      // vector
      mem(i).v_inst    := exe(i).v_inst
      mem(i).vwbaddr   := exe(i).vwbaddr
      mem(i).vrf_wen   := exe(i).vrf_wen
      mem(i).vmask     := exe(i).vmask
      mem(i).vl        := exe(i).vl
      mem(i).vfinish   := exe(i).vfinish
      mem(i).vwbdata   := exe_vwbdata(i)

      // debug
      // when (exe(i).vfinish) {
      //   printf("exe(%d) DASM(%x) addr %d\n", i.U(2.W), exe(i).inst, exe(i).vwbaddr)
      //   printf("%x\n%x\n%x\n", exe(i).vop1_data, exe(i).vop2_data, exe(i).vop3_data)
      // }
    }
  }

  val target = Mux(exe_wire(0).bj_sel, alu(0).target, alu(1).target)
  val pc_sel = Mux(exe_wire(0).bj_sel, alu(0).ctrl.pc_sel, alu(1).ctrl.pc_sel)

  io.front.ras_pop := Pulse(exe_wire_retn, !stall(1)(Stage.MEM))
  io.front.ras_push.valid := Pulse(exe_wire_call, !stall(1)(Stage.MEM))
  io.front.ras_push.bits  := target.conti
  io.front.feedBack.valid := Pulse(exe_bj_valid, !stall(1)(Stage.MEM))
  io.front.feedBack.redirect := pc_sel === PC_BRJMP || pc_sel === PC_JALR
  io.front.feedBack.tgt :=
    Mux(pc_sel === PC_BRJMP, target.brjmp,
    Mux(pc_sel === PC_JALR,  target.jpreg,
      target.conti
    ))
  io.front.feedBack.history  := exe_btb.history
  io.front.feedBack.diff     := exe_btb.diff
  io.front.fb_pc := Mux(exe_wire(0).bj_sel, exe(0).pc, exe(1).pc)
  io.front.fb_type :=  Mux(exe_wire_branch, BTBType.branch.U,
    Mux(exe_wire_retn, BTBType.retn.U, BTBType.jump.U))

  val not_expect: Bool = io.front.feedBack.tgt =/= exe_btb.tgt
  mispredict := not_expect && exe_bj_valid
  exe_cancel := not_expect && exe_wire(0).bj_sel

  val mem_reg_jpnpc  = RegInit(0.U(conf.xprlen.W))
  when (!stall(1)(Stage.MEM)) {
    mem_reg_jpnpc :=
      (Fill(conf.xprlen, pc_sel === PC_BRJMP) & target.brjmp) |
      (Fill(conf.xprlen, pc_sel === PC_JALR)  & target.jpreg)
  }

  when ((stall(0)(Stage.EXE) || stall(1)(Stage.MEM)) && !stall(0)(Stage.MEM) || xcpt.valid) {
    mem_valid(0) := false.B
  }.elsewhen (!stall(1)(Stage.MEM)) {
    mem_valid(0) := exe_valid(0)
  }

  when ((stall(1)(Stage.EXE) && !stall(1)(Stage.MEM)) || xcpt.valid) {
    mem_valid(1) := false.B
  }.elsewhen (!stall(1)(Stage.MEM)) {
    mem_valid(1) := exe_valid(1) && !exe_cancel
  }

  // debug
  // printf("EXE exe_valid(0) %x exe_valid(1) %x\n", exe_valid(0), exe_valid(0))

  // for (i <- 0 until 2) {
  //     when (io.mem.req.valid && exe(i).v_inst) {
  //       printf("exe(%d) mem_en %d v_inst %d data %d\n", i.U(2.W), exe_wire(i).mem_en, exe(i).v_inst, exe(i).vs2_data)
  //     }
  // }

  val req_fire = RegInit(false.B)

  when (io.mem.req.valid && io.mem.req.ready) {
    req_fire := true.B
  }
  .elsewhen (io.mem.resp.valid) {
    req_fire := false.B
  }

  io.mem.req.valid     := (exe_wire(0).mem_en || (exe_wire(1).mem_en && !exe_cancel)) && (!req_fire || io.mem.resp.valid)//&& !ma_store && !ma_load
  io.mem.req.bits.addr := Mux(exe_wire(0).mem_en, exe_wbdata(0), exe_wbdata(1))
  io.mem.req.bits.fcn  := Mux(exe_wire(0).mem_en, exe(0).mem_fcn , exe(1).mem_fcn)
  io.mem.req.bits.typ  := Mux(exe_wire(0).mem_en, exe(0).mem_typ , exe(1).mem_typ)
  io.mem.req.bits.data := MuxCase(0.U, Array(
                            (exe_wire(0).mem_en && !exe(0).v_inst) -> Cat(Fill(480, 0.U), exe(0).rs2_data),
                            (exe_wire(1).mem_en && !exe(1).v_inst) -> Cat(Fill(480, 0.U), exe(1).rs2_data),
                            (exe_wire(0).mem_en && exe(0).v_inst)  -> exe(0).vs2_data,
                            (exe_wire(1).mem_en && exe(1).v_inst)  -> exe(1).vs2_data)
                          )

  // Memory Stage ============================================================================================================================================
  //==========================================================================================================================================================
  val mem_sel = Wire(Vec(2, Bool()))
  val mem_wire_jump = Wire(Vec(2, Bool()))
  for (i <- 0 until 2) {
    mem_wire(i).rf_wen  := mem_valid(i) && mem(i).rf_wen && mem(i).wbaddr =/= 0.U
    mem_wire(i).mem_en  := mem_valid(i) && mem(i).mem_en
    mem_wire(i).csr_cmd := Mux(mem_valid(i), mem(i).csr_cmd, CSR.N)
    mem_wire(i).illegal := mem_valid(i) && mem(i).illegal
    mem_wire_jump(i) := mem_valid(i) && mem_reg_jpnpc(1,0).orR

    mem_sel(i) := mem_wire(i).csr_cmd =/= CSR.N ||
                  mem_wire(i).mem_en  ||
                  mem_wire(i).illegal ||
                  mem_wire_jump(i)

    // resp data
    val resp_datai = io.mem.resp.bits.data
    val mem_data   = MuxCase(resp_datai,Array(
      (mem(i).mem_typ === MT_B) -> Cat(Fill(24,resp_datai(7)),resp_datai(7,0)),
      (mem(i).mem_typ === MT_H) -> Cat(Fill(16,resp_datai(15)),resp_datai(15,0)),
      (mem(i).mem_typ === MT_BU) -> Cat(Fill(24,0.U),resp_datai(7,0)),
      (mem(i).mem_typ === MT_HU) -> Cat(Fill(16,0.U),resp_datai(15,0))
   ))

    // WB Mux
    mem_wbdata(i) := MuxCase(mem_reg_wbdata(i), Array( // default is wb_alu and wb_pc4
      (mem(i).wb_sel === WB_MEM) -> mem_data,
      (mem(i).wb_sel === WB_CSR) -> csr.io.rw.rdata))

    wb_valid(i)  := mem_valid(i)
    wb(i).rf_wen := mem(i).rf_wen
    wb(i).wbaddr := mem(i).wbaddr
    wb_wbdata(i) := mem_wbdata(i)

    // vector
    mem_vwbdata(i) := MuxCase(mem(i).vwbdata, Array( 
      (mem(i).wb_sel === WB_MEM) -> io.mem.resp.bits.data))

    wb(i).vrf_wen := mem(i).vrf_wen
    wb(i).vwbaddr := mem(i).vwbaddr
    wb(i).vmask   := mem(i).vmask
    wb(i).vl      := mem(i).vl
    wb(i).vfinish := mem(i).vfinish
    wb(i).vwbdata := mem_vwbdata(i)

    // debug
    // printf("mem_wire(i).vrf_wen %x wb(i).vwbaddr %x wb_vwbdata(i) %x\ninst = DASM(%x)\n", mem(i).vrf_wen, wb(i).vwbaddr, wb(i).vwbdata, mem(i).inst)
  }

  val mem_inst     = Mux(mem_sel(0), mem(0).inst, mem(1).inst)
  val mem_exe_out  = Mux(mem_sel(0), mem_reg_wbdata(0), mem_reg_wbdata(1))
  val mem_csr_cmd  = Mux(mem_sel(0), mem_wire(0).csr_cmd, mem_wire(1).csr_cmd)
  val mem_pc       = Mux(mem_sel(0), mem(0).pc, mem(1).pc)
  val mem_illegal  = Mux(mem_sel(0), mem_wire(0).illegal, mem_wire(1).illegal)
  val mem_en       = Mux(mem_sel(0), mem_wire(0).mem_en, mem_wire(1).mem_en)
  val mem_fcn      = Mux(mem_sel(0), mem(0).mem_fcn, mem(1).mem_fcn)
  val mem_typ      = Mux(mem_sel(0), mem(0).mem_typ, mem(1).mem_typ)
  val mem_rs2_data = Mux(mem_sel(0), mem(0).rs2_data, mem(1).rs2_data)
  // Control Status Registers
  csr.io := DontCare
  csr.io.rw.addr  := mem_inst(CSR_ADDR_MSB,CSR_ADDR_LSB)
  csr.io.rw.wdata := mem_exe_out
  csr.io.rw.cmd   := mem_csr_cmd
  csr.io.pc       := mem_pc

  val ls_addr_ma_valid = MuxLookup(mem_typ(1,0) ,false.B, Array(
    2.U -> mem_exe_out(0),
    3.U -> mem_exe_out(1,0).orR
  ))

  val ma_load: Bool    = mem_en && mem_fcn === M_XRD && ls_addr_ma_valid
  val ma_store: Bool   = mem_en && mem_fcn === M_XWR && ls_addr_ma_valid
  val ma_jump: Bool    = Mux(mem_sel(0), mem_wire_jump(0) , mem_wire_jump(1))
  val ma_illegal: Bool = Mux(mem_sel(0), mem_wire(0).illegal, mem_wire(1).illegal)
  csr.io.xcpt  := ma_load || ma_store || ma_jump || mem_illegal
  csr.io.cause := MuxCase(0.U, Array(
    ma_jump    -> Causes.misaligned_fetch.U,
    ma_illegal -> Causes.illegal_instruction.U,
    ma_load    -> Causes.misaligned_load.U,
    ma_store   -> Causes.misaligned_store.U
  ))
  csr.io.tval  := MuxCase(0.U, Array(
    ma_jump    -> mem_reg_jpnpc,
    ma_illegal -> mem_inst,
    ma_load    -> mem_exe_out,
    ma_store   -> mem_exe_out
  ))
  xcpt.valid := ma_jump || ma_load || ma_store || ma_illegal || csr.io.eret
  xcpt.bits  := csr.io.evec

  when (stall(0)(Stage.MEM) || (xcpt.valid && mem_sel(0))) {
    wb_valid(0) := false.B
  } .otherwise { wb_valid(0) := mem_valid(0) }

  when (stall(1)(Stage.MEM) || xcpt.valid) {
    wb_valid(1) := false.B
  } .otherwise { wb_valid(1) := mem_valid(1) }

  // debug
  // printf("MEM mem_valid(0) %x mem_valid(1) %x\n", mem_valid(0), mem_valid(1))


  //===============================================================
  // Writeback Stage ===========================================================================================================================================
  //============================================================================================================================================================
  for (i <- 0 until 2) {
    wb_wire(i).rf_wen   := wb_valid(i) && wb(i).rf_wen && wb(i).wbaddr =/= 0.U
    regfile.io.waddr(i) := wb(i).wbaddr
    regfile.io.wdata(i) := wb_wbdata(i)
    regfile.io.wen(i)   := wb_wire(i).rf_wen

    // vector
    vregfile.io.waddr(i) := wb(i).vwbaddr
    vregfile.io.wen(i)   := wb_valid(i) && wb(i).vrf_wen
    vregfile.io.wdata(i) := wb(i).vwbdata
    vregfile.io.vmask(i) := wb(i).vmask
    vregfile.io.vl(i)    := wb(i).vl
    // debug
    // printf("vregfile.io.wen(i) %x vregfile.io.waddr(i) %x vregfile.io.wdata(i) %x\n", vregfile.io.wen(i), vregfile.io.waddr(i), vregfile.io.wdata(i))
  }

  io.finish := (wb zip wb_valid).map{ case (a, b) => a.vfinish & b }.reduce(_||_)
  
//  when (io.cyc === 5070.U) { regfile.io.wdata(0) := "h00001631".U }
//  when (io.cyc === 10037.U) { regfile.io.wdata(0) := "h00002ad8".U }
//  when (io.cyc === 5102.U) { regfile.io.wdata(0) := "h000010cc".U }
//  when (io.cyc === 10048.U) { regfile.io.wdata(0) := "h0000210a".U }

  val retire = Wire(Vec(2, Bool()))
  retire(0) := wb_valid.asUInt.xorR
  retire(1) := wb_valid.asUInt.andR
  csr.io.retire := retire.asUInt //FIXME
  // Add your own uarch counters here!
  csr.io.counters.foreach(_.inc := false.B)
  //control pipeline signals====================================================================================================================================
  //============================================================================================================================================================
  val exe_load_inst = Wire(Vec(2, Bool()))
  val rs1_addr_N0   = Wire(Vec(2, Bool()))
  val rs2_addr_N0   = Wire(Vec(2, Bool()))
  val rAW           = Wire(Vec(2, Bool()))
  for (i <- 0 until 2) {
    exe_load_inst(i) := exe_wire(i).mem_en && (exe(i).mem_fcn === M_XRD || exe(i).mem_fcn === M_VRD)
    rs1_addr_N0(i)   := dec(i).rs1_addr =/= 0.U
    rs2_addr_N0(i)   := dec(i).rs2_addr =/= 0.U
    rAW(i) :=
      ((exe(0).wbaddr === dec(i).rs1_addr || exe(0).vwbaddr === dec(i).vs1_addr) && dec_wire(i).rs1_oen && exe_load_inst(0)) ||
      ((exe(0).wbaddr === dec(i).rs2_addr || exe(0).vwbaddr === dec(i).vs2_addr) && dec_wire(i).rs2_oen && exe_load_inst(0)) ||
      ((exe(1).wbaddr === dec(i).rs1_addr || exe(1).vwbaddr === dec(i).vs1_addr) && dec_wire(i).rs1_oen && exe_load_inst(1)) ||
      ((exe(1).wbaddr === dec(i).rs2_addr || exe(1).vwbaddr === dec(i).vs2_addr) && dec_wire(i).rs2_oen && exe_load_inst(1))
  }

  stall(0)(Stage.DEC) := (rAW(0) || exe_wire(0).csr_cmd =/= CSR.N || exe_wire(1).csr_cmd =/= CSR.N)

  stall(1)(Stage.DEC) :=
    (((dec(0).wbaddr === dec(1).rs1_addr || dec(0).vwbaddr === dec(1).vs1_addr) && dec_wire(1).rs1_oen && (dec_wire(0).rf_wen || dec_wire(0).vrf_wen)) ||
    (( dec(0).wbaddr === dec(1).rs2_addr || dec(0).vwbaddr === dec(1).vs2_addr) && dec_wire(1).rs2_oen && (dec_wire(0).rf_wen || dec_wire(0).vrf_wen)) ||
    dec_wire(0).csr_cmd =/= CSR.N || rAW(1) || stall(0)(Stage.DEC))

  stall(0)(Stage.EXE) := exe_wire(0).mem_en && !io.mem.req.ready
  stall(1)(Stage.EXE) := stall(0)(Stage.EXE) || exe_wire.map(_.mem_en).reduce(_&&_) || exe_wire(1).mem_en && !io.mem.req.ready

  stall(0)(Stage.MEM) := mem_wire(0).mem_en && !io.mem.resp.valid
  stall(1)(Stage.MEM) := mem_sel.asUInt.andR || (mem_wire(1).mem_en && !io.mem.resp.valid) || stall(0)(Stage.MEM)

  io.front.kill := Pulse(mispredict, forward = !stall(1)(Stage.MEM))
  io.front.xcpt := xcpt
  io.front.forward(0) := !stall(0)(Stage.DEC) && !stall(1)(Stage.EXE) && !stall(1)(Stage.MEM)
  io.front.forward(1) := !stall(1)(Stage.DEC) && !stall(1)(Stage.EXE) && !stall(1)(Stage.MEM)

  // for (i <- 0 until conf.nInst) {
  //   when (exe_valid(i)) {
  //     printf("Core: Cyc= %d PC(%x, %x, %x) [%c%c %c%c %c%c] %c%c %c%c Exe: DASM(%x)\n"
  //       , io.cyc
  //       , io.front.pc(i)
  //       , exe(i).pc
  //       , mem(i).pc
  //       , Mux(stall(i)(Stage.MEM), Str("M"), Str(" ")), Str(""+i)
  //       , Mux(stall(i)(Stage.EXE), Str("E"), Str(" ")), Str(""+i)
  //       , Mux(stall(i)(Stage.DEC), Str("D"), Str(" ")), Str(""+i)
  //       , Mux(alu(i).ctrl.pc_sel === 1.U, Str("B"),    //BJ -> B
  //         Mux(alu(i).ctrl.pc_sel === 2.U, Str("J"),    //JR -> J
  //         Mux(alu(i).ctrl.pc_sel === 3.U, Str("E"),    //EX -> E
  //         Mux(alu(i).ctrl.pc_sel === 0.U, Str(" "), Str("?"))))), Str(""+i)
  //       , Mux(csr.io.illegal, Str("X"), Str(" ")), Str(""+i)
  //       , Mux(!exe_valid(i) || xcpt.valid || (exe_cancel && i.U === 1.U) ||
  //         stall(1)(Stage.MEM) || (stall(1)(Stage.EXE) && i.U === 1.U), BUBBLE, exe(i).inst)
  //     )
  //   }

  // }
    
  for (i <- 0 until conf.nInst) {
    when (exe_valid(i)) {
      printf("Cyc= %d Exe: PC(%x) DASM(%x)\n"
        , io.cyc(19, 0)
        
        , exe(i).pc
        
        
        , Mux(!exe_valid(i) || xcpt.valid || (exe_cancel && i.U === 1.U) ||
          stall(1)(Stage.MEM) || (stall(1)(Stage.EXE) && i.U === 1.U), BUBBLE, exe(i).inst)
      )
    }
  }
}