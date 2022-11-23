package fuxi

import chisel3._
import chisel3.util._
import common.{CPUConfig, CSR}
import common.Instructions._

class CtrlInfo extends Bundle {
  val br_type = UInt(BR_N.getWidth.W)
  val op1_sel = UInt(OP1_X.getWidth.W)
  val op2_sel = UInt(OP2_X.getWidth.W)
  val rs1_oen = Bool()
  val rs2_oen = Bool()
  val alu_fun = UInt(ALU_X.getWidth.W)
  val wb_sel  = UInt(WB_X.getWidth.W)
  val rf_wen  = Bool()
  val mem_en  = Bool()
  val mem_fcn = UInt(M_X.getWidth.W)
  val mem_typ = UInt(MT_X.getWidth.W)
  val csr_cmd = UInt(CSR.N.getWidth.W)
  val illegal = Bool()
  val fencei  = Bool()
  // vector
  val v_inst  = Bool()
  val vrf_wen = Bool()
  val vmask   = Bool()
  val vacc    = Bool()
  val vfinish = Bool()
}

class DataInfo(val data_width: Int) extends Bundle {
  val imm_i    = UInt(data_width.W)
  val imm_s    = UInt(data_width.W)
  val imm_sb   = UInt(data_width.W)
  val imm_u    = UInt(data_width.W)
  val imm_uj   = UInt(data_width.W)
  val imm_z    = UInt(data_width.W)
}

class InstDecoder(implicit conf: CPUConfig) extends Module {
  val io = IO(new Bundle {
    val inst  = Input(UInt(conf.xprlen.W))
    val cinfo = Output(new CtrlInfo())
    val dinfo = Output(new DataInfo(conf.xprlen))
    val rs1_addr = Output(UInt(5.W))
    val rs2_addr = Output(UInt(5.W))
    val wbaddr   = Output(UInt(5.W))
    // vector
    val vs1_addr = Output(UInt(5.W))
    val vs2_addr = Output(UInt(5.W))
    val vwbaddr  = Output(UInt(5.W))   
  })

  val signals =
    ListLookup(io.inst,
                 List(N, BR_N  , OP1_X , OP2_X    , OEN_0, OEN_0, ALU_X   , WB_X  ,  REN_0, MEN_0, M_X  , MT_X, CSR.N, N,       VINS_N, N,     N),
      Array(   /* val  |  BR  |  op1  |   op2     |  R1  |  R2  |  ALU    |  wb   | rf   | mem  | mem  | mask | csr | fence.i | vector | acc | finish*/
               /* inst | type |   sel |    sel    |  oen |  oen |   fcn   |  sel  | wen  |  en  |  wr  | type | cmd |         |  inst  |     |       */
        LW     -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_MEM, REN_1, MEN_1, M_XRD, MT_W, CSR.N, N,     VINS_N, N, N),
        LB     -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_MEM, REN_1, MEN_1, M_XRD, MT_B, CSR.N, N,     VINS_N, N, N),
        LBU    -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_MEM, REN_1, MEN_1, M_XRD, MT_BU,CSR.N, N,     VINS_N, N, N),
        LH     -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_MEM, REN_1, MEN_1, M_XRD, MT_H, CSR.N, N,     VINS_N, N, N),
        LHU    -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_MEM, REN_1, MEN_1, M_XRD, MT_HU,CSR.N, N,     VINS_N, N, N),
        SW     -> List(Y, BR_N  , OP1_RS1, OP2_STYPE , OEN_1, OEN_1, ALU_ADD , WB_X  , REN_0, MEN_1, M_XWR, MT_W, CSR.N, N,     VINS_N, N, N),
        SB     -> List(Y, BR_N  , OP1_RS1, OP2_STYPE , OEN_1, OEN_1, ALU_ADD , WB_X  , REN_0, MEN_1, M_XWR, MT_B, CSR.N, N,     VINS_N, N, N),
        SH     -> List(Y, BR_N  , OP1_RS1, OP2_STYPE , OEN_1, OEN_1, ALU_ADD , WB_X  , REN_0, MEN_1, M_XWR, MT_H, CSR.N, N,     VINS_N, N, N),

        AUIPC  -> List(Y, BR_N  , OP1_PC , OP2_UTYPE , OEN_0, OEN_0, ALU_ADD   ,WB_ALU,REN_1, MEN_0, M_X , MT_X,  CSR.N, N,     VINS_N, N, N),
        LUI    -> List(Y, BR_N  , OP1_X  , OP2_UTYPE , OEN_0, OEN_0, ALU_COPY_2,WB_ALU,REN_1, MEN_0, M_X , MT_X,  CSR.N, N,     VINS_N, N, N),

        ADDI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N,     VINS_N, N, N),
        ANDI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_AND , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N,     VINS_N, N, N),
        ORI    -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_OR  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N,     VINS_N, N, N),
        XORI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_XOR , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N,     VINS_N, N, N),
        SLTI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_SLT , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N,     VINS_N, N, N),
        SLTIU  -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_SLTU, WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N,     VINS_N, N, N),
        SLLI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_SLL , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N,     VINS_N, N, N),
        SRAI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_SRA , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N,     VINS_N, N, N),
        SRLI   -> List(Y, BR_N  , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_SRL , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N,     VINS_N, N, N),

        SLL    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SLL , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N,     VINS_N, N, N),
        ADD    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_ADD , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N,     VINS_N, N, N),
        SUB    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SUB , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N,     VINS_N, N, N),
        SLT    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SLT , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N,     VINS_N, N, N),
        SLTU   -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SLTU, WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N,     VINS_N, N, N),
        AND    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_AND , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N,     VINS_N, N, N),
        OR     -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_OR  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N,     VINS_N, N, N),
        XOR    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_XOR , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N,     VINS_N, N, N),
        SRA    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SRA , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N,     VINS_N, N, N),
        SRL    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_SRL , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N,     VINS_N, N, N),

        JAL    -> List(Y, BR_J  , OP1_RS1, OP2_UJTYPE, OEN_0, OEN_0, ALU_X   , WB_PC4, REN_1, MEN_0, M_X  , MT_X, CSR.N, N,     VINS_N, N, N),
        JALR   -> List(Y, BR_JR , OP1_RS1, OP2_ITYPE , OEN_1, OEN_0, ALU_ADD , WB_PC4, REN_1, MEN_0, M_X  , MT_X, CSR.N, N,     VINS_N, N, N),
        BEQ    -> List(Y, BR_EQ , OP1_RS1, OP2_SBTYPE, OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N,     VINS_N, N, N),
        BNE    -> List(Y, BR_NE , OP1_RS1, OP2_SBTYPE, OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N,     VINS_N, N, N),
        BGE    -> List(Y, BR_GE , OP1_RS1, OP2_SBTYPE, OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N,     VINS_N, N, N),
        BGEU   -> List(Y, BR_GEU, OP1_RS1, OP2_SBTYPE, OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N,     VINS_N, N, N),
        BLT    -> List(Y, BR_LT , OP1_RS1, OP2_SBTYPE, OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N,     VINS_N, N, N),
        BLTU   -> List(Y, BR_LTU, OP1_RS1, OP2_SBTYPE, OEN_1, OEN_1, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N,     VINS_N, N, N),

        CSRRWI -> List(Y, BR_N  , OP1_IMZ, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.W, N,     VINS_N, N, N),
        CSRRSI -> List(Y, BR_N  , OP1_IMZ, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.S, N,     VINS_N, N, N),
        CSRRW  -> List(Y, BR_N  , OP1_RS1, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.W, N,     VINS_N, N, N),
        CSRRS  -> List(Y, BR_N  , OP1_RS1, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.S, N,     VINS_N, N, N),
        CSRRC  -> List(Y, BR_N  , OP1_RS1, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.C, N,     VINS_N, N, N),
        CSRRCI -> List(Y, BR_N  , OP1_IMZ, OP2_X     , OEN_1, OEN_1, ALU_COPY_1,WB_CSR,REN_1, MEN_0, M_X  , MT_X, CSR.C, N,     VINS_N, N, N),

        ECALL  -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.I, N,     VINS_N, N, N),
        MRET   -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.I, N,     VINS_N, N, N),
        DRET   -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.I, N,     VINS_N, N, N),
        EBREAK -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.I, N,     VINS_N, N, N),
        WFI    -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, N,     VINS_N, N, N), // implemented as a NOP

        NOP    -> List(Y, BR_N  , OP1_RS1, OP2_RS2   , OEN_1, OEN_1, ALU_ADD , WB_ALU, REN_1, MEN_0, M_X  , MT_X, CSR.N, N,     VINS_N, N, N),
        
        FENCE_I-> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_0, M_X  , MT_X, CSR.N, Y,     VINS_N, N, N),
        // kill pipeline and refetch instructions since the pipeline will be holding stall instructions.
        FENCE  -> List(Y, BR_N  , OP1_X  , OP2_X     , OEN_0, OEN_0, ALU_X   , WB_X  , REN_0, MEN_1, M_X  , MT_X, CSR.N, N,     VINS_N, N, N), // implemented
        // we are already sequentially consistent, so no need to honor the fence instruction

        // vector instructions
                      /* val  |  BR  |  op1  |   op2     |  R1  |  R2  |  ALU    |  wb   | rf   | mem  | mem  | mask | csr | fence.i | vector | acc |*/
                      /* inst | type |   sel |    sel    |  oen |  oen |   fcn   |  sel  | wen  |  en  |  wr  | type | cmd |         |  inst  | acc |*/
        VADD_VV  -> List(Y, BR_N, OP1_VS2, OP2_VS1,   OEN_1,  OEN_1, ALU_ADD,  WB_ALU, REN_1,  MEN_0,  M_X,   MT_X,  CSR.N, N, VINS_Y, N, N),
        VSUB_VV  -> List(Y, BR_N, OP1_VS2, OP2_VS1,   OEN_1,  OEN_1, ALU_SUB,  WB_ALU, REN_1,  MEN_0,  M_X,   MT_X,  CSR.N, N, VINS_Y, N, N),
        VMUL_VV  -> List(Y, BR_N, OP1_VS2, OP2_VS1,   OEN_1,  OEN_1, ALU_MUL,  WB_ALU, REN_1,  MEN_0,  M_X,   MT_X,  CSR.N, N, VINS_Y, N, N),
        VMACC_VV -> List(Y, BR_N, OP1_VS2, OP2_VS1,   OEN_1,  OEN_1, ALU_MUL,  WB_ALU, REN_1,  MEN_0,  M_X,   MT_X,  CSR.N, N, VINS_Y, Y, N),
        
        VAND_VV  -> List(Y, BR_N, OP1_VS2, OP2_VS1,   OEN_1,  OEN_1, ALU_AND,  WB_ALU, REN_1,  MEN_0,  M_X,   MT_X,  CSR.N, N, VINS_Y, N, N),
        VOR_VV   -> List(Y, BR_N, OP1_VS2, OP2_VS1,   OEN_1,  OEN_1, ALU_OR,   WB_ALU, REN_1,  MEN_0,  M_X,   MT_X,  CSR.N, N, VINS_Y, N, N),
        VXOR_VV  -> List(Y, BR_N, OP1_VS2, OP2_VS1,   OEN_1,  OEN_1, ALU_XOR,  WB_ALU, REN_1,  MEN_0,  M_X,   MT_X,  CSR.N, N, VINS_Y, N, N),
        VMAX_VV  -> List(Y, BR_N, OP1_VS2, OP2_VS1,   OEN_1,  OEN_1, ALU_MAX,  WB_ALU, REN_1,  MEN_0,  M_X,   MT_X,  CSR.N, N, VINS_Y, N, N),
        VMIN_VV  -> List(Y, BR_N, OP1_VS2, OP2_VS1,   OEN_1,  OEN_1, ALU_MIN,  WB_ALU, REN_1,  MEN_0,  M_X,   MT_X,  CSR.N, N, VINS_Y, N, N),
        VMAXU_VV -> List(Y, BR_N, OP1_VS2, OP2_VS1,   OEN_1,  OEN_1, ALU_MAXU, WB_ALU, REN_1,  MEN_0,  M_X,   MT_X,  CSR.N, N, VINS_Y, N, N),
        VMINU_VV -> List(Y, BR_N, OP1_VS2, OP2_VS1,   OEN_1,  OEN_1, ALU_MINU, WB_ALU, REN_1,  MEN_0,  M_X,   MT_X,  CSR.N, N, VINS_Y, N, N),

        VADD_VX  -> List(Y, BR_N, OP1_VS2, OP2_RS1,   OEN_1,  OEN_1, ALU_ADD , WB_ALU, REN_1,  MEN_0,  M_X,   MT_X,  CSR.N, N, VINS_Y, N, N),
        VSUB_VX  -> List(Y, BR_N, OP1_VS2, OP2_RS1,   OEN_1,  OEN_1, ALU_SUB , WB_ALU, REN_1,  MEN_0,  M_X,   MT_X,  CSR.N, N, VINS_Y, N, N),
        VRSUB_VX -> List(Y, BR_N, OP1_VS2, OP2_RS1,   OEN_1,  OEN_1, ALU_ADD , WB_ALU, REN_1,  MEN_0,  M_X,   MT_X,  CSR.N, N, VINS_Y, N, N),

        VLE8_V   -> List(Y, BR_N, OP1_RS1, OP2_X,     OEN_1,  OEN_0, ALU_ADD,  WB_MEM, REN_1,  MEN_1,  M_VRD, MT_B,  CSR.N, N, VINS_Y, N, N),
        VLE16_V  -> List(Y, BR_N, OP1_RS1, OP2_X,     OEN_1,  OEN_0, ALU_ADD,  WB_MEM, REN_1,  MEN_1,  M_VRD, MT_H,  CSR.N, N, VINS_Y, N, N),
        VLE32_V  -> List(Y, BR_N, OP1_RS1, OP2_X,     OEN_1,  OEN_0, ALU_ADD,  WB_MEM, REN_1,  MEN_1,  M_VRD, MT_W,  CSR.N, N, VINS_Y, N, N),
        VLE64_V  -> List(Y, BR_N, OP1_RS1, OP2_X,     OEN_1,  OEN_0, ALU_ADD,  WB_MEM, REN_1,  MEN_1,  M_VRD, MT_64, CSR.N, N, VINS_Y, N, N),

        VSE8_V   -> List(Y, BR_N, OP1_RS1, OP2_VD,    OEN_1,  OEN_1, ALU_ADD,  WB_MEM, REN_0,  MEN_1,  M_VWR, MT_B,  CSR.N, N, VINS_Y, N, N),
        VSE16_V  -> List(Y, BR_N, OP1_RS1, OP2_VD,    OEN_1,  OEN_1, ALU_ADD,  WB_MEM, REN_0,  MEN_1,  M_VWR, MT_H,  CSR.N, N, VINS_Y, N, N),
        VSE32_V  -> List(Y, BR_N, OP1_RS1, OP2_VD,    OEN_1,  OEN_1, ALU_ADD,  WB_MEM, REN_0,  MEN_1,  M_VWR, MT_W,  CSR.N, N, VINS_Y, N, N),
        VSE64_V  -> List(Y, BR_N, OP1_RS1, OP2_VD,    OEN_1,  OEN_1, ALU_ADD,  WB_MEM, REN_0,  MEN_1,  M_VWR, MT_64, CSR.N, N, VINS_Y, N, N),

        VFINISH  -> List(Y, BR_N, OP1_X,   OP2_X,     OEN_0,  OEN_0, ALU_X,    WB_X,   REN_0,  MEN_0,  M_X,   MT_X,  CSR.N, N, VINS_Y, N, Y),

        VMV_S_X  -> List(Y, BR_N, OP1_RS1, OP2_X,     OEN_1,  OEN_0, ALU_COPY_1, WB_ALU, REN_1,  MEN_0,  M_X,   MT_X,  CSR.N, N, VINS_Y, N, N),

        // 下面两条的确属于向量指令，但是还是将其vector ins置为false以保证其能使用标量的写回通路
        VSETVLI  -> List(Y, BR_N, OP1_RS1, OP2_ITYPE, OEN_1,  OEN_0, ALU_COPY_1, WB_CSR, REN_0, MEN_0, M_X,   MT_X,  CSR.VLI, N, VINS_N, N, N),
        VSETVL   -> List(Y, BR_N, OP1_RS1, OP2_RS2,   OEN_1,  OEN_1, ALU_COPY_1, WB_CSR, REN_0, MEN_0, M_X,   MT_X,  CSR.VL , N, VINS_N, N, N),


                  
      ))

  // Put these control signals in variables
  val (val_inst: Bool) :: br_type :: op1_sel :: op2_sel :: (rs1_oen: Bool) :: (rs2_oen: Bool) :: sig = signals
  val alu_fun :: wb_sel :: (rf_wen: Bool) :: (mem_en: Bool) :: mem_fcn :: mem_typ :: csr_cmd :: (fencei: Bool) :: (v_inst: Bool) :: (vacc: Bool) :: (vfinish: Bool) :: Nil = sig

  io.cinfo.br_type := br_type
  io.cinfo.op1_sel := op1_sel
  io.cinfo.op2_sel := op2_sel
  io.cinfo.rs1_oen := rs1_oen
  io.cinfo.rs2_oen := rs2_oen
  io.cinfo.alu_fun := alu_fun
  io.cinfo.wb_sel  := wb_sel
  io.cinfo.rf_wen  := rf_wen && !v_inst
  io.cinfo.mem_en  := mem_en
  io.cinfo.mem_fcn := mem_fcn
  io.cinfo.mem_typ := mem_typ
  io.cinfo.csr_cmd := csr_cmd
  io.cinfo.illegal := !val_inst //illegal instruction
  io.cinfo.fencei  := fencei

  // vector
  io.cinfo.v_inst  := v_inst
  io.cinfo.vrf_wen := rf_wen && v_inst
  io.cinfo.vmask   := io.inst(VMASK).asBool()
  io.cinfo.vacc    := vacc
  io.cinfo.vfinish := vfinish

  io.rs1_addr := io.inst(RS1_MSB, RS1_LSB)
  io.rs2_addr := io.inst(RS2_MSB, RS2_LSB)
  io.wbaddr   := io.inst(RD_MSB , RD_LSB)

  io.vs1_addr := io.inst(RS1_MSB, RS1_LSB)  // 15 - 19
  io.vs2_addr := Mux( op2_sel === OP2_VD, io.inst(RD_MSB , RD_LSB), io.inst(RS2_MSB, RS2_LSB) ) // 20 - 24
  io.vwbaddr  := io.inst(RD_MSB , RD_LSB)   // 7 - 11
  
  // vector end ==========
  
  // immediates
  val imm_itype  = io.inst(31,20)
  val imm_stype  = Cat(io.inst(31,25), io.inst(11,7))
  val imm_sbtype = Cat(io.inst(31), io.inst(7), io.inst(30, 25), io.inst(11,8))
  val imm_utype  = io.inst(31, 12)
  val imm_ujtype = Cat(io.inst(31), io.inst(19,12), io.inst(20), io.inst(30,21))

  // sign-extend immediates
  io.dinfo.imm_i  := Cat(Fill(20,imm_itype(11)), imm_itype)
  io.dinfo.imm_s  := Cat(Fill(20,imm_stype(11)), imm_stype)
  io.dinfo.imm_sb := Cat(Fill(19,imm_sbtype(11)), imm_sbtype, 0.U)
  io.dinfo.imm_u  := Cat(imm_utype, Fill(12,0.U))
  io.dinfo.imm_uj := Cat(Fill(11,imm_ujtype(19)), imm_ujtype, 0.U)
  io.dinfo.imm_z  := Cat(Fill(27,0.U), io.inst(19,15))

}
