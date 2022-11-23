package boom.acc

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import boom.exu.ygjk._

class Withacc_LZ extends Config((site,here,up) => {  
    case BuildYGAC => 
        (p:Parameters) => {          
            val myAccel = Module(new LZAcc)
            myAccel
        }
    }
)

trait LZpara {
  val N = 4096
  val L_distance = 12
  val M = 7
  val L_length   = 5
  val LZword = 18
  val LogWord = 5
}

class LZAcc extends MyACCModule with LZpara{

  //*宏定义
  val matchMax  = (1<<L_length)+1
  val launch    = 123
  //*
  
  //* 子模块 实例化
  val ReadBF  = Module(new ReadBuff)
  val WriteBF = Module(new WriteBuff)
  val Cmp     = Module(new LZcmp)
  //*

  //* Wire 定义
  val epoch  = Wire(Bool()) // new task
  val shift  = Wire(Bool()) // whether need move data
  val done   = Wire(Bool()) // whether current cmp finish
  val runout = Wire(Bool()) //input runout
  val word0  = Wire(Vec(LZword,Bool())) // not match
  val word1  = Wire(Vec(LZword,Bool())) // 1 byte
  val word2  = Wire(Vec(LZword,Bool())) // 2 bytes
  val word3  = Wire(Vec(LZword,Bool())) // more than 3 bytes
  val match_bytes = Wire(UInt(L_length.W)) // for encoding only
  val match_index = Wire(UInt(L_distance.W)) //matched site
  val L_ans  = Wire(UInt(LogWord.W)) //length written to window
  val round_end   = Wire(Bool())
  val drain  = Wire(Bool()) // input finish,ready to drain out WriteBF
  val query_ans = Wire(Vec(32,Bool()))
  //*

  //* Reg 定义 
  val begin   = Reg(Bool()) // new round
  val source  = Reg(UInt(64.W)) //the length of input
  val passed  = Reg(UInt(64.W)) //the length of has been processed 
  val last1   = Reg(UInt(8.W)) 
  val last2   = Reg(UInt(8.W))
  val run_cyc = Reg(UInt(32.W)) // Elapsed time
  //*

  //* 数据输入
  io.sdma.read1_req.ready := false.B
  ReadBF.io.epoch   := epoch
  ReadBF.io.in      := io.sdma.read0_req.bits.asUInt()
  ReadBF.io.DValid  := io.sdma.read0_req.valid
  ReadBF.io.runout  := runout 
  ReadBF.io.shift   := shift
  io.sdma.read0_req.ready := ReadBF.io.DReady
  //*

  io.query.rd := query_ans.asUInt() //?
  //* Wire 连接
  epoch := false.B
  shift := false.B
  done  := false.B
  runout:= false.B
  drain := false.B
  round_end := !begin && done
  word0(0) := 0.U
  for(i <- 1 to 8){
    word0(i) := ReadBF.io.out(i-1)
  }
  for(i <- 9 to 17){
    word0(i) := 0.U
  }
  word1(0) := 0.U
  for(i <- 1 to 8){
    word1(i) := last1(i-1)
  }
  for(i <- 9 to 17){
    word1(i) := 0.U
  }
  word2(0) := 0.U
  for(i <- 1 to 8){
    word2(i) := last2(i-1)
  }
  word2(9) := 0.U
  for(i <- 10 to 17){
    word2(i) := last1(i-10)
  }
  word3(0) := 1.U
  for(i <- 1 to L_length){
    word3(i) := match_bytes(i-1)
  }
  for(i <- L_length+1 to LZword-1){
    word3(i) := match_index(i-L_length-1)
  }
  match_bytes := Cmp.io.L_match - 2.U
  match_index := Cmp.io.site
  L_ans := 0.U
  query_ans(0) := WriteBF.io.finish
  query_ans(1) := 0.U
  for(i <- 2 to 31){
    query_ans(i) := run_cyc(i)
  }
  //*

  //* 状态机定义
  val s_idle :: s_epoch :: s_run :: s_drian0:: s_drian1 :: s_finish ::Nil = Enum(6)
  val sta  = RegInit(s_idle)
  switch(sta){
    is(s_idle){
      when(io.config.valid && io.config.bits.cfgData1 === launch.U){
        passed := 0.U
        source := io.config.bits.cfgData2
        sta := s_epoch
      }
    }
    is(s_epoch){
      run_cyc := 0.U
      epoch := true.B
      begin := true.B
      sta   := s_run
    }
    is(s_run){
      when(ReadBF.io.valid && !WriteBF.io.busy && !round_end){
        shift := true.B
        passed := passed + 1.U
      }
      when(Cmp.io.done || Cmp.io.L_match>=matchMax.U || passed===(source-1.U)){
        done := true.B
      }
      when(shift){
        last1 := ReadBF.io.out
        last2 := last1
      }
      when(ReadBF.io.valid && !WriteBF.io.busy && done && passed=/=source){
        L_ans := Mux(Cmp.io.L_match>1.U,18.U,9.U)
      }
      when(ReadBF.io.valid && !WriteBF.io.busy && done){
        begin := true.B
      }.elsewhen(ReadBF.io.valid && !WriteBF.io.busy){
        begin := false.B
      }

      when(passed===source){
        runout := true.B
        word0(0) := 1.U
        for(i <- 1 to LogWord){
          word0(i) := 0.U
        }
        when(!WriteBF.io.busy){
          L_ans := (LogWord+1).U
          sta   := s_drian0
        }
      }
      run_cyc := run_cyc + 1.U
    }
    is(s_drian0){
      sta := s_drian1
    }
    is(s_drian1){
      drain := true.B
      when(WriteBF.io.finish){
        sta := s_finish
      }
      run_cyc := run_cyc + 1.U
    }
    is(s_finish){
        when(io.config.valid && io.config.bits.cfgData1 === launch.U){
          passed := 0.U
          source := io.config.bits.cfgData2
          sta := s_epoch
      }
    }
  }
  //*

  //* LZcmp 核心模块
  Cmp.io.data   := ReadBF.io.out
  Cmp.io.shift  := shift
  Cmp.io.epoch  := epoch
  Cmp.io.begin  := begin
  //*

  //* 数据输出
  val WBF_in = Wire(Vec(LZword,Bool()))
  for(i <- 0 to 17){
    when(begin || runout){
      WBF_in(i) := word0(i)
    }.elsewhen(Cmp.io.L_match===1.U){
      WBF_in(i) := word1(i)
    }.elsewhen(Cmp.io.L_match===2.U){
      WBF_in(i) := word2(i)
    }.otherwise{
      WBF_in(i) := word3(i)
    }
  }
  for(i <- 0 to LZword-1){
    WriteBF.io.in(i) := WBF_in(i) 
  }
  WriteBF.io.L_in   := L_ans 
  WriteBF.io.epoch  := epoch
  WriteBF.io.drain := drain 
  WriteBF.io.ready  := io.sdma.write_req.ready
  val WBFans = Wire(UInt(512.W))
  WBFans := WriteBF.io.out.asUInt()
  for(i <- 0 to 15){
    io.sdma.write_req.bits(i) := WBFans(i*32+31,i*32)
  }
  io.sdma.write_req.valid := WriteBF.io.valid
  //*
  //?
  when(ReadBF.io.valid && !WriteBF.io.busy){
    printf(p"ReadBF.out: ${ReadBF.io.out}\n")
    printf(p"done : ${done}\n")
    printf(p"begin: ${begin}\n")
    printf(p"position: ${Cmp.io.site}\n")
    printf(p"match length: ${Cmp.io.L_match}\n")
    printf(p"L_ans: ${L_ans}\n")
    printf(p"WBF_in: ${WBF_in.asUInt()}\n")
    printf(p"passed: ${passed}\n")
    printf(p"source: ${source}\n")
    printf(p"runout: ${runout}\n")
    printf(p"drain: ${drain}\n")
    printf(p"------------------------------------------------------\n")
  }
  //?
}

//* Data supply and it's submodule
class ReadBuff extends Module{
  val io = IO(new Bundle{
    val epoch   = Input(Bool())
    val in      = Input(UInt(512.W))
    val DValid  = Input(Bool())
    val runout  = Input(Bool())
    val shift   = Input(Bool())
    val out     = Output(UInt(8.W))
    val valid   = Output(Bool())
    val DReady  = Output(Bool())
  })
  val data  = VecInit(Seq.fill(8)(Module(new RDbuffCell).io))
  val update= Wire(Bool())
  val addr  = Wire(UInt(6.W))
  val cnt   = Reg(UInt(6.W))
  val valid = Reg(Bool())
  val DReady= Reg(Bool())

  update := false.B
  for(i <- 0 to 7){
    data(i).in    := io.in(i*64+63,i*64)
    data(i).en    := update
    data(i).addr  := addr(2,0)
  }
  when(io.shift===true.B){
    addr := cnt + 1.U
  }.otherwise{
    addr := cnt
  }

  val s_idle :: s_run :: s_wait :: s_tmp :: Nil = Enum(4)
  val sta  = RegInit(s_idle)

  switch(sta){
    is(s_idle){
      when(io.epoch === true.B){
        sta   := s_run
        valid := false.B
        cnt   := 0.U
      }
    }
    is(s_run){
      when(io.runout===true.B){
        valid := false.B
        sta := s_idle
      }.elsewhen(valid===false.B){
        DReady := true.B 
        sta := s_wait
      }
      
      when(cnt>=63.U && io.shift===true.B){
        valid := false.B
      }
      cnt := addr

    }
    is(s_wait){
      when(io.DValid){
        DReady  := false.B
        update  := true.B
        cnt     := 0.U
        sta     := s_tmp
      }
    }
    is(s_tmp){
      valid   := true.B
      sta := s_run
    }
  }

  val tmp_out = Reg(UInt(8.W))
  tmp_out := data(addr(5,3)).out
  io.out    := tmp_out
  io.valid  := valid && sta===s_run
  io.DReady := DReady
}

class RDbuffCell extends Module{
  val io = IO(new Bundle{
    val in   = Input(UInt(64.W))
    val en   = Input(Bool())
    val addr = Input(UInt(3.W))
    val out  = Output(UInt(8.W)) 
  })
  val data = Reg(Vec(8,UInt(8.W)))
  for(i <- 0 to 7){
    when(io.en===true.B){
      data(i) := io.in(i*8+7,i*8)
    }
  }
  io.out := data(io.addr)
}

//* WriteBack and it's submodule
class WriteBuff extends Module with LZpara{
  val io = IO(new Bundle{
    val in      = Input(Vec(LZword,Bool()))
    val L_in    = Input(UInt(LogWord.W))
    val epoch   = Input(Bool())
    val drain  = Input(Bool())
    val ready   = Input(Bool())
    val out     = Output(Vec(512,Bool()))
    val valid   = Output(Bool())
    val busy    = Output(Bool())
    val finish  = Output(Bool())
  })

  val busy    = Wire(Bool())
  val BuffAry = VecInit(Seq.fill(512+LZword)(Module(new AnsCell).io))
  val vacant  = Reg(UInt(10.W)) // 0 - 512
  val pending = Reg(UInt(LogWord.W)) // 0 - 18
  val step    = Wire(UInt(4.W)) // 0 -10
  val step_in = Wire(UInt(4.W)) //just for window
  val valid   = Reg(Bool())

  val s_idle :: s_run :: s_write :: s_drian :: s_finish :: Nil = Enum(5)
  val sta  = RegInit(s_idle)
  
  step    := 0.U
  busy := false.B
  io.finish  := false.B
  when(busy){
    step_in := step
  }.elsewhen(io.L_in > 0.U){
    step_in := 10.U
  }.otherwise{
    step_in := 0.U
  }
  
  switch(sta){
    is(s_idle){
      when(io.epoch === true.B){
        sta     := s_run
        vacant  := 512.U
        pending := 0.U
        valid   := false.B
      }
    }
    is(s_run){
      
      vacant  := vacant - Mux(step===10.U,LZword.U,step)
      when(busy){
        pending := pending - Mux(step===10.U,LZword.U,step)
      }.otherwise{
        pending := io.L_in
      }

      when(pending>9.U && pending<LZword.U && pending<=vacant){
        busy := true.B
        step := pending - 9.U
      }.elsewhen(pending>0.U && pending<=vacant){
        step := Mux(pending===LZword.U,10.U,pending)
      }.elsewhen(pending>0.U){ 
        step := Mux(vacant>9.U,vacant-9.U,vacant)
        busy := true.B
      }.otherwise{
        step := 0.U
      }

      when(vacant === 0.U){
        sta     := s_write
        busy    := true.B
        valid   := true.B
      }.elsewhen(io.drain === true.B){
        sta := s_drian
      }
    }
    is(s_write){
      busy := true.B
      when(valid && io.ready===true.B){
        vacant := 512.U
        valid  := false.B
        sta    := s_run
      }
    }
    is(s_drian){
      when(valid && io.ready===true.B){
        valid  := false.B
        sta    := s_finish
      }.elsewhen(vacant === 0.U){
        valid  := true.B
      }.elsewhen(vacant === 512.U){
        sta    := s_finish
      }.otherwise{
        step    := Mux(vacant>=9.U,9.U,vacant)
        vacant  := vacant - step
      }
    }
    is(s_finish){
      io.finish := true.B
      when(io.epoch === true.B){
        sta     := s_run
        vacant  := 512.U
        pending := 0.U
        valid   := false.B
      }
    }
  }
  
  for (i <- 0 to 511+LZword){
    BuffAry(i).in(0)      := BuffAry(i).out
  }

  //前512bit，是真正写回的数据
  for(i <- 0 to 511){
    for(j <- 1 to 9){
      BuffAry(i).in(j) := BuffAry(i+j).out
    }
    BuffAry(i).shiftStep  := step
    BuffAry(i).in(10) := BuffAry(i+LZword).out
    io.out(i) := BuffAry(i).out
  }

  //后18bit，接收来自编码器的内容
  for(j <- 1 to 9){
    for(i <- 512 to 511+LZword-j){
      BuffAry(i).in(j) := BuffAry(i+j).out
    }
    for(i <- 512+LZword-j to 511+LZword){
      BuffAry(i).in(j) := 0.U
    }
  }
  for(i <- 512 to 511+LZword){
    BuffAry(i).shiftStep  := step_in
    BuffAry(i).in(10) := io.in(i-512)
  }

  io.valid  := valid 
  io.busy   := busy
  //?
  /*
  val in_window = Wire(UInt(18.W))
  val tmp_window = Wire(Vec(18,Bool()))
  for(i <- 0 to 17){
    tmp_window(i) := BuffAry(512+i).out
  }
  in_window := tmp_window.asUInt()
  when(sta === s_run && io.L_in>0.U){
    printf(p"step: ${step}\n")
    printf(p"pending: ${pending}\n")
    printf(p"vacant: ${vacant}\n")
    printf(p"window: ${in_window}\n")
    printf(p"*****************************\n")
  }*/
  //?
}

class AnsCell extends Module with LZpara {
  val io = IO(new Bundle{
    val in = Input(Vec(11,Bool()))
    val shiftStep = Input(UInt(4.W))
    val out = Output(Bool())
  })
  val data =  Reg(Bool())
  when(io.shiftStep =/= 0.U){
    data := io.in(io.shiftStep)
  }
  io.out := data
}

//* LZcmp core and it's submodule
class PriEncoder extends Module with LZpara{
  val io = IO(new Bundle() {
    val i = Input(UInt(N.W))
    val vld = Output(Bool())
    val o = Output(UInt(L_distance.W))
  })
  //val have1 = Wire(Bool())
  val tmp = DividePriEncoder(io.i)
  io.vld := tmp._1
  io.o := tmp._2
}

object DividePriEncoder {
  def apply(in: Bits)= {
    val InputWidth = 1 << log2Ceil(in.getWidth)
    val EncoderInput = Wire(UInt(InputWidth.W))
    EncoderInput := in
    RealApply(EncoderInput, InputWidth)    
  }

  def RealApply(in: UInt, width: Int):(Bool,UInt) = {
    val outputWidth = log2Ceil(width)
    val result = Wire(UInt(outputWidth.W))
    val valid = Wire(Bool())
    if(width == 2){
      val idx = in(1) && (~in(0)).asBool
      result := idx
      val have1 = in.orR()
      valid := have1
    }else{
      val subWidth = width >> 1
      val rightResult = RealApply(in(subWidth - 1, 0), subWidth)
      val leftResult = RealApply(in(width-1,subWidth),subWidth)

      valid := rightResult._1 || leftResult._1
      result := Mux(rightResult._1,Cat(0.U,rightResult._2),Cat(1.U,leftResult._2))
    }
    (valid,result)
  }
}

class Systolic extends Module{
  val io= IO(new Bundle{
    val epoch   = Input(Bool())
    val AtciIn  = Input(Bool())
    val begin   = Input(Bool())
    val xin     = Input(UInt(8.W))
    val y       = Input(UInt(8.W))
    val shift   = Input(Bool())
    val AtciOut = Output(Bool())
    val xout    = Output(UInt(8.W))
    val ismatch = Output(Bool())
  })
  val active  = Reg(Bool())
  val history = Reg(Bool())
  val x       = Reg(UInt(8.W))

  val alive   = Wire(Bool())
  val eq      = Wire(Bool())
  val ans     = Wire(Bool())

  when(io.shift){
    history := ans
  }

  when(io.epoch){
    active := 0.B
  }.elsewhen(io.shift){
    active := io.AtciIn
  }
  io.AtciOut := active

  when(io.shift){
    x := io.xin
  }
  io.xout := x

  alive := io.begin || history
  eq    := x===io.y
  ans   := active && eq && alive
  io.ismatch := ans
}

class LZcmp extends Module with LZpara{
  val io = IO(new Bundle{
    val data    = Input(UInt(8.W))
    val shift   = Input(Bool())
    val epoch   = Input(Bool())
    val begin   = Input(Bool())
    val site    = Output(UInt(L_distance.W))
    val L_match = Output(UInt((L_length+1).W))
    val eq_vld  = Output(Bool())
    val done    = Output(Bool())
  })

  val SysAry  = VecInit(Seq.fill(N)(Module(new Systolic).io))
  val Nnor   = Module(new BigNOR)
  val eq_bf   = Reg(Vec(N,Bool()))
  val encoder = Module(new PriEncoder)
  val cnt     = Reg(UInt((L_length+1).W))

  // Global signal of Systolic arrays
  for (i <- 0 to N-1){
    SysAry(i).epoch := io.epoch
    SysAry(i).begin := io.begin
    SysAry(i).shift := io.shift
    SysAry(i).y     := io.data
  
    Nnor.io.in(i)   := SysAry(i).ismatch
    when(io.shift){
      eq_bf(i)      := SysAry(i).ismatch
    }
  }
  encoder.io.i := eq_bf.asUInt()

  // Horizontal connections between Systolic arrays
  for(i <- 1 to N-1){
    SysAry(i).xin     := SysAry(i-1).xout
    SysAry(i).AtciIn  := SysAry(i-1).AtciOut
  }
  
  SysAry(0).xin   := io.data
  SysAry(0).AtciIn:= true.B
  
  
  // count logic
  when(io.begin){
    cnt := (~Nnor.io.out).asUInt()
  } .elsewhen(io.shift && !Nnor.io.out){
    cnt := cnt + 1.U
  }

  io.site   := encoder.io.o
  io.eq_vld := encoder.io.vld
  io.done   := Nnor.io.out
  when(io.begin){
    io.L_match := 0.U
  } .otherwise{
    io.L_match := cnt
  }
}

class BigNOR extends Module with LZpara{
  val io= IO(new Bundle{
    val in= Input(Vec(N,Bool()))
    val out= Output(Bool())
  })
  val tInt = Wire(UInt(N.W))
  val tOr = Wire(Bool())
  tInt := io.in.asUInt()
  tOr := tInt.orR()
  io.out := ~tOr
}
