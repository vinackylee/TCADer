
package boom.acc

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import boom.exu.ygjk._


trait AESParameters{
    val keyNum = 32
    val statemtrow = 4
    val statemtcol =4
    val wordNum = 60
    val wordGroupNum = 4
    val byteNum = 8
/*
    //type of key and block
    val type128 = 0
    val type192 = 1
    val type256 = 2
*/
}

class Withacc_AES extends Config((site,here,up) => {  
    case BuildYGAC => 
        (p:Parameters) => {          
            val myAccel = Module(new AESModule)
            myAccel
        }
    }
)

class AESModule extends MyACCModule  with AESParameters{
//    val io = IO(Flipped(new YGJKIO))

    val keyin = RegInit(VecInit(Seq.fill(keyNum)(0.U(byteNum.W))))
    val key = RegInit(VecInit(Seq.fill(keyNum)(0.U(byteNum.W))))
    val word = RegInit(VecInit(Seq.fill(wordNum)(VecInit(Seq.fill(wordGroupNum)(3.U(byteNum.W))))))

    val in = RegInit(VecInit(Seq.fill(dataNum)(0x05050505.U(dataWidth.W))))
    val in_v = RegInit(VecInit(Seq.fill(4)(false.B)))
    val in_index = RegInit(0.U(3.W))

    val out = RegInit(VecInit(Seq.fill(dataNum)(0x05050505.U(dataWidth.W))))
    val out_v = RegInit(VecInit(Seq.fill(4)(false.B)))
    val out_index = RegInit(0.U(3.W))

    val key_type = RegInit(0.U(2.W)) // 0-128, 1-192, 2-256 

  io.sdma.read1_req.ready := false.B
  io.query.rd := 0.U

    val s_idle :: s_key_compare :: s_key_entend :: s_compute :: Nil = Enum(4)
    val sta  = RegInit(s_idle)

    val config_i = RegInit(0.U(3.W))
    when(io.config.valid){  //接收到启动指令
        when(config_i===0.U){
            key_type := io.config.bits.cfgData1
            config_i := 1.U
        }.elsewhen(config_i===1.U){
            for(i <- 0 until 8){
              keyin(i) := io.config.bits.cfgData1(63-i*8,56-i*8)
              keyin(i+8) := io.config.bits.cfgData2(63-i*8,56-i*8)
            }
            when(key_type===0.U){
              config_i := 3.U
            }.otherwise{
              config_i := 2.U
            }
        }.elsewhen(config_i===2.U){
            for(i <- 0 until 8){
              keyin(i+16) := io.config.bits.cfgData1(63-i*8,56-i*8)
              keyin(i+24) := io.config.bits.cfgData2(63-i*8,56-i*8)
            }
            config_i := 3.U
        }
    }

    val keyExtend = Module(new keySchedule)
    val sbox = Module(new Sbox)
    val aesEnc0 = Module(new aes_enc)

    aesEnc0.io.key_type := key_type

    keyExtend.io.key.bits := keyin
    keyExtend.io.key_type := key_type

    sbox.io.in0 := aesEnc0.io.subByteout
    aesEnc0.io.subBytein := sbox.io.out0

    when(sta === s_key_entend){
      sbox.io.in0(0) := keyExtend.io.subByteout
    }
    keyExtend.io.subBytein := sbox.io.out0(0)
    keyExtend.io.w.ready := true.B

    when(keyExtend.io.w.fire()){
      word := keyExtend.io.w.bits
    }

    val in0 = Wire(Vec(statemtrow,Vec(statemtcol,UInt(byteNum.W))))
    
    in0(0) := Seq(in(0.U+4.U*in_index)(7,0), in(1.U+4.U*in_index)(7,0), in(2.U+4.U*in_index)(7,0), in(3.U+4.U*in_index)(7,0))
    in0(1) := Seq(in(0.U+4.U*in_index)(15,8), in(1.U+4.U*in_index)(15,8), in(2.U+4.U*in_index)(15,8), in(3.U+4.U*in_index)(15,8))
    in0(2) := Seq(in(0.U+4.U*in_index)(23,16), in(1.U+4.U*in_index)(23,16), in(2.U+4.U*in_index)(23,16), in(3.U+4.U*in_index)(23,16))
    in0(3) := Seq(in(0.U+4.U*in_index)(31,24), in(1.U+4.U*in_index)(31,24), in(2.U+4.U*in_index)(31,24), in(3.U+4.U*in_index)(31,24))

    when(io.sdma.read0_req.fire()){
        in := io.sdma.read0_req.bits
        for(i <- 0 until 4){
            in_v(i) := true.B
        }
    }

    aesEnc0.io.in.bits := in0
    aesEnc0.io.in.valid := in_v(in_index) && sta === s_compute
    when(aesEnc0.io.in.fire()){
        in_v(in_index) := false.B
        when(in_index === 3.U){
            in_index := 0.U
        }.otherwise{
            in_index := in_index + 1.U
        }
    }

    io.sdma.read0_req.ready := !in_v.reduce(_|_) && sta === s_compute

    aesEnc0.io.w := word

    val out0 = Wire(Vec(4,UInt(dataWidth.W)))

    out0(0) := Cat(aesEnc0.io.out.bits(3)(0), aesEnc0.io.out.bits(2)(0), aesEnc0.io.out.bits(1)(0), aesEnc0.io.out.bits(0)(0))
    out0(1) := Cat(aesEnc0.io.out.bits(3)(1), aesEnc0.io.out.bits(2)(1), aesEnc0.io.out.bits(1)(1), aesEnc0.io.out.bits(0)(1))
    out0(2) := Cat(aesEnc0.io.out.bits(3)(2), aesEnc0.io.out.bits(2)(2), aesEnc0.io.out.bits(1)(2), aesEnc0.io.out.bits(0)(2))
    out0(3) := Cat(aesEnc0.io.out.bits(3)(3), aesEnc0.io.out.bits(2)(3), aesEnc0.io.out.bits(1)(3), aesEnc0.io.out.bits(0)(3))

    aesEnc0.io.out.ready := out_v(out_index) === false.B
    when(aesEnc0.io.out.fire()){
        out(0.U+4.U*out_index) := out0(0)
        out(1.U+4.U*out_index) := out0(1)
        out(2.U+4.U*out_index) := out0(2)
        out(3.U+4.U*out_index) := out0(3)
        out_v(out_index) := true.B
        when(out_index === 3.U){
            out_index := 0.U
        }.otherwise{
            out_index := out_index + 1.U
        }
    }

    io.sdma.write_req.bits := out
    io.sdma.write_req.valid := out_v.reduce(_&_)
    when(io.sdma.write_req.fire()){
        for(i <- 0 until 4){
            out_v(i) := false.B
        }
    }

    keyExtend.io.key.valid := false.B

    switch(sta){
      is(s_idle){
        when(config_i === 3.U){
//          sta := s_key_compare
            sta := s_compute
        }
      }
      is(s_key_compare){
//        printf(p"sta = key_compare\n")
        val res = RegInit(VecInit(Seq.fill(keyNum)(false.B)))
        for(i <- 0 until keyNum){
            res(i) := key(i) =/= keyin(i)
        }
        when(res.reduce(_ || _)){
          keyExtend.io.key.valid := true.B
          when(keyExtend.io.key.fire()){
            sta := s_key_entend  
          }
        }.otherwise{
          sta := s_compute
        }
      }
      is(s_key_entend){
//        printf(p"sta = key_entend\n")
        key := keyin 
        when(keyExtend.io.w.fire()){   
            sta := s_compute
        }
      }
      is(s_compute){
//        printf(p"sta = compute\n")
        when(!io.sdma.run){
          config_i := 0.U
          sta := s_idle
        }
      }
    }
}



class aes_enc extends Module with AESParameters{
    val io = IO(new Bundle{
        val w = Input(Vec(wordNum,Vec(wordGroupNum,UInt(byteNum.W))))
        val in = DeqIO(Vec(statemtrow,Vec(statemtcol,UInt(byteNum.W))))
        val out = EnqIO(Vec(statemtrow,Vec(statemtcol,UInt(byteNum.W))))
        val run = Output(Bool())
        val key_type = Input(UInt(2.W))

        val subBytein = Input(Vec(statemtrow,Vec(statemtcol,UInt(byteNum.W))))
        val subByteout = Output(Vec(statemtrow,Vec(statemtcol,UInt(byteNum.W))))
 
    })
    val addRoundKey = Module(new addRoundKey)
    val state = RegInit(VecInit(Seq.fill(statemtrow)(VecInit(Seq.fill(statemtcol)(0.U(byteNum.W))))))
    val rowshift = Module(new shiftrow)
    val mixCol_addkey = Module(new MixColumn_addRoundKey)
    val i = RegInit(1.U(4.W))
    val run = RegInit(false.B)

    val round = RegInit(0.U(4.W))


    io.run := run

    when(io.in.fire()){
      state := addRoundKey.io.statemtout
    }.elsewhen(i <= round && run){
      state := mixCol_addkey.io.out
    }.elsewhen(i === round + 1.U){
      state := addRoundKey.io.statemtout
    }
    
    when(io.in.fire()){
      when(io.key_type === 0.U){
        round := 9.U
      }.elsewhen(io.key_type === 1.U){
        round := 11.U
      }.elsewhen(io.key_type === 2.U){
        round := 13.U
      }
    }

    when(io.in.fire()){
      run := true.B
    }.elsewhen(io.out.fire()){
      run := false.B
    }
    
    when(run){
      io.in.ready := false.B
//      printf(p"state ${state}\n")
    }.otherwise{
      io.in.ready := true.B
    }
    
    when(io.out.fire()){
      i := 1.U
    }.elsewhen(run){
      i := Mux(i>=round + 2.U , round + 2.U, i + 1.U)
    }


    io.subByteout := state

    rowshift.io.in := io.subBytein

    mixCol_addkey.io.in := rowshift.io.out
    mixCol_addkey.io.win := io.w
    mixCol_addkey.io.index := i
    
    when(io.in.fire()){
      addRoundKey.io.statemtin := io.in.bits 
    }.otherwise{
      addRoundKey.io.statemtin := rowshift.io.out
    }
    when(io.in.fire()){
      addRoundKey.io.win := Seq(io.w(0),io.w(1),io.w(2),io.w(3))
    }.elsewhen(round === 9.U){
      addRoundKey.io.win := Seq(io.w(40),io.w(41),io.w(42),io.w(43))
    }.elsewhen(round === 11.U){
      addRoundKey.io.win := Seq(io.w(48),io.w(49),io.w(50),io.w(51))
    }.otherwise{
      addRoundKey.io.win := Seq(io.w(56),io.w(57),io.w(58),io.w(59))
    }
    

    io.out.bits := state


    when(i >=round + 2.U){
      io.out.valid := true.B
    }.otherwise{
      io.out.valid := false.B
    }
}

class keySchedule extends Module with AESParameters{
    val io = IO(new Bundle{
        val key = DeqIO(Vec(keyNum,UInt(byteNum.W)))
        val w = EnqIO(Vec(wordNum,Vec(wordGroupNum,UInt(byteNum.W))))

        val key_type = Input(UInt(2.W))

        val subByteout = Output(Vec(wordGroupNum,UInt(byteNum.W)))
        val subBytein = Input(Vec(wordGroupNum,UInt(byteNum.W)))
    })
    val word = RegInit(VecInit(Seq.fill(wordNum)(VecInit(Seq.fill(wordGroupNum)(0.U(byteNum.W))))))
    val run = RegInit(false.B)
    val index = RegInit(4.U(6.W))
    val index_round = RegInit(0.U(4.W))
    val w_num = RegInit(0.U(6.W))
    val tmp = Wire((Vec(wordGroupNum,UInt(byteNum.W))))

    
    io.subByteout := tmp
    val rcon = Reg(Vec(10,UInt(8.W)))
    rcon := Seq(0x01.U, 0x02.U, 0x04.U, 0x08.U, 
                0x10.U, 0x20.U, 0x40.U, 0x80.U,
                0x1b.U, 0x36.U)

    val nk = RegInit(0.U(4.W))
    val nk_w = Wire(UInt(4.W))
    nk_w := nk
    when(io.key.fire()){
      when(io.key_type === 0.U){
        nk := 4.U
        nk_w := 4.U
        w_num := 44.U
      }.elsewhen(io.key_type === 1.U){
        nk := 6.U
        nk_w := 6.U
        w_num := 52.U
      }.elsewhen(io.key_type === 2.U){
        nk := 8.U
        nk_w := 8.U
        w_num := 60.U
      }
    }
    

    when(nk > 6.U && index(2,0) === 4.U){
      tmp := Seq(word(index-1.U)(0),word(index-1.U)(1),word(index-1.U)(2),word(index-1.U)(3))
    }.otherwise{
      tmp := Seq(word(index-1.U)(1),word(index-1.U)(2),word(index-1.U)(3),word(index-1.U)(0))
    }

    io.key.ready := !run
    io.w.bits := word
    //前nk个w
    when(io.key.fire()){
        run := true.B
        for( i<- 0 until 8){
            word(i) := Seq(io.key.bits(0 + i *4),io.key.bits(1 + i *4),io.key.bits(2 + i *4),io.key.bits(3 + i *4))
        }
        index := nk_w
        index_round := 0.U
    }
    when(run && index < w_num){
      when(nk === 4.U){
        when(index(1,0) =/= 0.U){
            word(index)(0) := word(index-nk)(0) ^ word(index-1.U)(0)
            word(index)(1) := word(index-nk)(1) ^ word(index-1.U)(1)
            word(index)(2) := word(index-nk)(2) ^ word(index-1.U)(2)
            word(index)(3) := word(index-nk)(3) ^ word(index-1.U)(3)
        }.otherwise{
            word(index)(0) := io.subBytein(0) ^ rcon(index_round) ^ word(index-nk)(0)
            word(index)(1) := io.subBytein(1) ^ word(index-nk)(1)
            word(index)(2) := io.subBytein(2) ^ word(index-nk)(2)
            word(index)(3) := io.subBytein(3) ^ word(index-nk)(3)

            index_round := index_round + 1.U
        }
      }.elsewhen(nk === 6.U){
        when(index =/= 6.U && index =/= 12.U && index =/= 18.U && index =/= 24.U && index =/= 30.U && index =/= 36.U && index =/= 42.U && index =/= 48.U){
            word(index)(0) := word(index-nk)(0) ^ word(index-1.U)(0)
            word(index)(1) := word(index-nk)(1) ^ word(index-1.U)(1)
            word(index)(2) := word(index-nk)(2) ^ word(index-1.U)(2)
            word(index)(3) := word(index-nk)(3) ^ word(index-1.U)(3)
        }.otherwise{
            word(index)(0) := io.subBytein(0) ^ rcon(index_round) ^ word(index-nk)(0)
            word(index)(1) := io.subBytein(1) ^ word(index-nk)(1)
            word(index)(2) := io.subBytein(2) ^ word(index-nk)(2)
            word(index)(3) := io.subBytein(3) ^ word(index-nk)(3)

            index_round := index_round + 1.U
        }
      }.elsewhen(nk === 8.U){
        when(index(2,0) === 0.U){
            word(index)(0) := io.subBytein(0) ^ rcon(index_round) ^ word(index-nk)(0)
            word(index)(1) := io.subBytein(1) ^ word(index-nk)(1)
            word(index)(2) := io.subBytein(2) ^ word(index-nk)(2)
            word(index)(3) := io.subBytein(3) ^ word(index-nk)(3)

            index_round := index_round + 1.U
        }.elsewhen(index(2,0) === 4.U){
            word(index)(0) := io.subBytein(0) ^ word(index-nk)(0)
            word(index)(1) := io.subBytein(1) ^ word(index-nk)(1)
            word(index)(2) := io.subBytein(2) ^ word(index-nk)(2)
            word(index)(3) := io.subBytein(3) ^ word(index-nk)(3)
        }.otherwise{
            word(index)(0) := word(index-nk)(0) ^ word(index-1.U)(0)
            word(index)(1) := word(index-nk)(1) ^ word(index-1.U)(1)
            word(index)(2) := word(index-nk)(2) ^ word(index-1.U)(2)
            word(index)(3) := word(index-nk)(3) ^ word(index-1.U)(3)
        }
      }  
      index := index + 1.U
    }
    when(index === w_num){
        io.w.valid := true.B
        when(io.w.fire()){
            index := nk
            run := false.B
        }
    }.otherwise{
        io.w.valid := false.B
    }

//    printf(p"key ${io.key}\n")
//    printf(p"index $index\n")
//    printf(p"word${word}\n")
}


class shiftrow extends Module with AESParameters{
    val io = IO(new Bundle{
        val in = Input(Vec(statemtrow,Vec(statemtcol,UInt(byteNum.W))))
        val out = Output(Vec(statemtrow,Vec(statemtcol,UInt(byteNum.W))))
    })
    io.out(0) := io.in(0)
    io.out(1) := Seq(io.in(1)(1), io.in(1)(2), io.in(1)(3), io.in(1)(0))
    io.out(2) := Seq(io.in(2)(2), io.in(2)(3), io.in(2)(0), io.in(2)(1))
    io.out(3) := Seq(io.in(3)(3), io.in(3)(0), io.in(3)(1), io.in(3)(2))
}

/*
    2, 3, 1, 1,
    1, 2, 3, 1,
    1, 1, 2, 3,
    3, 1, 1, 2 
*/

class MixColumn_addRoundKey extends Module with AESParameters{
    val io = IO(new Bundle{
        val in = Input(Vec(statemtrow,Vec(statemtcol,UInt(byteNum.W))))
        val win = Input(Vec(wordNum,Vec(wordGroupNum,UInt(byteNum.W))))
        val index = Input(UInt(4.W))
        val out = Output(Vec(statemtrow,Vec(statemtcol,UInt(byteNum.W))))
    })
    for(i <- 0 until statemtcol){
      val tmp0 = Cat(io.in(0)(i)(6,0), 0.U)
      val tmp1 = Cat(io.in(1)(i)(6,0), 0.U)
      val tmp2 = Cat(io.in(2)(i)(6,0), 0.U)
      val tmp3 = Cat(io.in(3)(i)(6,0), 0.U)

      when(io.in(0)(i)(7) ^ io.in(1)(i)(7) === 1.U){
        io.out(0)(i) := 27.U ^ tmp0 ^ tmp1 ^ io.in(1)(i) ^ io.in(2)(i) ^ io.in(3)(i) ^ io.win(io.index * 4.U + i.U)(0)
      }.otherwise{
        io.out(0)(i) := tmp0 ^ tmp1 ^ io.in(1)(i) ^ io.in(2)(i) ^ io.in(3)(i) ^ io.win(io.index * 4.U + i.U)(0)
      }

      when(io.in(1)(i)(7) ^ io.in(2)(i)(7) === 1.U){
        io.out(1)(i) := 27.U ^ tmp1 ^ tmp2 ^ io.in(2)(i) ^ io.in(3)(i) ^ io.in(0)(i) ^ io.win(io.index * 4.U + i.U)(1)
      }.otherwise{
        io.out(1)(i) := tmp1 ^ tmp2 ^ io.in(2)(i) ^ io.in(3)(i) ^ io.in(0)(i) ^ io.win(io.index * 4.U + i.U)(1)
      }

      when(io.in(2)(i)(7) ^ io.in(3)(i)(7) === 1.U){
        io.out(2)(i) := 27.U ^ tmp2 ^ tmp3 ^ io.in(3)(i) ^ io.in(0)(i) ^ io.in(1)(i) ^ io.win(io.index * 4.U + i.U)(2)
      }.otherwise{
        io.out(2)(i) := tmp2 ^ tmp3 ^ io.in(3)(i) ^ io.in(0)(i) ^ io.in(1)(i) ^ io.win(io.index * 4.U + i.U)(2)
      }

      when(io.in(3)(i)(7) ^ io.in(0)(i)(7) === 1.U){
        io.out(3)(i) := 27.U ^ tmp3 ^ tmp0 ^ io.in(0)(i) ^ io.in(1)(i) ^ io.in(2)(i) ^ io.win(io.index * 4.U + i.U)(3)
      }.otherwise{
        io.out(3)(i) := tmp3 ^ tmp0 ^ io.in(0)(i) ^ io.in(1)(i) ^ io.in(2)(i) ^ io.win(io.index * 4.U + i.U)(3)
      }
    }

}

class addRoundKey extends Module with AESParameters{
    val io = IO(new Bundle{
        val statemtin = Input(Vec(statemtrow,Vec(statemtcol,UInt(byteNum.W))))
        val win = Input(Vec(4,Vec(wordGroupNum,UInt(byteNum.W))))
        val statemtout = Output(Vec(statemtrow,Vec(statemtcol,UInt(byteNum.W))))
    })
    for(i <- 0 until statemtrow){
      for(j <- 0 until statemtcol){
        io.statemtout(i)(j) := io.statemtin(i)(j) ^ io.win(j)(i)
      }
    }
}

class Sbox extends Module with AESParameters{
    val io = IO(new Bundle{
        val in0 = Input(Vec(statemtrow,Vec(statemtcol,UInt(byteNum.W))))
        val out0 = Output(Vec(statemtrow,Vec(statemtcol,UInt(byteNum.W))))

/*
        val in1 = Input(Vec(statemtrow,Vec(statemtcol,UInt(byteNum.W))))
        val out1 = Output(Vec(statemtrow,Vec(statemtcol,UInt(byteNum.W))))

        val in2 = Input(Vec(statemtrow,Vec(statemtcol,UInt(byteNum.W))))
        val out2 = Output(Vec(statemtrow,Vec(statemtcol,UInt(byteNum.W))))

        val in3 = Input(Vec(statemtrow,Vec(statemtcol,UInt(byteNum.W))))
        val out3 = Output(Vec(statemtrow,Vec(statemtcol,UInt(byteNum.W))))
*/
    })
    val num = Reg(Vec(16,Vec(16,UInt(8.W))))

    num(0) := Seq(
               0x63.U, 0x7c.U, 0x77.U, 0x7b.U, 0xf2.U, 0x6b.U, 0x6f.U, 0xc5.U, 
               0x30.U, 0x01.U, 0x67.U, 0x2b.U, 0xfe.U, 0xd7.U, 0xab.U, 0x76.U)
    num(1) := Seq(
               0xca.U, 0x82.U, 0xc9.U, 0x7d.U, 0xfa.U, 0x59.U, 0x47.U, 0xf0.U, 
               0xad.U, 0xd4.U, 0xa2.U, 0xaf.U, 0x9c.U, 0xa4.U, 0x72.U, 0xc0.U)
    num(2) := Seq(
               0xb7.U, 0xfd.U, 0x93.U, 0x26.U, 0x36.U, 0x3f.U, 0xf7.U, 0xcc.U,
               0x34.U, 0xa5.U, 0xe5.U, 0xf1.U, 0x71.U, 0xd8.U, 0x31.U, 0x15.U)
    num(3) := Seq(
               0x04.U, 0xc7.U, 0x23.U, 0xc3.U, 0x18.U, 0x96.U, 0x05.U, 0x9a.U,
               0x07.U, 0x12.U, 0x80.U, 0xe2.U, 0xeb.U, 0x27.U, 0xb2.U, 0x75.U)
    num(4) := Seq(
               0x09.U, 0x83.U, 0x2c.U, 0x1a.U, 0x1b.U, 0x6e.U, 0x5a.U, 0xa0.U,
               0x52.U, 0x3b.U, 0xd6.U, 0xb3.U, 0x29.U, 0xe3.U, 0x2f.U, 0x84.U)
    num(5) := Seq(
               0x53.U, 0xd1.U, 0x00.U, 0xed.U, 0x20.U, 0xfc.U, 0xb1.U, 0x5b.U,
               0x6a.U, 0xcb.U, 0xbe.U, 0x39.U, 0x4a.U, 0x4c.U, 0x58.U, 0xcf.U)
    num(6) := Seq(
               0xd0.U, 0xef.U, 0xaa.U, 0xfb.U, 0x43.U, 0x4d.U, 0x33.U, 0x85.U,
               0x45.U, 0xf9.U, 0x02.U, 0x7f.U, 0x50.U, 0x3c.U, 0x9f.U, 0xa8.U)
    num(7) := Seq(
               0x51.U, 0xa3.U, 0x40.U, 0x8f.U, 0x92.U, 0x9d.U, 0x38.U, 0xf5.U,
               0xbc.U, 0xb6.U, 0xda.U, 0x21.U, 0x10.U, 0xff.U, 0xf3.U, 0xd2.U)
    num(8) := Seq(
               0xcd.U, 0x0c.U, 0x13.U, 0xec.U, 0x5f.U, 0x97.U, 0x44.U, 0x17.U,
               0xc4.U, 0xa7.U, 0x7e.U, 0x3d.U, 0x64.U, 0x5d.U, 0x19.U, 0x73.U)
    num(9) := Seq(
               0x60.U, 0x81.U, 0x4f.U, 0xdc.U, 0x22.U, 0x2a.U, 0x90.U, 0x88.U,
               0x46.U, 0xee.U, 0xb8.U, 0x14.U, 0xde.U, 0x5e.U, 0x0b.U, 0xdb.U)
    num(10) := Seq(
               0xe0.U, 0x32.U, 0x3a.U, 0x0a.U, 0x49.U, 0x06.U, 0x24.U, 0x5c.U,
               0xc2.U, 0xd3.U, 0xac.U, 0x62.U, 0x91.U, 0x95.U, 0xe4.U, 0x79.U)
    num(11) := Seq(
               0xe7.U, 0xc8.U, 0x37.U, 0x6d.U, 0x8d.U, 0xd5.U, 0x4e.U, 0xa9.U,
               0x6c.U, 0x56.U, 0xf4.U, 0xea.U, 0x65.U, 0x7a.U, 0xae.U, 0x08.U)
    num(12) := Seq(
               0xba.U, 0x78.U, 0x25.U, 0x2e.U, 0x1c.U, 0xa6.U, 0xb4.U, 0xc6.U,
               0xe8.U, 0xdd.U, 0x74.U, 0x1f.U, 0x4b.U, 0xbd.U, 0x8b.U, 0x8a.U)
    num(13) := Seq(
               0x70.U, 0x3e.U, 0xb5.U, 0x66.U, 0x48.U, 0x03.U, 0xf6.U, 0x0e.U,
               0x61.U, 0x35.U, 0x57.U, 0xb9.U, 0x86.U, 0xc1.U, 0x1d.U, 0x9e.U)
    num(14) := Seq(
               0xe1.U, 0xf8.U, 0x98.U, 0x11.U, 0x69.U, 0xd9.U, 0x8e.U, 0x94.U,
               0x9b.U, 0x1e.U, 0x87.U, 0xe9.U, 0xce.U, 0x55.U, 0x28.U, 0xdf.U)
    num(15) := Seq(
               0x8c.U, 0xa1.U, 0x89.U, 0x0d.U, 0xbf.U, 0xe6.U, 0x42.U, 0x68.U,
               0x41.U, 0x99.U, 0x2d.U, 0x0f.U, 0xb0.U, 0x54.U, 0xbb.U, 0x16.U)

/*
    for(i <- 0 until wordGroupNum){
        io.subByteout(i) := num(io.subBytein(i)(7,4))(io.subBytein(i)(3,0))
    }
*/
    for(i <- 0 until statemtrow){
      for(j <- 0 until statemtcol){
        io.out0(i)(j) := num(io.in0(i)(j)(7,4))(io.in0(i)(j)(3,0))
      }
    }
/*
    for(i <- 0 until statemtrow){
      for(j <- 0 until statemtcol){
        io.out1(i)(j) := num(io.in1(i)(j)(7,4))(io.in1(i)(j)(3,0))
      }
    }

    for(i <- 0 until statemtrow){
      for(j <- 0 until statemtcol){
        io.out2(i)(j) := num(io.in2(i)(j)(7,4))(io.in2(i)(j)(3,0))
      }
    }

    for(i <- 0 until statemtrow){
      for(j <- 0 until statemtcol){
        io.out3(i)(j) := num(io.in3(i)(j)(7,4))(io.in3(i)(j)(3,0))
      }
    }
*/
}
