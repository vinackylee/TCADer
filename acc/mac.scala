
package boom.acc

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import boom.exu.ygjk._

class Withacc_MAC extends Config((site,here,up) => {  
    case BuildYGAC => 
        (p:Parameters) => {          
            val myAccel = Module(new MACModule)
            myAccel
        }
    }
)

class MACModule extends MyACCModule{

	val sum = RegInit(0.U(32.W))
	val run = RegInit(false.B)
	val mat_res = RegInit(VecInit(Seq.fill(dataNum)(0.U(dataWidth.W))))

	val in0 = RegInit(VecInit(Seq.fill(dataNum)(0x05050505.U(dataWidth.W))))
	val in0_v = RegInit(VecInit(Seq.fill(dataNum)(false.B)))

	val in1 = RegInit(VecInit(Seq.fill(dataNum)(0x05050505.U(dataWidth.W))))
	val in1_v = RegInit(VecInit(Seq.fill(dataNum)(false.B)))

	when(io.config.valid){
		sum := 0.U
		run := true.B
	}.elsewhen(!io.sdma.run){
		run := false.B
	}

	io.sdma.read0_req.ready := !in0_v.reduce(_|_)
	io.sdma.read1_req.ready := !in1_v.reduce(_|_)

	when(io.sdma.read0_req.fire()){
		in0 := io.sdma.read0_req.bits
		for(i <- 0 until dataNum){
			in0_v(i) := true.B
		}
	}

	when(io.sdma.read1_req.fire()){
		in1 := io.sdma.read1_req.bits
		for(i <- 0 until dataNum){
			in0_v(i) := true.B
		}
	}

	val index = RegInit(0.U(4.W))

	when(run === true.B){
		when(in0_v(index)){
			for(i <- 0 until 2){
				mat_res(i.U+index) := mat_res(i.U+index) + in0(i.U+index) * in1(i.U+index)
				in0_v(i.U+index) := false.B
				in1_v(i.U+index) := false.B
			}
			when(index === dataNum.U - 2.U){
				index := 0.U
			}.otherwise{
				index := index + 2.U
			}
		}
	}

	io.sdma.write_req.valid := false.B
	io.sdma.write_req.bits := io.sdma.read0_req.bits

	sum := mat_res.reduce(_ + _)
	io.query.rd := sum
}
