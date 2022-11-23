
package boom.acc

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import boom.exu.ygjk._


class Withacc_MEMCPY extends Config((site,here,up) => {  
    case BuildYGAC => 
        (p:Parameters) => {          
            val myAccel = Module(new MEMCPYModule)
            myAccel
        }
    }
)

class MEMCPYModule extends MyACCModule{
 
  io.sdma.read0_req.ready := io.sdma.write_req.ready
  io.sdma.write_req.valid := io.sdma.read0_req.valid
  io.sdma.write_req.bits := io.sdma.read0_req.bits

  io.sdma.read1_req.ready := false.B
  io.query.rd := 0.U
}

