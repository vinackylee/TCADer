//******************************************************************************
// Copyright (c) 2018 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

package chipyard.tests

import org.scalatest._

import chisel3._
import chisel3.util._
import chisel3.iotesters._

import freechips.rocketchip.config.{Parameters, Config}
import freechips.rocketchip.system._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._



import boom.common.BoomTilesKey
import boom._
//import boom.system._

/**
 * Factory object to help create a set of BOOM parameters to use in tests
 */
object BoomTestUtils {

  private def augment(tp: TileParams)(implicit p: Parameters): Parameters = p.alterPartial {
 
    case TileKey => tp
    case TileVisibilityNodeKey => TLEphemeralNode()(ValName("tile_master"))
//      println("ZZ")
    // TODO: Figure out proper TL parameters
/*    case SharedMemoryTLEdge => new TLEdgeOut(TLClientPortParameters(Seq(TLClientParameters(
                                                                          name = "fake-client-node",
                                                                          sourceId = IdRange(0,2)))),
                                             TLManagerPortParameters(Seq(TLManagerParameters(
                                                                           address = Seq(
                                                                             AddressSet(x"8000_0000",
                                                                                        x"1000_0000" - 1)),
                                                                           supportsGet = TransferSizes(1, 64),
                                                                           supportsPutFull = TransferSizes(1, 64),
                                                                           supportsPutPartial = TransferSizes(1, 64))),
                                                                     8),
                                             Parameters.empty,
                                             null)
*/
    case LookupByHartId => println("W")
    lookupByHartId(Seq(tp))
  }

  private def lookupByHartId(tps: Seq[TileParams]) = {
    // return a new lookup hart
    new LookupByHartIdImpl {
      def apply[T <: Data](f: TileParams => Option[T], hartId: UInt): T =
        PriorityMux(tps.collect { case t if f(t).isDefined => (t.hartId.U === hartId) -> f(t).get })
    }
  }

  def getBoomParameters(configName: String, configPackage: String = "chipyard"): Parameters = {
    // get the full path to the config
    val fullConfigName = configPackage + "." + configName

    // get the default unmodified params
    val origParams: Parameters = try {
      (Class.forName(fullConfigName).newInstance.asInstanceOf[Config] ++ Parameters.empty)
    }
    catch {
      case e: java.lang.ClassNotFoundException =>
        throw new Exception(s"""Unable to find config "$fullConfigName".""", e)
    }

    // get the tile parameters
 
    val boomTileParams = origParams(BoomTilesKey) // this is a seq
    // augment the parameters
    val outParams = augment(boomTileParams(0))(origParams)
    outParams
  }
}
/*
list(BoomTileParams(BoomCoreParams(8,3,96,List(IssueParams(3,1,24,2), IssueParams(3,2,24,1), 
IssueParams(3,1,24,4)),24,24,100,96,12,24,true,false,true,false,false,true,true,true,false,false,false,FtqParameters(32)
,BoomBTBParameters(true,false,512,2,4,16,20,13,true,true,8),BimParameters(1024,2,128,4,4),None,Some(GShareParameters(true,23,4096)),
None,None,2,3,2,4,8,1,8,true,0,Some(FPUParams(64,true,4,4)),true,true,false,Some(0),true,false,Some(MulDivParams(1,1,false,true,1))
,0,512,0,true,true,true,true,true,false,false,false,false),Some(ICacheParams(64,8,128,32,0,None,None,None,false,64,2,16)),Some(DCac
heParams(64,8,128,16,None,None,1,4,17,16,1,64,false,false,false,false,None)),Some(BTBParams(28,14,6,6,Some(BHTParams(512,1,8,3)),fa
lse)),0,false,Some(boom_tile),0,None,None,false,None))
*/