package chipyard

import freechips.rocketchip.config.{Config}

// ---------------------
// BOOM Configs
// ---------------------
class YgjkBoomConfig extends Config(
//  new boom.acc.Withacc ++
  new boom.exu.ygjk.WithYGJKAccel ++
  new chipyard.iobinders.WithUARTAdapter ++                      // display UART with a SimUARTAdapter
  new chipyard.iobinders.WithTieOffInterrupts ++                 // tie off top-level interrupts
  new chipyard.iobinders.WithBlackBoxSimMem ++                   // drive the master AXI4 memory with a SimAXIMem
  new chipyard.iobinders.WithTiedOffDebug ++                     // tie off debug (since we are using SimSerial for testing)
  new chipyard.iobinders.WithSimSerial ++                        // drive TSI with SimSerial for testing
  new testchipip.WithTSI ++                                      // use testchipip serial offchip link
  new chipyard.config.WithNoGPIO ++                              // no top-level GPIO pins (overrides default set in sifive-blocks)
  new chipyard.config.WithBootROM ++                             // use default bootrom
  new chipyard.config.WithUART ++                                // add a UART
  new chipyard.config.WithL2TLBs(1024) ++                        // use L2 TLBs
  new chipyard.iobinders.WithSimAXIMMIO ++
  new freechips.rocketchip.subsystem.WithNoSlavePort ++          // no top-level MMIO slave port (overrides default set in rocketchip)
  new freechips.rocketchip.subsystem.WithInclusiveCache ++       // use Sifive L2 cache
  new freechips.rocketchip.subsystem.WithNExtTopInterrupts(2) ++ // no external interrupts
  new boom.common.WithMediumBooms ++                              // small boom config
  new boom.common.WithNBoomCores(1) ++                           // single-core boom
  new freechips.rocketchip.system.BaseConfig)                    // "base" rocketchip system

class AESFBoomConfig extends Config(
//  new boom.acc.Withacc ++
//  new boom.exu.ygjk.WithYGJKAccel ++
  new chipyard.iobinders.WithUARTAdapter ++                      // display UART with a SimUARTAdapter
  new chipyard.iobinders.WithTieOffInterrupts ++                 // tie off top-level interrupts
  new chipyard.iobinders.WithBlackBoxSimMem ++                   // drive the master AXI4 memory with a SimAXIMem
  new chipyard.iobinders.WithTiedOffDebug ++                     // tie off debug (since we are using SimSerial for testing)
  new chipyard.iobinders.WithSimSerial ++                        // drive TSI with SimSerial for testing
  new testchipip.WithTSI ++                                      // use testchipip serial offchip link
  new chipyard.config.WithNoGPIO ++                              // no top-level GPIO pins (overrides default set in sifive-blocks)
  new chipyard.config.WithBootROM ++                             // use default bootrom
  new chipyard.config.WithUART ++                                // add a UART
  new chipyard.config.WithL2TLBs(1024) ++                        // use L2 TLBs
  new freechips.rocketchip.subsystem.WithNoMMIOPort ++           // no top-level MMIO master port (overrides default set in rocketchip)
  new freechips.rocketchip.subsystem.WithNoSlavePort ++          // no top-level MMIO slave port (overrides default set in rocketchip)
  new freechips.rocketchip.subsystem.WithInclusiveCache ++       // use Sifive L2 cache
  new freechips.rocketchip.subsystem.WithNExtTopInterrupts(0) ++ // no external interrupts
//  new boom.common.WithSmallBooms ++                              // small boom config
  new boom.common.WithMediumBooms ++ 
  new boom.common.WithNBoomCores(1) ++                           // single-core boom
  new freechips.rocketchip.system.BaseConfig)                    // "base" rocketchip system

class MACBoomConfig extends Config(
  new boom.acc.Withacc_MAC ++
  new boom.exu.ygjk.WithYGJKAccel ++
  new chipyard.iobinders.WithUARTAdapter ++                      // display UART with a SimUARTAdapter
  new chipyard.iobinders.WithTieOffInterrupts ++                 // tie off top-level interrupts
  new chipyard.iobinders.WithBlackBoxSimMem ++                   // drive the master AXI4 memory with a SimAXIMem
  new chipyard.iobinders.WithTiedOffDebug ++                     // tie off debug (since we are using SimSerial for testing)
  new chipyard.iobinders.WithSimSerial ++                        // drive TSI with SimSerial for testing
  new testchipip.WithTSI ++                                      // use testchipip serial offchip link
  new chipyard.config.WithNoGPIO ++                              // no top-level GPIO pins (overrides default set in sifive-blocks)
  new chipyard.config.WithBootROM ++                             // use default bootrom
  new chipyard.config.WithUART ++                                // add a UART
  new chipyard.config.WithL2TLBs(1024) ++                        // use L2 TLBs
  new freechips.rocketchip.subsystem.WithNoMMIOPort ++           // no top-level MMIO master port (overrides default set in rocketchip)
  new freechips.rocketchip.subsystem.WithNoSlavePort ++          // no top-level MMIO slave port (overrides default set in rocketchip)
  new freechips.rocketchip.subsystem.WithInclusiveCache ++       // use Sifive L2 cache
  new freechips.rocketchip.subsystem.WithNExtTopInterrupts(0) ++ // no external interrupts
//  new boom.common.WithSmallBooms ++                              // small boom config
  new boom.common.WithMediumBooms++ 
  new boom.common.WithNBoomCores(1) ++                           // single-core boom
  new freechips.rocketchip.system.BaseConfig)                    // "base" rocketchip system

class LZBoomConfig extends Config(
  new boom.acc.Withacc_LZ ++
  new boom.exu.ygjk.WithYGJKAccel ++
  new chipyard.iobinders.WithUARTAdapter ++                      // display UART with a SimUARTAdapter
  new chipyard.iobinders.WithTieOffInterrupts ++                 // tie off top-level interrupts
  new chipyard.iobinders.WithBlackBoxSimMem ++                   // drive the master AXI4 memory with a SimAXIMem
  new chipyard.iobinders.WithTiedOffDebug ++                     // tie off debug (since we are using SimSerial for testing)
  new chipyard.iobinders.WithSimSerial ++                        // drive TSI with SimSerial for testing
  new testchipip.WithTSI ++                                      // use testchipip serial offchip link
  new chipyard.config.WithNoGPIO ++                              // no top-level GPIO pins (overrides default set in sifive-blocks)
  new chipyard.config.WithBootROM ++                             // use default bootrom
  new chipyard.config.WithUART ++                                // add a UART
  new chipyard.config.WithL2TLBs(1024) ++                        // use L2 TLBs
  new freechips.rocketchip.subsystem.WithNoMMIOPort ++           // no top-level MMIO master port (overrides default set in rocketchip)
  new freechips.rocketchip.subsystem.WithNoSlavePort ++          // no top-level MMIO slave port (overrides default set in rocketchip)
  new freechips.rocketchip.subsystem.WithInclusiveCache ++       // use Sifive L2 cache
  new freechips.rocketchip.subsystem.WithNExtTopInterrupts(0) ++ // no external interrupts
//  new boom.common.WithSmallBooms ++                              // small boom config
  new boom.common.WithMediumBooms++ 
  new boom.common.WithNBoomCores(1) ++                           // single-core boom
  new freechips.rocketchip.system.BaseConfig)                    // "base" rocketchip system

class AESBoomConfig extends Config(
  new boom.acc.Withacc_AES ++
  new boom.exu.ygjk.WithYGJKAccel ++
  new chipyard.iobinders.WithUARTAdapter ++                      // display UART with a SimUARTAdapter
  new chipyard.iobinders.WithTieOffInterrupts ++                 // tie off top-level interrupts
  new chipyard.iobinders.WithBlackBoxSimMem ++                   // drive the master AXI4 memory with a SimAXIMem
  new chipyard.iobinders.WithTiedOffDebug ++                     // tie off debug (since we are using SimSerial for testing)
  new chipyard.iobinders.WithSimSerial ++                        // drive TSI with SimSerial for testing
  new testchipip.WithTSI ++                                      // use testchipip serial offchip link
  new chipyard.config.WithNoGPIO ++                              // no top-level GPIO pins (overrides default set in sifive-blocks)
  new chipyard.config.WithBootROM ++                             // use default bootrom
  new chipyard.config.WithUART ++                                // add a UART
  new chipyard.config.WithL2TLBs(1024) ++                        // use L2 TLBs
  new freechips.rocketchip.subsystem.WithNoMMIOPort ++           // no top-level MMIO master port (overrides default set in rocketchip)
  new freechips.rocketchip.subsystem.WithNoSlavePort ++          // no top-level MMIO slave port (overrides default set in rocketchip)
  new freechips.rocketchip.subsystem.WithInclusiveCache ++       // use Sifive L2 cache
  new freechips.rocketchip.subsystem.WithNExtTopInterrupts(0) ++ // no external interrupts
//  new boom.common.WithSmallBooms ++                              // small boom config
  new boom.common.WithMediumBooms++ 
  new boom.common.WithNBoomCores(1) ++                           // single-core boom
  new freechips.rocketchip.system.BaseConfig)                    // "base" rocketchip system

class MEMCPYBoomConfig extends Config(
  new boom.acc.Withacc_MEMCPY ++
  new boom.exu.ygjk.WithYGJKAccel ++
  new chipyard.iobinders.WithUARTAdapter ++                      // display UART with a SimUARTAdapter
  new chipyard.iobinders.WithTieOffInterrupts ++                 // tie off top-level interrupts
  new chipyard.iobinders.WithBlackBoxSimMem ++                   // drive the master AXI4 memory with a SimAXIMem
  new chipyard.iobinders.WithTiedOffDebug ++                     // tie off debug (since we are using SimSerial for testing)
  new chipyard.iobinders.WithSimSerial ++                        // drive TSI with SimSerial for testing
  new testchipip.WithTSI ++                                      // use testchipip serial offchip link
  new chipyard.config.WithNoGPIO ++                              // no top-level GPIO pins (overrides default set in sifive-blocks)
  new chipyard.config.WithBootROM ++                             // use default bootrom
  new chipyard.config.WithUART ++                                // add a UART
  new chipyard.config.WithL2TLBs(1024) ++                        // use L2 TLBs
  new freechips.rocketchip.subsystem.WithNoMMIOPort ++           // no top-level MMIO master port (overrides default set in rocketchip)
  new freechips.rocketchip.subsystem.WithNoSlavePort ++          // no top-level MMIO slave port (overrides default set in rocketchip)
  new freechips.rocketchip.subsystem.WithInclusiveCache ++       // use Sifive L2 cache
  new freechips.rocketchip.subsystem.WithNExtTopInterrupts(0) ++ // no external interrupts
//  new boom.common.WithSmallBooms ++                              // small boom config
  new boom.common.WithMediumBooms++ 
  new boom.common.WithNBoomCores(1) ++                           // single-core boom
  new freechips.rocketchip.system.BaseConfig)                    // "base" rocketchip system
/*
class ACCBoomConfig extends Config(
  new boom.acc.Withacc ++
//  new fuxi.Withacc ++
//  new boom.acc.Withaccisp++
  new boom.exu.ygjk.WithYGJKAccel ++
  new chipyard.iobinders.WithUARTAdapter ++                      // display UART with a SimUARTAdapter
  new chipyard.iobinders.WithTieOffInterrupts ++                 // tie off top-level interrupts
  new chipyard.iobinders.WithBlackBoxSimMem ++                   // drive the master AXI4 memory with a SimAXIMem
  new chipyard.iobinders.WithTiedOffDebug ++                     // tie off debug (since we are using SimSerial for testing)
  new chipyard.iobinders.WithSimSerial ++                        // drive TSI with SimSerial for testing
  new testchipip.WithTSI ++                                      // use testchipip serial offchip link
  new chipyard.config.WithNoGPIO ++                              // no top-level GPIO pins (overrides default set in sifive-blocks)
  new chipyard.config.WithBootROM ++                             // use default bootrom
  new chipyard.config.WithUART ++                                // add a UART
  new chipyard.config.WithL2TLBs(1024) ++                        // use L2 TLBs
  new freechips.rocketchip.subsystem.WithNoMMIOPort ++           // no top-level MMIO master port (overrides default set in rocketchip)
  new freechips.rocketchip.subsystem.WithNoSlavePort ++          // no top-level MMIO slave port (overrides default set in rocketchip)
  new freechips.rocketchip.subsystem.WithInclusiveCache ++       // use Sifive L2 cache
  new freechips.rocketchip.subsystem.WithNExtTopInterrupts(0) ++ // no external interrupts
//  new boom.common.WithSmallBooms ++                              // small boom config
  new boom.common.WithMediumBooms++ 
  new boom.common.WithNBoomCores(1) ++                           // single-core boom
  new freechips.rocketchip.system.BaseConfig)                    // "base" rocketchip system
*/
class AESBoomConfig1 extends Config(
//  new boom.acc.Withacc ++
//  new fuxi.Withacc ++
//  new boom.acc.Withaccisp++
  new boom.exu.ygjk.WithYGJKAccel ++
  new chipyard.iobinders.WithUARTAdapter ++                      // display UART with a SimUARTAdapter
  new chipyard.iobinders.WithTieOffInterrupts ++                 // tie off top-level interrupts
  new chipyard.iobinders.WithBlackBoxSimMem ++                   // drive the master AXI4 memory with a SimAXIMem
  new chipyard.iobinders.WithTiedOffDebug ++                     // tie off debug (since we are using SimSerial for testing)
  new chipyard.iobinders.WithSimSerial ++                        // drive TSI with SimSerial for testing
  new testchipip.WithTSI ++                                      // use testchipip serial offchip link
  new chipyard.config.WithNoGPIO ++                              // no top-level GPIO pins (overrides default set in sifive-blocks)
  new chipyard.config.WithBootROM ++                             // use default bootrom
  new chipyard.config.WithUART ++                                // add a UART
  new chipyard.config.WithL2TLBs(1024) ++                        // use L2 TLBs
  new freechips.rocketchip.subsystem.WithNoMMIOPort ++           // no top-level MMIO master port (overrides default set in rocketchip)
  new freechips.rocketchip.subsystem.WithNoSlavePort ++          // no top-level MMIO slave port (overrides default set in rocketchip)
  new freechips.rocketchip.subsystem.WithInclusiveCache ++       // use Sifive L2 cache
  new freechips.rocketchip.subsystem.WithNExtTopInterrupts(0) ++ // no external interrupts
//  new boom.common.WithSmallBooms ++                              // small boom config
  new boom.common.WithMediumBooms_copy ++ 
  new boom.common.WithNBoomCores(1) ++                           // single-core boom
  new freechips.rocketchip.system.BaseConfig)                    // "base" rocketchip system

class RoccBoomConfig extends Config(
  new chipyard.iobinders.WithUARTAdapter ++                      // display UART with a SimUARTAdapter
  new chipyard.iobinders.WithTieOffInterrupts ++                 // tie off top-level interrupts
  new chipyard.iobinders.WithBlackBoxSimMem ++                   // drive the master AXI4 memory with a SimAXIMem
  new chipyard.iobinders.WithTiedOffDebug ++                     // tie off debug (since we are using SimSerial for testing)
  new chipyard.iobinders.WithSimSerial ++                        // drive TSI with SimSerial for testing
  new testchipip.WithTSI ++                                      // use testchipip serial offchip link
  new chipyard.config.WithNoGPIO ++                              // no top-level GPIO pins (overrides default set in sifive-blocks)
  new chipyard.config.WithBootROM ++                             // use default bootrom
  new chipyard.config.WithUART ++                                // add a UART
  new chipyard.config.WithL2TLBs(1024) ++                        // use L2 TLBs
  new freechips.rocketchip.subsystem.WithNoMMIOPort ++           // no top-level MMIO master port (overrides default set in rocketchip)
  new freechips.rocketchip.subsystem.WithNoSlavePort ++          // no top-level MMIO slave port (overrides default set in rocketchip)
  new freechips.rocketchip.subsystem.WithInclusiveCache ++       // use Sifive L2 cache
  new freechips.rocketchip.subsystem.WithNExtTopInterrupts(0) ++ // no external interrupts
//  new boom.exu.WithRoccAccel ++
  new boom.common.WithSmallBooms ++                              // small boom config
  new boom.common.WithNBoomCores(1) ++                           // single-core boom
  new freechips.rocketchip.system.BaseConfig)                    // "base" rocketchip system

class SmallBoomConfig extends Config(
  new chipyard.iobinders.WithUARTAdapter ++                      // display UART with a SimUARTAdapter
  new chipyard.iobinders.WithTieOffInterrupts ++                 // tie off top-level interrupts
  new chipyard.iobinders.WithBlackBoxSimMem ++                   // drive the master AXI4 memory with a SimAXIMem
  new chipyard.iobinders.WithTiedOffDebug ++                     // tie off debug (since we are using SimSerial for testing)
  new chipyard.iobinders.WithSimSerial ++                        // drive TSI with SimSerial for testing
  new testchipip.WithTSI ++                                      // use testchipip serial offchip link
  new chipyard.config.WithNoGPIO ++                              // no top-level GPIO pins (overrides default set in sifive-blocks)
  new chipyard.config.WithBootROM ++                             // use default bootrom
  new chipyard.config.WithUART ++                                // add a UART
  new chipyard.config.WithL2TLBs(1024) ++                        // use L2 TLBs
  new freechips.rocketchip.subsystem.WithNoMMIOPort ++           // no top-level MMIO master port (overrides default set in rocketchip)
  new freechips.rocketchip.subsystem.WithNoSlavePort ++          // no top-level MMIO slave port (overrides default set in rocketchip)
  new freechips.rocketchip.subsystem.WithInclusiveCache ++       // use Sifive L2 cache
  new freechips.rocketchip.subsystem.WithNExtTopInterrupts(0) ++ // no external interrupts
  new boom.common.WithSmallBooms ++                              // small boom config
  new boom.common.WithNBoomCores(1) ++                           // single-core boom
  new freechips.rocketchip.system.BaseConfig)                    // "base" rocketchip system

class MediumBoomConfig extends Config(
  new chipyard.iobinders.WithUARTAdapter ++
  new chipyard.iobinders.WithTieOffInterrupts ++
  new chipyard.iobinders.WithBlackBoxSimMem ++
  new chipyard.iobinders.WithTiedOffDebug ++
  new chipyard.iobinders.WithSimSerial ++
  new testchipip.WithTSI ++
  new chipyard.config.WithNoGPIO ++
  new chipyard.config.WithBootROM ++
  new chipyard.config.WithUART ++
  new chipyard.config.WithL2TLBs(1024) ++
  new freechips.rocketchip.subsystem.WithNoMMIOPort ++
  new freechips.rocketchip.subsystem.WithNoSlavePort ++
  new freechips.rocketchip.subsystem.WithInclusiveCache ++
  new freechips.rocketchip.subsystem.WithNExtTopInterrupts(0) ++
  new boom.common.WithMediumBooms ++                              // medium boom config
  new boom.common.WithNBoomCores(1) ++
  new freechips.rocketchip.system.BaseConfig)

class LargeBoomConfig extends Config(
  new chipyard.iobinders.WithUARTAdapter ++
  new chipyard.iobinders.WithTieOffInterrupts ++
  new chipyard.iobinders.WithBlackBoxSimMem ++
  new chipyard.iobinders.WithTiedOffDebug ++
  new chipyard.iobinders.WithSimSerial ++
  new testchipip.WithTSI ++
  new chipyard.config.WithNoGPIO ++
  new chipyard.config.WithBootROM ++
  new chipyard.config.WithUART ++
  new chipyard.config.WithL2TLBs(1024) ++
  new freechips.rocketchip.subsystem.WithNoMMIOPort ++
  new freechips.rocketchip.subsystem.WithNoSlavePort ++
  new freechips.rocketchip.subsystem.WithInclusiveCache ++
  new freechips.rocketchip.subsystem.WithNExtTopInterrupts(0) ++
  new boom.common.WithLargeBooms ++                              // large boom config
  new boom.common.WithNBoomCores(1) ++
  new freechips.rocketchip.system.BaseConfig)

class MegaBoomConfig extends Config(
  new chipyard.iobinders.WithUARTAdapter ++
  new chipyard.iobinders.WithTieOffInterrupts ++
  new chipyard.iobinders.WithBlackBoxSimMem ++
  new chipyard.iobinders.WithTiedOffDebug ++
  new chipyard.iobinders.WithSimSerial ++
  new testchipip.WithTSI ++
  new chipyard.config.WithNoGPIO ++
  new chipyard.config.WithBootROM ++
  new chipyard.config.WithUART ++
  new chipyard.config.WithL2TLBs(1024) ++
  new freechips.rocketchip.subsystem.WithNoMMIOPort ++
  new freechips.rocketchip.subsystem.WithNoSlavePort ++
  new freechips.rocketchip.subsystem.WithInclusiveCache ++
  new freechips.rocketchip.subsystem.WithNExtTopInterrupts(0) ++
  new boom.common.WithMegaBooms ++                              // mega boom config
  new boom.common.WithNBoomCores(1) ++
  new freechips.rocketchip.system.BaseConfig)

class DualSmallBoomConfig extends Config(
  new chipyard.iobinders.WithUARTAdapter ++
  new chipyard.iobinders.WithTieOffInterrupts ++
  new chipyard.iobinders.WithBlackBoxSimMem ++
  new chipyard.iobinders.WithTiedOffDebug ++
  new chipyard.iobinders.WithSimSerial ++
  new testchipip.WithTSI ++
  new chipyard.config.WithNoGPIO ++
  new chipyard.config.WithBootROM ++
  new chipyard.config.WithUART ++
  new chipyard.config.WithL2TLBs(1024) ++
  new freechips.rocketchip.subsystem.WithNoMMIOPort ++
  new freechips.rocketchip.subsystem.WithNoSlavePort ++
  new freechips.rocketchip.subsystem.WithInclusiveCache ++
  new freechips.rocketchip.subsystem.WithNExtTopInterrupts(0) ++
  new boom.common.WithSmallBooms ++
  new boom.common.WithNBoomCores(2) ++                         // 2 boom cores
  new freechips.rocketchip.system.BaseConfig)

class SmallRV32BoomConfig extends Config(
  new chipyard.iobinders.WithUARTAdapter ++
  new chipyard.iobinders.WithTieOffInterrupts ++
  new chipyard.iobinders.WithBlackBoxSimMem ++
  new chipyard.iobinders.WithTiedOffDebug ++
  new chipyard.iobinders.WithSimSerial ++
  new testchipip.WithTSI ++
  new chipyard.config.WithNoGPIO ++
  new chipyard.config.WithBootROM ++
  new chipyard.config.WithUART ++
  new chipyard.config.WithL2TLBs(1024) ++
  new freechips.rocketchip.subsystem.WithNoMMIOPort ++
  new freechips.rocketchip.subsystem.WithNoSlavePort ++
  new freechips.rocketchip.subsystem.WithInclusiveCache ++
  new freechips.rocketchip.subsystem.WithNExtTopInterrupts(0) ++
  new boom.common.WithoutBoomFPU ++                        // no fp
  new boom.common.WithBoomRV32 ++                          // rv32 (32bit)
  new boom.common.WithSmallBooms ++
  new boom.common.WithNBoomCores(1) ++
  new freechips.rocketchip.system.BaseConfig)

class HwachaLargeBoomConfig extends Config(
  new chipyard.iobinders.WithUARTAdapter ++
  new chipyard.iobinders.WithTieOffInterrupts ++
  new chipyard.iobinders.WithBlackBoxSimMem ++
  new chipyard.iobinders.WithTiedOffDebug ++
  new chipyard.iobinders.WithSimSerial ++
  new testchipip.WithTSI ++
  new chipyard.config.WithNoGPIO ++
  new chipyard.config.WithBootROM ++
  new chipyard.config.WithUART ++
  new chipyard.config.WithL2TLBs(1024) ++
  new hwacha.DefaultHwachaConfig ++                         // use Hwacha vector accelerator
  new freechips.rocketchip.subsystem.WithNoMMIOPort ++
  new freechips.rocketchip.subsystem.WithNoSlavePort ++
  new freechips.rocketchip.subsystem.WithInclusiveCache ++
  new freechips.rocketchip.subsystem.WithNExtTopInterrupts(0) ++
  new boom.common.WithLargeBooms ++
  new boom.common.WithNBoomCores(1) ++
  new freechips.rocketchip.system.BaseConfig)

class LoopbackNICLargeBoomConfig extends Config(
  new chipyard.iobinders.WithUARTAdapter ++
  new chipyard.iobinders.WithTieOffInterrupts ++
  new chipyard.iobinders.WithBlackBoxSimMem ++
  new chipyard.iobinders.WithTiedOffDebug ++
  new chipyard.iobinders.WithSimSerial ++
  new chipyard.iobinders.WithLoopbackNIC ++                        // drive NIC IOs with loopback
  new testchipip.WithTSI ++
  new icenet.WithIceNIC ++
  new chipyard.config.WithNoGPIO ++
  new chipyard.config.WithBootROM ++
  new chipyard.config.WithUART ++
  new chipyard.config.WithL2TLBs(1024) ++
  new freechips.rocketchip.subsystem.WithNoMMIOPort ++
  new freechips.rocketchip.subsystem.WithNoSlavePort ++
  new freechips.rocketchip.subsystem.WithInclusiveCache ++
  new freechips.rocketchip.subsystem.WithNExtTopInterrupts(0) ++
  new boom.common.WithLargeBooms ++
  new boom.common.WithNBoomCores(1) ++
  new freechips.rocketchip.system.BaseConfig)

