package io.github.yfblock.Gowin

import spinal.core._

// IDIV_SEL 0~63
// FBDIV_SEL 0~63
// ODIV_SEL 2,4,8,16,32,48,64,80,96,112,128
// DYN_SDIV_SEL 2~128
class GowinSP(dataWidth: Int) extends BlackBox {
  val generic = new Generic {
    val READ_MODE        = "1'b0"    // 读模式配置 1'b0 bypass模式，1'b1 pipeline模式
    val WRITE_MODE       = "2'b00"   // 写模式配置 2'b00 normal模式, 2'b01 write-through 模式, 
                                  // 2'b10 read-before-write 模式
    val BIT_WIDTH        = dataWidth      // 数据宽度配置
    val BLK_SEL          = "3'b000"  // 块选择参数
    val RESET_MODE       = "SYNC" // 复位模式，支持 SYNC 和 ASYNC
    // val INIT_RAM_00      = ""  // INIT_RAM_00 ~ INIT_RAM_3F 用于初始化存储单元初始化数据
                                  // 数据宽度 256 per ADDR
  }
  val io = new Bundle {
    val DO       = out UInt (32 bits)// 数据输出信号
    val DI       = in UInt (32 bits) // 数据输入信号
    val AD       = in UInt (14 bits)  // 地址输入信号
    val WRE      = in Bool()          // 写使能输入信号，True: 写入, False: 读出
    val CE       = in Bool()          // 时钟使能输入信号，高电平有效
    val CLK      = in Bool()          // 时钟输入信号
    val RESET    = in Bool()          // 复位输出信号，支持同步异步，高电平有效，复位寄存器，不复位存储器得值
    val OCE      = in Bool()          // 输出信号使能信号，用于pipeline模式，对Bypass模式无效
    val BLKSEL   = in UInt(3 bits)    // BSRAM 块选择信号，需要多块级联时使用
  }
  noIoPrefix()
  setBlackBoxName("SP")

  def assignDefaults = {
    io.OCE := True;
    io.CE  := True;
    io.RESET := ~this.clockDomain.readResetWire;
    // io.RESET := False;
    io.CLK <> this.clockDomain.readClockWire;
    io.BLKSEL := 0;
    io.DI := 0;
    io.AD := 0;
  }
}
