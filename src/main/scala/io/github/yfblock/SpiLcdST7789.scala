package io.github.yfblock

import spinal.core._
import spinal.lib.fsm._

import scala.language.postfixOps

case class SpiLcdPort() extends Bundle {
  val resetn = out(Bool());
  val clk    = out(Bool());
  val cs     = out(Bool());
  val rs     = out(Bool());
  val data   = out(Bool());
}

object SpiLcdST7789 {
  def apply(spiLcdPort: SpiLcdPort): Unit = {
    val spiLcdST7789 = new SpiLcdST7789()
    spiLcdST7789.io.lcd_resetn <> spiLcdPort.resetn
    spiLcdST7789.io.lcd_clk <> spiLcdPort.clk
    spiLcdST7789.io.lcd_cs <> spiLcdPort.cs
    spiLcdST7789.io.lcd_rs <> spiLcdPort.rs
    spiLcdST7789.io.lcd_data <> spiLcdPort.data
  }
}

case class SpiLcdST7789() extends Component {
  val io = new Bundle {
    val lcd_resetn = out.Bool().setAsReg().init(False);
    val lcd_clk    = out.Bool();
    val lcd_cs     = out.Bool().setAsReg().init(True);
    val lcd_rs     = out.Bool().setAsReg().init(True);
    val lcd_data   = out.Bool().setAsReg.init(False);
  }

  noIoPrefix()

  val MAX_CMDS = 70;
  val init_cmd = Vec(UInt(9 bits), MAX_CMDS);
  init_cmd(0)  := U"9'h036";
  init_cmd(1)  := U"9'h170";
  init_cmd(2)  := U"9'h03A";
  init_cmd(3)  := U"9'h105";
  init_cmd(4)  := U"9'h0B2";
  init_cmd(5)  := U"9'h10C";
  init_cmd(6)  := U"9'h10C";
  init_cmd(7)  := U"9'h100";
  init_cmd(8)  := U"9'h133";
  init_cmd(9)  := U"9'h133";
  init_cmd(10) := U"9'h0B7";
  init_cmd(11) := U"9'h135";
  init_cmd(12) := U"9'h0BB";
  init_cmd(13) := U"9'h119";
  init_cmd(14) := U"9'h0C0";
  init_cmd(15) := U"9'h12C";
  init_cmd(16) := U"9'h0C2";
  init_cmd(17) := U"9'h101";
  init_cmd(18) := U"9'h0C3";
  init_cmd(19) := U"9'h112";
  init_cmd(20) := U"9'h0C4";
  init_cmd(21) := U"9'h120";
  init_cmd(22) := U"9'h0C6";
  init_cmd(23) := U"9'h10F";
  init_cmd(24) := U"9'h0D0";
  init_cmd(25) := U"9'h1A4";
  init_cmd(26) := U"9'h1A1";
  init_cmd(27) := U"9'h0E0";
  init_cmd(28) := U"9'h1D0";
  init_cmd(29) := U"9'h104";
  init_cmd(30) := U"9'h10D";
  init_cmd(31) := U"9'h111";
  init_cmd(32) := U"9'h113";
  init_cmd(33) := U"9'h12B";
  init_cmd(34) := U"9'h13F";
  init_cmd(35) := U"9'h154";
  init_cmd(36) := U"9'h14C";
  init_cmd(37) := U"9'h118";
  init_cmd(38) := U"9'h10D";
  init_cmd(39) := U"9'h10B";
  init_cmd(40) := U"9'h11F";
  init_cmd(41) := U"9'h123";
  init_cmd(42) := U"9'h0E1";
  init_cmd(43) := U"9'h1D0";
  init_cmd(44) := U"9'h104";
  init_cmd(45) := U"9'h10C";
  init_cmd(46) := U"9'h111";
  init_cmd(47) := U"9'h113";
  init_cmd(48) := U"9'h12C";
  init_cmd(49) := U"9'h13F";
  init_cmd(50) := U"9'h144";
  init_cmd(51) := U"9'h151";
  init_cmd(52) := U"9'h12F";
  init_cmd(53) := U"9'h11F";
  init_cmd(54) := U"9'h11F";
  init_cmd(55) := U"9'h120";
  init_cmd(56) := U"9'h123";
  init_cmd(57) := U"9'h021";
  init_cmd(58) := U"9'h029";
  init_cmd(59) := U"9'h02A"; // column
  init_cmd(60) := U"9'h100";
  init_cmd(61) := U"9'h128";
  init_cmd(62) := U"9'h101";
  init_cmd(63) := U"9'h117";
  init_cmd(64) := U"9'h02B"; // row
  init_cmd(65) := U"9'h100";
  init_cmd(66) := U"9'h135";
  init_cmd(67) := U"9'h100";
  init_cmd(68) := U"9'h1BB";
  init_cmd(69) := U"9'h02C"; // start

  val ONE_MS    = this.clockDomain.frequency.getValue.toInt / 1000;
  val CNT_100MS = ONE_MS * 100;
  val CNT_120MS = ONE_MS * 120;
  val CNT_200MS = ONE_MS * 200;

  io.lcd_clk <> ~this.clockDomain.readClockWire;
  val cmd_index = Reg(UInt(7 bits)).init(0);
  val clk_cnt   = Reg(UInt(32 bits)).init(0);
  val bit_loop  = Reg(UInt(5 bits)).init(0);
  val pixel_cnt = Reg(UInt(16 bits)).init(0);

  val pixel = (pixel_cnt >= 21600) ? U"16'hF800" |
    ((pixel_cnt >= 10800) ? U"16'h07E0" | U"16'h001F");

  new StateMachine {
    val INIT_RESET   = new State with EntryPoint;
    val INIT_PREPARE = new State;
    val INIT_WAKEUP  = new State;
    val INIT_SNOOZE  = new State;
    val INIT_WORKING = new State;
    val INIT_DONE    = new State;

    INIT_RESET.whenIsActive {
      clk_cnt := clk_cnt + 1;
      when(clk_cnt === CNT_100MS) {
        clk_cnt := 0;
        goto(INIT_PREPARE);
        io.lcd_resetn := True;
      }
    }

    INIT_PREPARE.whenIsActive {
      clk_cnt := clk_cnt + 1;
      when(clk_cnt === CNT_200MS) {
        clk_cnt := 0;
        goto(INIT_WAKEUP)
      }
    }

    INIT_WAKEUP.whenIsActive {
      when(bit_loop === 0) {
        io.lcd_cs   := False;
        io.lcd_rs   := False;
        io.lcd_data := U"8'h11" (7 - bit_loop.resized)
        bit_loop    := bit_loop + 1;
      } elsewhen (bit_loop === 8) {
        io.lcd_cs := True;
        io.lcd_rs := True;
        bit_loop  := 0;
        goto(INIT_SNOOZE);
      } otherwise {
        io.lcd_data := U"8'h11" (7 - bit_loop.resized);
        bit_loop    := bit_loop + 1;
      }
    }

    INIT_SNOOZE.whenIsActive {
      clk_cnt := clk_cnt + 1;
      when(clk_cnt === CNT_120MS) {
        clk_cnt := 0;
        goto(INIT_WORKING);
      }
    }

    INIT_WORKING.whenIsActive {
      when(cmd_index === MAX_CMDS) {
        goto(INIT_DONE);
      } elsewhen (bit_loop === 0) {
        io.lcd_cs   := False;
        io.lcd_rs   := init_cmd(cmd_index)(8);
        io.lcd_data := init_cmd(cmd_index)(7 - bit_loop.resized);
        bit_loop    := bit_loop + 1;
      } elsewhen (bit_loop === 8) {
        io.lcd_cs := True;
        io.lcd_rs := True;
        bit_loop  := 0;
        cmd_index := cmd_index + 1;
      } otherwise {
        io.lcd_data := init_cmd(cmd_index)(7 - bit_loop.resized);
        bit_loop    := bit_loop + 1;
      }
    }

    INIT_DONE.whenIsActive {
      when(pixel_cnt === 32400) {} elsewhen (bit_loop === 0) {
        io.lcd_cs   := False;
        io.lcd_rs   := True;
        io.lcd_data := pixel(15 - bit_loop.resized);
        bit_loop    := bit_loop + 1;
      } elsewhen (bit_loop === 16) {
        io.lcd_cs := True;
        io.lcd_rs := True;
        bit_loop  := 0;
        pixel_cnt := pixel_cnt + 1;
      } otherwise {
        io.lcd_data := pixel(15 - bit_loop.resized);
        bit_loop    := bit_loop + 1;
      }
    }
  }
}
