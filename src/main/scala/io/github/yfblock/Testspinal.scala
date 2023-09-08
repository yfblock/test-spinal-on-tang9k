package io.github.yfblock

import spinal.core._
import spinal.sim._
import spinal.core.sim._

import scala.language.postfixOps
import spinal.lib.fsm.StateMachine
import spinal.lib.fsm.EntryPoint
import spinal.lib.fsm.State
import spinal.core.sim.SpinalSimConfig
import spinal.lib.io.TriState

class Testspinal extends Component {
  val io = new Bundle {
    val user_button = in Bool ()
    val reset_button = in Bool ()
    val leds = out UInt (6 bits)

    val xtal_in = in Bool();
    // val lcd_resetn = out Bool();
    // val lcd_clk = out Bool();
    // val lcd_cs = out Bool();
    // val lcd_rs = out Bool();
    // val lcd_data = out Bool();

    val tm_clk = out Bool();
    val tm_dio = inout(Analog(Bool()));
    // val tm_dio = TriState(Bool());
  }
  // or else .cst requires a `io_` prefix.
  noIoPrefix()

  val clk = ClockDomain(
    clock = io.xtal_in,
    reset = io.reset_button,
    frequency = FixedFrequency(27 MHz),
    config = ClockDomainConfig(
      clockEdge = RISING, 
      resetKind = SYNC, 
      resetActiveLevel = LOW
    )
  )

  // val osc_clk = new Bool()
  // val osc = new Osc(100)    // 2.5 MHz
  // osc_clk <> osc.io.OSCOUT

  // val oscClockDomain = ClockDomain(
  //   clock = osc_clk,
  //   reset = io.reset_button,
  //   config = ClockDomainConfig(
  //     clockEdge = RISING, 
  //     resetKind = SYNC, 
  //     resetActiveLevel = LOW
  //   )
  // )

  val carea = new ClockingArea(clk) {
    val slow_clk = new SlowArea(250) {
      val tm_display = new TM1637();
      tm_display.io.clk <> io.tm_clk;
      tm_display.io.dio <> io.tm_dio;
      tm_display.io.test_led <> io.leds(0);
      tm_display.io.tip_led <> io.leds(1);
      // io.leds(0) := io.tm_dio;
      io.leds(5 downto 2) := U"4'b1111";
    }
  }
}

// Run this main to generate the RTL
object Main {
  def main(args: Array[String]): Unit = {
    new java.io.File("rtl").mkdirs
    SpinalConfig(targetDirectory = "rtl").generateVerilog(new Testspinal)
  }
}

object TestSpinalSim {
  def main(args: Array[String]) {
    SimConfig.withWave.doSim(new TM1637) { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      // dut.io.xtal_in #= true
      // // 等待三个时钟周期，不知道是否有更好的写法
      // for(idx <- 0 to 3){
      //   dut.clockDomain.waitRisingEdge()
      // }
      // dut.io.xtal_in #= false
      // for(idx <- 0 to 99){
      //   //Wait a rising edge on the clock
      //   dut.clockDomain.waitRisingEdge()
      // }
      for(idx <- 0 to 999) {
        dut.clockDomain.waitRisingEdge()
      }
    }
  }
}
