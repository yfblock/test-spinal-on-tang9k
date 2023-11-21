package io.github.yfblock

import io.github.yfblock.Gowin.{Osc, OscClockDomain}
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.master

import scala.language.postfixOps

class Testspinal extends Component {
  val io = new Bundle {
    val user_button = in Bool()
    val reset_button = in Bool()
    val xtal_in = in Bool();
    val lcd_interface = out(SpiLcdPort()).setName("lcd")
    val leds = out(UInt (6 bits))
//    val tm_clk = out Bool()
//    val tm_dio = inout Bool()
    val tm = master(TMPort())
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

  val oscClockDomain = OscClockDomain(100, io.reset_button);
  oscClockDomain(SpiLcdST7789(io.lcd_interface))
  oscClockDomain(TM1637(io.tm, io.leds(0)))
  io.leds(5 downto 1) := U"5'b11111"
//  val carea = new ClockingArea(clk) {
//    SpiLcdST7789(io.lcd_interface)
//  }
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
      for (idx <- 0 to 999) {
        dut.clockDomain.waitRisingEdge()
      }
    }
  }
}
