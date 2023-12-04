package io.github.yfblock

import io.github.yfblock.Gowin.{Osc, OscClockDomain}
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.master

import scala.language.postfixOps
import spinal.lib.io.InOutWrapper

class Testspinal extends Component {
  val io = new Bundle {
    val user_button = in Bool()
    val reset_button = in Bool()
    val xtal_in = in Bool();
    // val lcd_interface = out(SpiLcdPort()).setName("lcd")
    val leds = out(UInt (6 bits))
    val tm = master(TMPort())
    val ds1302 = master(DSPort())
    val tm1638 = master(TM1638Port())
  }

  // or else .cst requires a `io_` prefix.
  noIoPrefix()

  val ds = DataStore()

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

  val tm1638_leds = UInt(8 bits)
  val tm1638_keys = UInt(8 bits)
  
  tm1638_leds := tm1638_keys

//  val oscClockDomain = OscClockDomain(100, io.reset_button);
  new ClockingArea(clk) {
    new SlowArea(500 kHz) {
      // SpiLcdST7789(io.lcd_interface)
      TM1637(io.tm, ds)
      // RealClock(tclock)
      DS1302(io.ds1302, ds)
      TM1638(io.tm1638, ds, tm1638_leds, tm1638_keys)
    }
  }
  io.leds := U"6'b111111"
  // io.leds(5 downto 1) := U("5'b11111")
}

// Run this main to generate the RTL
object Main {
  def main(args: Array[String]): Unit = {
    new java.io.File("rtl").mkdirs
    SpinalConfig(targetDirectory = "rtl", anonymSignalPrefix = "tmp").generateVerilog(InOutWrapper(new Testspinal))
  }
}

object TestSpinalSim {
  def main(args: Array[String]) {
    SimConfig.withWave.doSim(InOutWrapper(new Testspinal)) { dut =>
      val clk = ClockDomain(
        clock = dut.io.xtal_in,
        reset = dut.io.reset_button,
        frequency = FixedFrequency(27 MHz),
        config = ClockDomainConfig(
          clockEdge = RISING,
          resetKind = SYNC,
          resetActiveLevel = LOW
        )
      )
      clk.forkStimulus(period = 10)
      for (idx <- 0 to 99999) {
        clk.waitRisingEdge()
      }
    }
  }
}
