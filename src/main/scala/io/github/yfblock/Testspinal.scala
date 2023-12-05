package io.github.yfblock

import io.github.yfblock.Gowin.{Osc, OscClockDomain}
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.master

import scala.language.postfixOps
import spinal.lib.io.InOutWrapper
import spinal.lib.fsm._

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
    val srf05 = master(SRF05Port())
    val uart_tx = out Bool()
    val uart_rx = out Bool()
  }

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

  val tx_en = Bool()
  val tx_data = Bits(8 bits)
  val is_busy = Bool()
//  val oscClockDomain = OscClockDomain(100, io.reset_button);
  val tm1638Displays = Vec.fill(8)(UInt(4 bits))
  val tm1637Displays = Vec.fill(4)(UInt(4 bits))

  for(i <- 0 until 4) {
    tm1637Displays(i) := 0
    tm1638Displays(i * 2) := 0
    tm1638Displays(i * 2 + 1) := 0
  }

  new ClockingArea(clk) {
    new StateMachine {
      val START = new State {
        whenIsActive {
          tm1638Displays(0) := ds.getYear1.resized
          tm1638Displays(1) := ds.getYear0
          tm1638Displays(2) := ds.getMonth1.resized
          tm1638Displays(3) := ds.getMonth0
          tm1638Displays(4) := ds.getDay1.resized
          tm1638Displays(5) := ds.getDay0
          tm1638Displays(6) := ds.getHour1.resized
          tm1638Displays(7) := ds.getHour0

          tm1637Displays(3) := ds.getSec0
          tm1637Displays(2) := ds.getSec1.resized
          tm1637Displays(1) := ds.getMin0
          tm1637Displays(0) := ds.getMin1.resized

          when(tm1638_keys(0)) {
            goto(SUPER_SONIC)
          }
        }
      }

      val SUPER_SONIC = new State with EntryPoint {
        whenIsActive {
          tm1637Displays(3) := ds.getSec0
          tm1637Displays(2) := ds.getSec1.resized
          tm1637Displays(1) := ds.getMin0
          tm1637Displays(0) := ds.getMin1.resized

          // for(i <- 0 until 4) {
          //   tm1638Displays(4 + i) := distance((16 - i * 4 - 1) downto (16 - (i + 1) * 4))
          // }
        }
      }
    }
    // Uart Transmite
    UartTx(io.uart_tx, tx_data, tx_en, is_busy)
    val datas = Vec("Hello\r\n".map(c => B(c.toInt, 8 bits)))
    val index = Reg(UInt(log2Up(datas.length) bits)).init(0)

    tx_en := index < datas.length
    when(~is_busy && index =/= datas.length)(index := index + 1)
    tx_data := datas(index)

    new StateMachine {
      val START: State = new State with EntryPoint {
        whenIsActive {
          when(tm1638_keys(3)) {
            index := 0
            goto(END)
          }
        }
      }

      val END = new State {
        whenIsActive {
          when(~tm1638_keys(3)) {
            goto(START)
          }
        }
      }
    }
    
    // val distance = UInt(16 bits)
    // SRF05(io.srf05, distance)

    new SlowArea(500 kHz) {
      // SpiLcdST7789(io.lcd_interface)
      TM1637(io.tm, tm1637Displays)
      // RealClock(tclock)
      DS1302(io.ds1302, ds)
      TM1638(io.tm1638, tm1638Displays, tm1638_leds, tm1638_keys)
    }
  }
  
  io.leds := U"6'b111111"
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
