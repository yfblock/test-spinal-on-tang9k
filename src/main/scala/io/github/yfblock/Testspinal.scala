package io.github.yfblock

import io.github.yfblock.Gowin.{Osc, OscClockDomain}
import spinal.core._
import spinal.sim._
import spinal.core.sim._
import spinal.lib.master

import scala.language.postfixOps
import spinal.lib.io.InOutWrapper
import spinal.lib.fsm._
import spinal.lib
import spinal.lib.com.uart.Uart
import spinal.lib.com.spi.SpiMaster

case class UartData(s: String) extends Area {
  val datas = Vec(s.map(c => B(c.toInt, 8 bits)))
  val index = Reg(UInt(log2Up(datas.length) bits)).init(0)

  def next(): Bool = {
    index := index + 1
    index =/= datas.length - 1
  }

  def reset() = index := 0
  def current: Bits = datas(index)
}

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
    // val srf05 = master(SRF05Port())
    val uart = master(Uart())
    val spi = master(SpiMaster())
  }

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

  //  val oscClockDomain = OscClockDomain(100, io.reset_button);

  new ClockingArea(clk) {

    val spiData = Bits(8 bits)

    val ds = DataStore()
    val tm1638_leds = UInt(8 bits)
    val tm1638_keys = UInt(8 bits)
    
    tm1638_leds := tm1638_keys

    val tx_en = Bool()
    val tx_data = Bits(8 bits)
    val is_busy = Bool()
    val tm1638Displays = Vec.fill(8)(UInt(4 bits))
    val tm1637Displays = Vec.fill(4)(UInt(4 bits))

    // give all tm displays with 0
    tm1637Displays.map(a => a:=0)
    tm1638Displays.map(a => a:=0)

    // Uart Transmite
    val rxStream = lib.Stream(Bits(8 bits))
    val txStream = lib.Stream(Bits(8 bits))
    val uartData = UartData("Hello\r\n")
    UartController(io.uart, rxStream, txStream)
    txStream.setIdle()

    val asciiReg = Reg(Bits(8 bits)) init (0)
    new StateMachine {
      val START = new State with EntryPoint {
        whenIsActive {
          VecUtil.initWith(tm1638Displays, Array(
            ds.getYear1.resized,
            ds.getYear0,
            ds.getMonth1.resized,
            ds.getMonth0,
            ds.getDay1.resized,
            ds.getDay0,
            ds.getHour1.resized,
            ds.getHour0
          ))

          VecUtil.initWith(tm1637Displays, Array(
            ds.getSec0,
            ds.getSec1.resized,
            ds.getMin0,
            ds.getMin1.resized
          ).reverse)

          when(tm1638_keys(0)) {
            goto(SUPER_SONIC)
          }
        }
      }

      val SUPER_SONIC = new State {
        whenIsActive {
          VecUtil.initWith(tm1637Displays, Array(
            ds.getSec0,
            ds.getSec1.resized,
            ds.getMin0,
            ds.getMin1.resized
          ).reverse)
          tm1638Displays(7) := asciiReg(3 downto 0).asUInt
          tm1638Displays(6) := asciiReg(7 downto 4).asUInt
          tm1638Displays(5) := spiData(3 downto 0).asUInt
          tm1638Displays(4) := spiData(7 downto 4).asUInt
          when(rxStream.valid) {
            asciiReg := rxStream.payload
          }
        }
      }
    }
    
    new StateMachine {
      val START: State = new State with EntryPoint {
        whenIsActive {
          when(tm1638_keys(3)) {
            goto(END)
          }
        }
      }

      val END = new State {
        onEntry(uartData.reset())
        whenIsActive {
          txStream.valid := True
          txStream.payload := uartData.current
          when(txStream.ready) {
            when(~uartData.next()) {
              goto(COLLECT)
            }
          }
        }
      }

      val COLLECT = new State {
        whenIsActive {
          when(~tm1638_keys(3)) {
            goto(START)
          }
        }
      }
    }
    
    // val distance = UInt(16 bits)
    // SRF05(io.srf05, distance)
    P25Q32(io.spi, spiData)
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
    SpinalConfig(targetDirectory = "fpga_project/src", anonymSignalPrefix = "tmp").generateVerilog(InOutWrapper(new Testspinal))
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
