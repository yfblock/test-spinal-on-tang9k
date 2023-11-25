package io.github.yfblock

import spinal.core
import spinal.core._
import spinal.lib._
import spinal.lib.IMasterSlave
import spinal.lib.fsm._
import spinal.lib.io.ReadableOpenDrain
import spinal.lib.io.TriState
import spinal.lib.com.i2c
import spinal.lib.com.i2c.I2c

import scala.language.postfixOps

// Reference:
// https://github.com/Ncerzzk/FPGA-PWM/blob/8a35c9f82d3bcdde4b86c9e1d52bd20c5fbbc9fc/src/main/scala/mylib/DutTests.scala#L18
// https://github.com/Ncerzzk/FPGA-PWM/blob/8a35c9f82d3bcdde4b86c9e1d52bd20c5fbbc9fc/src/main/scala/mylib/MyTopLevel.scala#L25

// I2C timing diagram:
// https://m5stack.oss-cn-shenzhen.aliyuncs.com/resource/docs/datasheet/unit/Unit%208Encoder/I2C%20timing%20diagram.pdf
case class TMPort() extends Bundle with IMasterSlave {
  val clk = Bool()
  val dio = Analog(Bool())

  override def asMaster(): Unit = {
    out(clk)
    inout(dio)
  }

  override def setAsReg(): TMPort.this.type = {
    this.clk.setAsReg().init(True)
    this
  }
}

// Only can change data wire when clk is LOW,
// data can't be changed when clk is HIGH
// start signal is CLK is HIGH and DIO turn LOW from HIGH
// end signal is CLK is HIGH and DIO turn HIGH from LOW.
// when send done it will receive a ACK signal
// ACK signal will turn DIO as LOW.
object TM1637 {
  def apply(port: TMPort, led: Bool): Unit = {
    val add1 = new TM1637()
    port <> add1.io.port
    led := add1.io.led
  }
}

class TM1637 extends Component {
  val io = new Bundle {
    val port = master(TMPort().setAsReg())
    val led = out(Bool()).setAsReg().init(False)
  }

  val dio = TriState(Bool()).setAsReg()
  dio.write.init(True)
  dio.writeEnable.init(True)

  dio.read := io.port.dio
  when(dio.writeEnable) {
    io.port.dio := dio.write
  }

  noIoPrefix()
  //  val i2c = master(I2c())
  val bit_loop = Reg(UInt(4 bits)) init (0);
  val STEP = Reg(UInt(3 bits)) init (0);

//  val WRITE_COMMAND = U"8'b01000100".reversed
  val WRITE_COMMAND = U(0x87 + 8, 8 bits)
  new StateMachine {
    val START, COMMAND1, ACK, END, IDLE = new State

    setEntry(START)

    START.onEntry(STEP := 0)
      .whenIsActive {
        io.port.clk := True
        dio.write := True
        STEP := STEP + 1
        when(STEP === 5) {
          dio.write := False
          goto(COMMAND1)
        }
      }

    COMMAND1.onEntry {
        STEP := 0
        bit_loop := bit_loop + 1
      }
      .whenIsActive {
        STEP := STEP + 1
        switch(STEP) {
          is(0) {
            io.port.clk := False
          }
          is(1) {
            dio.write := WRITE_COMMAND(bit_loop.resized)
            bit_loop := bit_loop + 1
          }
          is(2) {
            io.port.clk := True
          }
          is(3) {
            STEP := 0
            when(bit_loop === 8) {
              goto(ACK)
            }
          }
        }
      }.onExit(bit_loop := 0)

    ACK.onEntry(STEP := 0).whenIsActive {
      STEP := STEP + 1
      switch(STEP) {
        is(0) {
          io.port.clk := False
        }
        is(1) {
          io.port.clk := True
          when(dio.read === True) {
            STEP := STEP
          }
        }
        is(2) {
          io.port.clk := False
          goto(END)
        }
      }
    }

    END.onEntry(
      STEP := 0
    ).whenIsActive {
      STEP := STEP + 1
      switch(STEP) {
        is(0)(io.port.clk := False)
        is(1)(dio.write := False)
        is(2)(io.port.clk := True)
        is(3) {
          dio.write := True
          goto(IDLE)
        }
      }
    }

    IDLE.whenIsActive {
      io.led := True
    }

    setEncoding(SpinalEnumEncoding(id => id))
  }
}
