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
  val dio = TriState(Bool())

  override def asMaster(): Unit = {
    out(clk)
    master(dio)
  }

  override def setAsReg(): TMPort.this.type = {
    this.clk.setAsReg().init(True)
    this.dio.write.setAsReg().init(True)
    this.dio.writeEnable.setAsReg().init(True)
    this
  }
}

case class TimeDisplay() extends Bundle {
  val rt1, rt2, rt3, rt4 = UInt(4 bits)

  override def setAsReg(): TimeDisplay.this.type = {
    this.rt1.setAsReg().init(0)
    this.rt2.setAsReg().init(0)
    this.rt3.setAsReg().init(0)
    this.rt4.setAsReg().init(0)
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

  def apply(port: TMPort, tm: TimeDisplay): TM1637 = {
    val add1 = new TM1637()
    port <> add1.io.port
    add1.io.display <> tm
    add1
  }
}

object TMData {
  def apply(data: Bits, continue: Bool = False): Bits = {
    continue ## data
  }

  def display(num: UInt, dot: Bool, continue: Bool = False): Bits = {
    continue ## dot ## B(
      num.mux(
        0  -> B(0x3f, 7 bits),
        1  -> B(0x06, 7 bits),
        2  -> B(0x5b, 7 bits),
        3  -> B(0x4f, 7 bits),
        4  -> B(0x66, 7 bits),
        5  -> B(0x6d, 7 bits),
        6  -> B(0x7d, 7 bits),
        7  -> B(0x07, 7 bits),
        8  -> B(0x7f, 7 bits),
        9  -> B(0x6f, 7 bits),
        10 -> B(0x77, 7 bits),
        11 -> B(0x7c, 7 bits),
        12 -> B(0x39, 7 bits),
        13 -> B(0x5e, 7 bits),
        14 -> B(0x79, 7 bits),
        15 -> B(0x71, 7 bits)
        // default -> B(0x00, 7 bits)
      )
    )
  }
}

class TM1637 extends Component {
  val io = new Bundle {
    val port = master(TMPort().setAsReg())
    // val restart = in(Reg(Bool)).init(False)
    val display = in(TimeDisplay())
  }

  val tm1637 = new Area {
    noIoPrefix()
    val bitLoop = Reg(UInt(4 bits)) init (0);
    val step    = Reg(UInt(2 bits)) init (0);

    val CommandGroup = Vec(
      TMData(B"8'b10001010"),
      TMData(B"8'b01000000"),
      TMData(B"8'b11000000", True),
      TMData.display(io.display.rt1.resized, dot = False, continue = True),
      TMData.display(io.display.rt2.resized, dot = True, continue = True),
      TMData.display(io.display.rt3.resized, dot = False, continue = True),
      TMData.display(io.display.rt4.resized, dot = False)
    )

    val CommandIndex = Reg(UInt(4 bits)) init (0)
    new StateMachine {
      val START, COMMAND1, ACK, ACK1, ACK2, END, IDLE = new State
      val DELAY                           = new StateDelay(5)

      setEntry(START)

      START
        .whenIsActive {
          io.port.clk       := True
          io.port.dio.write := True
          goto(DELAY)
        }

      DELAY.whenCompleted {
        io.port.dio.write := False
        goto(COMMAND1)
      }

      COMMAND1
        .onEntry(bitLoop := 0)
        .whenIsActive {
          step := step + 1
          switch(step) {
            is(0)(io.port.clk := False)
            is(1) {
              io.port.dio.write := CommandGroup(CommandIndex.resized)(
                bitLoop.resized
              )
              bitLoop := bitLoop + 1
            }
            is(2)(io.port.clk := True)
            is(3)(when(bitLoop === 8) (goto(ACK)))
          }
        }
        .onExit(bitLoop := 0)

      ACK.whenIsActive {
        io.port.clk := False
        goto(ACK1)
      }

      ACK1.whenIsActive {
        io.port.clk := True
        when(io.port.dio.read === False) {
          goto(ACK2)
        }
      }

      ACK2.whenIsActive {
        CommandIndex := CommandIndex + 1
        io.port.clk  := False
        when(CommandGroup(CommandIndex.resized)(8)) {
          goto(COMMAND1)
        } otherwise {
          goto(END)
        }
      }

      END
        .whenIsActive {
          step := step + 1
          switch(step) {
            is(0)(io.port.clk       := False)
            is(1)(io.port.dio.write := False)
            is(2)(io.port.clk       := True)
            is(3) {
              io.port.dio.write := True
              goto(IDLE)
            }
          }
        }

      IDLE
        .whenIsActive {
          when(CommandIndex === CommandGroup.length) {
            CommandIndex := 0
          }
          goto(START)
        }

      setEncoding(binaryOneHot)
    }
  }
}
