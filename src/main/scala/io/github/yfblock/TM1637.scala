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

  def apply(port: TMPort, ds: DataStore): TM1637 = {
    val tm1637 = new TM1637()
    port <> tm1637.io.port
    tm1637.io.displays(3) := ds.getSec0
    tm1637.io.displays(2) := ds.getSec1.resized
    tm1637.io.displays(1) := ds.getMin0
    tm1637.io.displays(0) := ds.getMin1.resized
    tm1637
  }
}

object TMData {
  def apply(data: Bits, continue: Bool = False): Bits = {
    continue ## data
  }

  def display(num: UInt, dot: Bool, continue: Bool = False): Bits = {
    continue ## this.displayCore(num, dot)
  }

  def displayCore(num: UInt, dot: Bool): Bits = {
    dot ## B(
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
      )
    )
  }
}

class TM1637 extends Component {
  val io = new Bundle {
    val port     = master(TMPort().setAsReg())
    val displays = in(Vec.fill(4)(UInt(4 bits)))
  }

  noIoPrefix()
  io.port.dio.writeEnable := True
  val bitLoop = Reg(UInt(4 bits)) init (0);
  val step    = Reg(UInt(2 bits)) init (0);

  val SHOW_DISPLAY = B"8'b10001001"
  val INCRE_MODE   = B"8'b01000000"
  val WRITE_DATA   = B"8'b11000000"

  val CommandGroup = Vec(
    TMData(B"8'b10001010"),
    TMData(B"8'b01000000"),
    TMData(B"8'b11000000", True),
    TMData.display(io.displays(0).resized, dot = False, continue = True),
    TMData.display(io.displays(1).resized, dot = True, continue = True),
    TMData.display(io.displays(2).resized, dot = False, continue = True),
    TMData.display(io.displays(3).resized, dot = False)
  )

  val CommandIndex = Reg(UInt(4 bits)) init (0)

  new StateMachine {
    val START = new State with EntryPoint {
      whenIsActive {
        io.port.clk       := True
        io.port.dio.write := True
        goto(DELAY)
      }
    }

    val DELAY = new StateDelay(5) {
      whenCompleted {
        io.port.dio.write := False
        goto(COMMAND1)
      }
    }

    val COMMAND1: State = new State {
      onEntry(bitLoop := 0)
      whenIsActive {
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
          is(3)(when(bitLoop === 8)(goto(ACK)))
        }
      }
    }

    // val ACK1 = new State {
    //     whenIsActive {
    //         step := step + 1
    //         switch(step) {
    //             is(0)(io.port.clk := False)
    //             is(1) {
    //                 io.port.dio.writeEnable := False
    //                 io.port.clk := True
    //                 when(io.port.dio.read)(step := step)
    //             }
    //             is(3) {
    //                 io.port.clk  := False
    //                 CommandIndex := CommandIndex + 1
    //                 when(CommandGroup(CommandIndex.resized)(8)) {
    //                     goto(COMMAND1)
    //                 } otherwise {
    //                     goto(END)
    //                 }
    //             }
    //         }
    //     }
    // }

    val ACK = new State {
      whenIsActive {
        io.port.clk := False
        goto(ACK1)
      }
    }

    val ACK1 = new State {
      whenIsActive {
        io.port.clk := True
        io.port.dio.writeEnable := False
        when(io.port.dio.read === False) {
          goto(ACK2)
        }
      }
    }

    val ACK2 = new State {
      whenIsActive {
        CommandIndex := CommandIndex + 1
        io.port.clk  := False
        when(CommandGroup(CommandIndex.resized)(8)) {
          goto(COMMAND1)
        } otherwise {
          goto(END)
        }
      }
    }

    val END = new State {
      whenIsActive {
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
    }

    val IDLE = new State {
      whenIsActive {
        when(CommandIndex === CommandGroup.length)(CommandIndex := 2)
        goto(START)
      }
    }

    setEncoding(binaryOneHot)
  }
}
