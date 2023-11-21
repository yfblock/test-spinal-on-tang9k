package io.github.yfblock

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
}

// Only can change data wire when clk is LOW,
// data can't be changed when clk is HIGH
// start signal is CLK is HIGH and DIO turn LOW from HIGH
// end signal is CLK is HIGH and DIO turn HIGH from LOW.
// when send done it will receive a ACK signal
// ACK signal will turn DIO as LOW.
object TM1637 {
  def apply(clk: Bool, dio: Bool): Unit = {
    val add1 = new TM1637()
    add1.io.port.clk <> clk
    add1.io.port.dio <> dio
  }

  def apply(port: TMPort): Unit = {
    val add1 = new TM1637()
    add1.io.port <> port
  }

  def apply(port: TMPort, led: Bool): Unit = {
    val add1 = new TM1637()
    add1.io.port <> port
    add1.io.led <> led
  }
}

object I2CWriteData {
  def apply(STEP: UInt, CLK: Bool, DIO: Bool, VALUE: Bool, NEXT: Unit): Unit = {
    STEP := STEP + 1
    when(STEP===0) {
      CLK := False
    } elsewhen(STEP === 1) {
      DIO := VALUE
    } elsewhen(STEP === 2) {
      CLK := True
    } elsewhen(STEP === 3) {
      NEXT
    }
  }
}

class TM1637 extends Component {
  val io = new Bundle {
    val port = master(TMPort())
    val led = out Bool()
  }

//  val i2c = master(I2c())

  val clk = Reg(Bool()) init(True)
  io.port.clk <> clk
  io.led := True

  val data     = Reg(UInt(8 bits)) init (0);
  val bit_loop = Reg(UInt(4 bits)) init (0);
  val STEP     = Reg(UInt(3 bits)) init (0);

  val WRITE_COMMAND = U"8'b01000100"

  new StateMachine {
    val INIT, START, COMMAND1, WAIT_ACK, END, IDLE = new State

    setEntry(INIT)

    INIT.whenIsActive {
      clk := True
      io.port.dio := True
      goto(START)
    }

    START.onEntry(STEP := 0)
    .whenIsActive {
      clk := True
      io.port.dio := False
      goto(COMMAND1)
    }

    COMMAND1.onEntry(STEP := 0)
    .whenIsActive {
      STEP := STEP + 1
      when(STEP === 0) {
        clk := False
//        io.port.clk := False
      } elsewhen (STEP === 1) {
//        io.port.dio := WRITE_COMMAND(bit_loop.resized)
        io.port.clk := False
        io.port.dio := True
      } elsewhen (STEP === 2) {
        io.port.clk := True
        io.port.dio := True
      } elsewhen (STEP === 3) {
        io.port.clk := True
        io.port.dio := True
        bit_loop := bit_loop + 1
        STEP := 0
        when(bit_loop === 7) {
          goto(WAIT_ACK)
        }
      }
    }.onExit(bit_loop := 0)

    WAIT_ACK.whenIsActive {
      clk := ~clk
      when(clk === False && io.port.dio === False) {
        goto(IDLE)
      }
    }

    END.whenIsActive {
      io.port.dio := True
    }

    IDLE.whenIsActive {
      io.led := False
    }

    setEncoding(SpinalEnumEncoding(id => id))
  }
}

