package io.github.yfblock

import spinal.core._
import spinal.lib._
import spinal.lib.io.TriState
import spinal.lib.fsm._

case class TM1638Port() extends Bundle with IMasterSlave {
  val stb = Bool()
  val clk = Bool()
  val dio = TriState(Bool())

  def asMaster(): Unit = {
    out(stb)
    out(clk)
    master(dio)
  }

  override def setAsReg(): this.type = {
    this.stb.setAsReg().init(True)
    this.clk.setAsReg().init(True)
    this.dio.write.setAsReg().init(True)
    this.dio.writeEnable.setAsReg().init(True)
    this
  }
}

object TM1638 {
  def apply(port: TM1638Port, td: TimeDisplay): TM1638 = {
    val tm1638 = new TM1638()
    port <> tm1638.io.port
    td <> tm1638.io.display
    tm1638
  }
}

class TM1638 extends Component {
  val io = new Bundle {
    val port    = master(TM1638Port()).setAsReg()
    val display = in(TimeDisplay())
  }

  val tm1637 = new Area {
    noIoPrefix()
    val bitLoop = Reg(UInt(4 bits)) init (0);
    val step    = Reg(UInt(2 bits)) init (0);

    val CommandGroup = Vec(
      TMData(B"8'b10001001"),
      TMData(B"8'b01000000"),
      TMData(B"8'b11000000", True),
      TMData.display(io.display.rt1.resized, dot = False, continue = True),
      TMData(B"8'b00000001", True),
      TMData.display(io.display.rt2.resized, dot = False, continue = True),
      TMData(B"8'b00000001", True),
      TMData.display(io.display.rt3.resized, dot = False, continue = True),
      TMData(B"8'b00000001", True),
      TMData.display(io.display.rt4.resized, dot = False, continue = True),
      TMData(B"8'b00000000")
    )

    val CommandIndex = Reg(UInt(4 bits)) init (0)

    def is_continue(index: UInt): Bool = CommandGroup(index.resized)(8)
    def dio_write(index: UInt, bitIndex: UInt): Unit =
      io.port.dio.write := CommandGroup(index.resized)(bitIndex.resized)

    new StateMachine {
      val START, GAP, COMMAND1, WAIT, END = new State
      val IDLE                            = new StateDelay(200)

      setEntry(START)

      START
        .whenIsActive {
          io.port.clk       := True
          io.port.dio.write := True
          io.port.stb       := False
          goto(GAP)
        }

      GAP.whenIsActive {
        bitLoop := 0
        goto(COMMAND1)
      }

      COMMAND1
        .whenIsActive {
          step := step + 1
          when(step(0) === False) {
            io.port.clk := step(1)
          } otherwise {
            when(step(1)) {
              bitLoop := bitLoop + 1
              when(bitLoop === 7)(goto(WAIT))
            } otherwise {
              dio_write(CommandIndex, bitLoop)
            }
          }
        }

      WAIT.whenIsActive {
        CommandIndex := CommandIndex + 1
        when(is_continue(CommandIndex)) {
          goto(GAP)
        } otherwise (goto(END))
      }

      END
        .whenIsActive {
          io.port.stb := True
          goto(IDLE)
        }

      IDLE.whenCompleted {
        when(CommandIndex === CommandGroup.length)(CommandIndex := 0)
        goto(START)
      }

      setEncoding(binaryOneHot)
    }
  }

}
