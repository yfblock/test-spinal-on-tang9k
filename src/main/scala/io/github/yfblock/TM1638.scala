package io.github.yfblock

import spinal.core._
import spinal.lib._
import spinal.lib.io.TriState
import spinal.lib.fsm._
import org.jline.console.impl.Builtins.Command

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
  def apply(port: TM1638Port, ds: DataStore): TM1638 = {
    val tm1638 = new TM1638()
    port <> tm1638.io.port
    tm1638.io.display(0) := ds.getYear1.resized
    tm1638.io.display(1) := ds.getYear0
    tm1638.io.display(2) := ds.getMonth1.resized
    tm1638.io.display(3) := ds.getMonth0
    tm1638.io.display(4) := ds.getDay1.resized
    tm1638.io.display(5) := ds.getDay0
    tm1638.io.display(6) := ds.getHour1.resized
    tm1638.io.display(7) := ds.getHour0
    tm1638
  }
}

class TM1638 extends Component {
  val io = new Bundle {
    val port    = master(TM1638Port()).setAsReg()
    // val display = in(TimeDisplay())
    val display = in(Vec.fill(8)(UInt(4 bits)))
  }

  val tm1637 = new Area {
    noIoPrefix()
    val bitLoop = Reg(UInt(4 bits)) init (0);
    val step    = Reg(UInt(2 bits)) init (0);

    val CommandGroup = Vec(
      TMData(B"8'b10001001"),
      TMData(B"8'b01000000"),
      TMData(B"8'b11000000", True),
      TMData.display(io.display(0).resized, False, True),
      TMData(B"8'b00000001", True),
      TMData.display(io.display(1).resized, False, True),
      TMData(B"8'b00000001", True),
      TMData.display(io.display(2).resized, False, True),
      TMData(B"8'b00000001", True),
      TMData.display(io.display(3).resized, False, True),
      TMData(B"8'b00000001", True),
      TMData.display(io.display(4).resized, False, True),
      TMData(B"8'b00000001", True),
      TMData.display(io.display(5).resized, False, True),
      TMData(B"8'b00000001", True),
      TMData.display(io.display(6).resized, False, True),
      TMData(B"8'b00000001", True),
      TMData.display(io.display(7).resized, False, True),
      TMData(B"8'b00000000")
    )

    val CommandIndex = Reg(UInt(log2Up(CommandGroup.length) bits)) init (0)

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
        when(CommandIndex === CommandGroup.length)(CommandIndex := 2)
        goto(START)
      }

      setEncoding(binaryOneHot)
    }
  }

}
