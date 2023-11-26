package io.github.yfblock;

import spinal.core._
import spinal.lib._
import spinal.lib.fsm.StateMachine
import spinal.lib.fsm.State
import spinal.lib.io.TriState
import spinal.lib.fsm.EntryPoint

case class DSPort() extends Bundle with IMasterSlave {
  val rst = Bool()
  val clk = Bool()
  val dat = Analog(Bool())

  override def asMaster(): Unit = {
    out(rst)
    out(clk)
    inout(dat)
  }

  override def setAsReg(): this.type = {
    this.clk.setAsReg().init(False)
    this.rst.setAsReg().init(False)
    this
  }
}

object DS1302 {
  def apply(port: DSPort, tm: TimeDisplay): DS1302 = {
    val ds1302 = new DS1302();
    ds1302.io.port <> port
    ds1302.io.tm <> tm
    ds1302
  }
}

class DS1302 extends Component {
  val io = new Bundle {
    val port = master(DSPort())
    val tm   = out(TimeDisplay()).setAsReg()
  }

  io.port.setAsReg()
  noIoPrefix()

  val dio = TriState(Bool()).setAsReg()
  dio.write.init(True)
  dio.writeEnable.init(True)

  dio.read := io.port.dat
  when(dio.writeEnable) {
    io.port.dat := dio.write
  }

  val stateCounter     = Reg(UInt(4 bits)) init (0)
  val bitIndex = Reg(UInt(3 bits)) init (0)

  val ADDRESS = U(0x81, 8 bits)
  val DATA    = Reg(UInt(8 bits)) init (0)

  new StateMachine {
    val START, WRITE_ADDRESS, READ, END, IDLE = new State

    setEntry(START)

    START
      .onEntry(stateCounter := 0)
      .whenIsActive {
        io.port.rst := True
        goto(WRITE_ADDRESS)
        io.tm.rt1 := 1
      }

    WRITE_ADDRESS
      .onEntry {
        stateCounter     := 0
        bitIndex := 0
      }
      .whenIsActive {
        stateCounter := stateCounter + 1
        switch(stateCounter) {
          is(0) {
            // io.port.dat := ADDRESS(bitIndex.resized)
            dio.write := ADDRESS(bitIndex.resized)
          }
          is(1)(io.port.clk := True)
          is(2) {
            stateCounter        := 0
            bitIndex    := bitIndex + 1
            io.port.clk := False
            when(bitIndex === 7) {
              dio.writeEnable := False
              goto(READ)
            }
          }
        }
        io.tm.rt1 := 2
      }

    READ
      .onEntry {
        stateCounter     := 0
        bitIndex := 0
      }
      .whenIsActive {
        stateCounter := stateCounter + 1
        switch(stateCounter) {
          is(0) {
            DATA(bitIndex.resized) := dio.read
          }
          is(1)(io.port.clk := True)
          is(2) {
            stateCounter        := 0
            bitIndex    := bitIndex + 1
            io.port.clk := False
            when(bitIndex === 7) {
              dio.writeEnable := True
              goto(END)
            }
          }
        }
        io.tm.rt1 := 3
      }

    END
      .onEntry {
        stateCounter     := 0
        bitIndex := 0
      }
      .whenIsActive {
        io.tm.rt4   := DATA(3 downto 0)
        io.tm.rt3   := DATA(6 downto 4).resized
        io.tm.rt2   := DATA(7).asUInt.resized
        io.port.rst := False
        io.tm.rt1   := 4
        goto(IDLE)
      }

    IDLE.whenIsActive {
    //   goto(START)
    }
  }
}
