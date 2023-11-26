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

  val STEP     = Reg(UInt(4 bits)) init (0)
  val bit_loop = Reg(UInt(3 bits)) init (0)

  val ADDRESS = U(0x81, 8 bits)
  val DATA    = Reg(UInt(8 bits)) init (0)

  new StateMachine {
    val START, WRITE_ADDRESS, READ, END, IDLE = new State

    setEntry(START)

    START
      .onEntry(STEP := 0)
      .whenIsActive {
        io.port.rst := True
        goto(WRITE_ADDRESS)
        io.tm.rt1 := 1
      }

    WRITE_ADDRESS
      .onEntry {
        STEP     := 0
        bit_loop := 0
      }
      .whenIsActive {
        STEP := STEP + 1
        switch(STEP) {
          is(0) {
            // io.port.dat := ADDRESS(bit_loop.resized)
            dio.write := ADDRESS(bit_loop.resized)
          }
          is(1)(io.port.clk := True)
          is(2) {
            STEP        := 0
            bit_loop    := bit_loop + 1
            io.port.clk := False
            when(bit_loop === 7) {
              dio.writeEnable := False
              goto(READ)
            }
          }
        }
        io.tm.rt1 := 2
      }

    READ
      .onEntry {
        STEP     := 0
        bit_loop := 0
      }
      .whenIsActive {
        STEP := STEP + 1
        switch(STEP) {
          is(0) {
            DATA(bit_loop.resized) := dio.read
          }
          is(1)(io.port.clk := True)
          is(2) {
            STEP        := 0
            bit_loop    := bit_loop + 1
            io.port.clk := False
            when(bit_loop === 7) {
              dio.writeEnable := True
              goto(END)
            }
          }
        }
        io.tm.rt1 := 3
      }

    END
      .onEntry {
        STEP     := 0
        bit_loop := 0
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
