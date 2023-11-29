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
  val dat = TriState(Bool())

  override def asMaster(): Unit = {
    out(rst)
    out(clk)
    master(dat)
  }

  override def setAsReg(): this.type = {
    this.clk.setAsReg().init(False)
    this.rst.setAsReg().init(False)
    this.dat.write.setAsReg().init(True)
    this.dat.writeEnable.setAsReg().init(True)
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

  val ds1302 = new Area {
    val stateCounter = Reg(UInt(4 bits)) init (0)
    val bitIndex     = Reg(UInt(3 bits)) init (0)

    val ADDRESS  = U(0x81, 8 bits)
    val MADDRESS = U(0x83, 8 bits)
    val DATA     = Reg(UInt(8 bits)) init (0)

    val state = new StateMachine {
      val START, WRITE_ADDRESS, READ, END, MSTART, MADDR, MREAD, MEND, IDLE =
        new State

      setEntry(START)

      START
        .onEntry(stateCounter := 0)
        .whenIsActive {
          io.port.clk := False
          io.port.rst := True
          goto(WRITE_ADDRESS)
        }

      WRITE_ADDRESS
        .onEntry {
          stateCounter := 0
          bitIndex     := 0
        }
        .whenIsActive {
          stateCounter := stateCounter + 1
          switch(stateCounter) {
            is(0) {
              io.port.dat.write := ADDRESS(bitIndex.resized)
            }
            is(1)(io.port.clk := True)
            is(3) {
              stateCounter := 0
              bitIndex     := bitIndex + 1
              io.port.clk  := False
              when(bitIndex === 7) {
                io.port.dat.writeEnable := False
                goto(READ)
              }
            }
          }
        }

      READ
        .onEntry {
          stateCounter := 0
          bitIndex     := 0
        }
        .whenIsActive {
          stateCounter := stateCounter + 1
          switch(stateCounter) {
            /// TIPS: between clk low and read there has a small interval
            is(1)(DATA(bitIndex.resized) := io.port.dat.read)
            is(2)(io.port.clk            := True)
            is(4) {
              io.port.clk  := False
              stateCounter := 0
              bitIndex     := bitIndex + 1
              when(bitIndex === 7) {
                io.port.dat.writeEnable := True
                goto(END)
              }
            }
          }
        }

      END
        .onEntry {
          stateCounter := 0
          bitIndex     := 0
        }
        .whenIsActive {
          stateCounter := stateCounter + 1
          switch(stateCounter) {
            is(0) {
              io.tm.rt4 := DATA(3 downto 0)
              io.tm.rt3 := DATA(6 downto 4).resized
              io.port.rst := False
            }
            is(1)(io.port.dat.write := False)
            is(2)(io.port.dat.write := True)
            is(4)(goto(MSTART))
          }
        }

      MSTART
        .onEntry(stateCounter := 0)
        .whenIsActive {
          io.port.clk := False
          io.port.rst := True
          goto(MADDR)
        }

      MADDR
        .onEntry {
          stateCounter := 0
          bitIndex     := 0
        }
        .whenIsActive {
          stateCounter := stateCounter + 1
          switch(stateCounter) {
            is(0) {
              io.port.dat.write := MADDRESS(bitIndex.resized)
            }
            is(1)(io.port.clk := True)
            is(3) {
              stateCounter := 0
              bitIndex     := bitIndex + 1
              io.port.clk  := False
              when(bitIndex === 7) {
                io.port.dat.writeEnable := False
                goto(MREAD)
              }
            }
          }
        }

      MREAD
        .onEntry {
          stateCounter := 0
          bitIndex     := 0
        }
        .whenIsActive {
          stateCounter := stateCounter + 1
          switch(stateCounter) {
            /// TIPS: between clk low and read there has a small interval
            is(1)(DATA(bitIndex.resized) := io.port.dat.read)
            is(2)(io.port.clk            := True)
            is(4) {
              stateCounter := 0
              io.port.clk  := False
              bitIndex     := bitIndex + 1
              when(bitIndex === 7) {
                io.port.dat.writeEnable := True
                goto(MEND)
              }
            }
          }
        }

      MEND
        .onEntry {
          stateCounter := 0
          bitIndex     := 0
        }
        .whenIsActive {
          stateCounter := stateCounter + 1
          switch(stateCounter) {
            is(0) {
              io.tm.rt2 := DATA(3 downto 0)
              io.tm.rt1 := DATA(6 downto 4).resized
              // io.tm.rt1   := 4
              io.port.rst := False
            }
            is(1)(io.port.dat.write := False)
            is(2)(io.port.dat.write := True)
            is(4)(goto(IDLE))
          }
        }

      IDLE
        .onEntry(stateCounter := 0)
        .whenIsActive {
          goto(START)
        }

      setEncoding(binaryOneHot)
    }

  }
}
